// enforce_continuation.cpp - Deterministic exit enforcement
// Fires on: Stop (every response)
// Target latency: <5ms (vs Python 50ms)
//
// Implements dual-condition exit gate from Ralph's approach:
// Exit allowed ONLY if: completion_indicators >= 2 AND exit_signal == true

#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <ctime>
#include <cstdlib>
#include <sys/stat.h>
#include <sqlite3.h>

const int COMPLETION_THRESHOLD = 2;
const std::string ENFORCE_DIR = std::string(std::getenv("HOME")) + "/.claude/enforcement";
const std::string STATE_FILE = ENFORCE_DIR + "/exit_state.json";

// Minimal JSON handling
class JSON {
public:
    static std::string extractString(const std::string& json, const std::string& key) {
        std::string search = "\"" + key + "\":";
        size_t pos = json.find(search);
        if (pos == std::string::npos) return "";

        pos = json.find("\"", pos + search.length());
        if (pos == std::string::npos) return "";

        size_t end = json.find("\"", pos + 1);
        if (end == std::string::npos) return "";

        return json.substr(pos + 1, end - pos - 1);
    }

    static bool extractBool(const std::string& json, const std::string& key) {
        std::string search = "\"" + key + "\":";
        size_t pos = json.find(search);
        if (pos == std::string::npos) return false;

        size_t val_start = json.find_first_not_of(" \t\n", pos + search.length());
        return json.substr(val_start, 4) == "true";
    }

    static int extractInt(const std::string& json, const std::string& key) {
        std::string search = "\"" + key + "\":";
        size_t pos = json.find(search);
        if (pos == std::string::npos) return 0;

        size_t val_start = json.find_first_not_of(" \t\n", pos + search.length());
        return std::stoi(json.substr(val_start));
    }
};

// State persistence
struct ExitState {
    std::vector<int> completion_indicators;
    std::string session_id;

    void load() {
        std::ifstream file(STATE_FILE);
        if (!file.is_open()) return;

        std::ostringstream buffer;
        buffer << file.rdbuf();
        std::string json = buffer.str();

        // Parse completion_indicators array
        size_t arr_start = json.find("\"completion_indicators\":[");
        if (arr_start != std::string::npos) {
            size_t arr_end = json.find("]", arr_start);
            std::string arr = json.substr(arr_start + 25, arr_end - arr_start - 25);

            // Parse integers from array
            std::istringstream stream(arr);
            std::string num;
            while (std::getline(stream, num, ',')) {
                try {
                    completion_indicators.push_back(std::stoi(num));
                } catch (...) {}
            }
        }

        session_id = JSON::extractString(json, "session_id");
    }

    void save() {
        // Create directory if needed
        system(("mkdir -p " + ENFORCE_DIR).c_str());

        std::ofstream file(STATE_FILE);
        if (!file.is_open()) {
            std::cerr << "[enforce] Failed to write state file" << std::endl;
            return;
        }

        // Keep only last 10 indicators
        if (completion_indicators.size() > 10) {
            completion_indicators.erase(
                completion_indicators.begin(),
                completion_indicators.begin() + completion_indicators.size() - 10
            );
        }

        file << "{\n";
        file << "  \"completion_indicators\": [";
        for (size_t i = 0; i < completion_indicators.size(); ++i) {
            if (i > 0) file << ", ";
            file << completion_indicators[i];
        }
        file << "],\n";
        file << "  \"session_id\": \"" << session_id << "\"\n";
        file << "}\n";
    }
};

// Get current loop number from checkpoint DB
int get_loop_number(const std::string& session_id) {
    std::string checkpoint_db = std::string(std::getenv("HOME")) + "/.claude/context-checkpoints.db";

    struct stat buffer;
    if (stat(checkpoint_db.c_str(), &buffer) != 0) {
        return 1;
    }

    sqlite3* db;
    if (sqlite3_open(checkpoint_db.c_str(), &db) != SQLITE_OK) {
        return 1;
    }

    const char* sql = "SELECT COUNT(*) FROM checkpoints WHERE session_id = ?";
    sqlite3_stmt* stmt;
    if (sqlite3_prepare_v2(db, sql, -1, &stmt, nullptr) != SQLITE_OK) {
        sqlite3_close(db);
        return 1;
    }

    sqlite3_bind_text(stmt, 1, session_id.c_str(), -1, SQLITE_STATIC);

    int count = 1;
    if (sqlite3_step(stmt) == SQLITE_ROW) {
        count = sqlite3_column_int(stmt, 0);
    }

    sqlite3_finalize(stmt);
    sqlite3_close(db);

    return count > 0 ? count : 1;
}

// Extract EXIT_SIGNAL from response (5-tier extraction)
bool extract_exit_signal(const std::string& output) {
    // Tier 1: Direct exit_signal field
    if (JSON::extractBool(output, "exit_signal")) {
        return true;
    }

    // Tier 2: RALPH_STATUS block
    if (output.find("---RALPH_STATUS---") != std::string::npos) {
        size_t start = output.find("---RALPH_STATUS---");
        size_t end = output.find("---", start + 18);
        if (end != std::string::npos) {
            std::string block = output.substr(start, end - start);
            if (block.find("EXIT_SIGNAL: true") != std::string::npos ||
                block.find("EXIT_SIGNAL:true") != std::string::npos) {
                return true;
            }
        }
    }

    // Tier 3: CONTINUATION_STATUS block
    if (output.find("---CONTINUATION_STATUS---") != std::string::npos) {
        size_t start = output.find("---CONTINUATION_STATUS---");
        size_t end = output.find("---", start + 25);
        if (end != std::string::npos) {
            std::string block = output.substr(start, end - start);
            if (block.find("EXIT_SIGNAL: true") != std::string::npos ||
                block.find("EXIT_SIGNAL:true") != std::string::npos) {
                return true;
            }
        }
    }

    // Tier 4: Completion keywords
    const std::vector<std::string> completion_keywords = {
        "all tasks complete",
        "nothing left to do",
        "fully implemented",
        "no more work",
        "project complete"
    };

    std::string lower_output = output;
    for (char& c : lower_output) {
        c = std::tolower(c);
    }

    for (const auto& keyword : completion_keywords) {
        if (lower_output.find(keyword) != std::string::npos) {
            return true;
        }
    }

    return false;
}

int main() {
    // Read hook input from stdin
    std::ostringstream buffer;
    buffer << std::cin.rdbuf();
    std::string input = buffer.str();

    // Extract session info
    std::string session_id = JSON::extractString(input, "session_id");
    bool stop_hook_active = JSON::extractBool(input, "stop_hook_active");

    // Prevent infinite loop: if stop_hook_active, always allow
    if (stop_hook_active) {
        std::exit(0);
    }

    // Load state
    ExitState state;
    state.load();

    // Check for new session (reset state)
    if (state.session_id != session_id) {
        state.session_id = session_id;
        state.completion_indicators.clear();
    }

    // Get current loop number
    int loop_number = get_loop_number(session_id);

    // Extract EXIT_SIGNAL from Claude's response
    // (In real usage, this would come from tool output or response)
    // For now, we check the input for testing
    bool exit_signal = extract_exit_signal(input);

    // Update state if EXIT_SIGNAL detected
    if (exit_signal) {
        state.completion_indicators.push_back(loop_number);
        std::cerr << "[enforce] EXIT_SIGNAL detected at loop " << loop_number << std::endl;
    }

    // Dual-condition gate: check if exit allowed
    int recent_indicators = 0;
    for (int indicator : state.completion_indicators) {
        if (indicator >= loop_number - 10) {
            recent_indicators++;
        }
    }

    bool should_exit = (recent_indicators >= COMPLETION_THRESHOLD) && exit_signal;

    // Save state
    state.save();

    // Log decision
    std::cerr << "[enforce] Loop " << loop_number << ": "
              << "indicators=" << recent_indicators << "/" << COMPLETION_THRESHOLD
              << ", exit_signal=" << (exit_signal ? "true" : "false")
              << ", gate=" << (should_exit ? "SATISFIED" : "BLOCKED")
              << std::endl;

    if (!should_exit && exit_signal) {
        std::cerr << "[enforce] ⚠️  CONTINUATION ENFORCED - Exit blocked "
                  << "(need " << COMPLETION_THRESHOLD << " completion indicators)"
                  << std::endl;

        // Block by exiting with code 2
        std::cerr << "CONTINUATION REQUIRED: Not enough completion indicators "
                  << "(" << recent_indicators << "/" << COMPLETION_THRESHOLD << ")"
                  << std::endl;
        std::exit(2);
    }

    // Allow exit - output visible confirmation
    std::cout << R"({"systemMessage": "✓ Continuation policy OK"})" << std::endl;
    std::exit(0);
}
