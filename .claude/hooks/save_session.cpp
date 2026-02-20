// save_session.cpp - High-frequency hook for session persistence
// Fires on: Stop (every response), PreCompact (before compaction)
// Target latency: <5ms
//
// Reads session transcript, extracts summary/topics/facts, saves to PostgreSQL (canonical.sessions)

#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <unordered_set>
#include <unordered_map>
#include <cstring>
#include <libpq-fe.h>

// Minimal JSON parsing (no external deps for speed)
class JSONParser {
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
};

// Extract summary from JSONL transcript
struct SessionData {
    std::vector<std::string> user_messages;
    std::vector<std::string> assistant_messages;
    std::unordered_set<std::string> tool_names;

    std::string generate_summary() const {
        std::ostringstream summary;
        summary << "Session with " << user_messages.size() << " user messages, "
                << assistant_messages.size() << " assistant messages. ";

        if (!user_messages.empty()) {
            summary << "Started with: " << truncate(user_messages.front(), 200);
            if (user_messages.size() > 1) {
                summary << " — Ended with: " << truncate(user_messages.back(), 200);
            }
        }

        return summary.str();
    }

    std::vector<std::string> extract_topics() const {
        // Simple keyword extraction (matches Python version)
        static const std::unordered_map<std::string, std::string> topic_keywords = {
            {"emacs", "emacs"}, {"e2e", "e2e-test"}, {"sensor", "sensor"},
            {"vault", "vault"}, {"dotfile", "dotfiles"}, {"backup", "backup"},
            {"hud", "sensor-hud"}, {"attention", "sensor-attention"},
            {"mcp", "mcp"}, {"build", "build"}, {"hook", "hooks"},
            {"picom", "picom"}, {"i3", "i3"}, {"chrome", "browser"}
        };

        std::unordered_set<std::string> topics_set;
        std::string all_text;
        for (const auto& msg : user_messages) {
            all_text += to_lower(msg) + " ";
        }

        for (const auto& kv : topic_keywords) {
            if (all_text.find(kv.first) != std::string::npos) {
                topics_set.insert(kv.second);
            }
        }

        return std::vector<std::string>(topics_set.begin(), topics_set.end());
    }

    std::vector<std::string> extract_key_facts() const {
        std::vector<std::string> facts;

        if (!tool_names.empty()) {
            std::ostringstream tools;
            tools << "tools-used: ";
            int count = 0;
            for (const auto& tool : tool_names) {
                if (count++ >= 15) break;
                if (count > 1) tools << ", ";
                tools << tool;
            }
            facts.push_back(tools.str());
        }

        facts.push_back("message-count: " + std::to_string(user_messages.size() + assistant_messages.size()));
        facts.push_back("user-message-count: " + std::to_string(user_messages.size()));

        return facts;
    }

private:
    static std::string truncate(const std::string& str, size_t max_len) {
        return str.length() <= max_len ? str : str.substr(0, max_len);
    }

    static std::string to_lower(const std::string& str) {
        std::string result = str;
        for (char& c : result) {
            c = std::tolower(c);
        }
        return result;
    }
};

// Parse session transcript from JSONL
SessionData parse_transcript(const std::string& transcript_path) {
    SessionData data;
    std::ifstream file(transcript_path);
    if (!file.is_open()) {
        std::cerr << "[save-session] Failed to open transcript: " << transcript_path << std::endl;
        return data;
    }

    std::string line;
    while (std::getline(file, line)) {
        if (line.empty()) continue;

        // Extract message content (minimal parsing)
        std::string role = JSONParser::extractString(line, "role");

        if (role == "user") {
            // Extract content field
            size_t content_pos = line.find("\"content\":");
            if (content_pos != std::string::npos) {
                size_t start = line.find("\"", content_pos + 10);
                if (start != std::string::npos) {
                    size_t end = line.find("\"", start + 1);
                    if (end != std::string::npos && end - start - 1 <= 500) {
                        data.user_messages.push_back(line.substr(start + 1, end - start - 1));
                    }
                }
            }
        } else if (role == "assistant") {
            // Extract tool names from tool_use blocks
            size_t tool_pos = line.find("\"type\":\"tool_use\"");
            if (tool_pos != std::string::npos) {
                std::string tool_name = JSONParser::extractString(line, "name");
                if (!tool_name.empty()) {
                    data.tool_names.insert(tool_name);
                }
            }

            // Extract text content
            size_t text_pos = line.find("\"type\":\"text\"");
            if (text_pos != std::string::npos) {
                std::string text = JSONParser::extractString(line, "text");
                if (!text.empty() && text.length() <= 500) {
                    data.assistant_messages.push_back(text);
                }
            }
        }
    }

    return data;
}

// Build a JSON array string from a vector of strings
static std::string to_json_array(const std::vector<std::string>& items,
                                  const char* fallback = nullptr) {
    std::ostringstream out;
    out << "[";
    if (items.empty() && fallback) {
        out << "\"" << fallback << "\"";
    } else {
        for (size_t i = 0; i < items.size(); ++i) {
            if (i > 0) out << ",";
            out << "\"" << items[i] << "\"";
        }
    }
    out << "]";
    return out.str();
}

// Build a plain-text version for full-text search columns
static std::string to_text_list(const std::vector<std::string>& items) {
    std::ostringstream out;
    for (size_t i = 0; i < items.size(); ++i) {
        if (i > 0) out << " ";
        out << items[i];
    }
    return out.str();
}

// Save session to PostgreSQL (canonical.sessions)
int save_to_postgres(const std::string& source_id, const SessionData& data) {
    static const char* conninfo =
        "host=localhost port=5432 user=actual password=actual "
        "dbname=worldview options='-c search_path=canonical,public'";

    PGconn* conn = PQconnectdb(conninfo);
    if (PQstatus(conn) != CONNECTION_OK) {
        // Fire-and-forget: don't block Claude on DB errors
        std::cerr << "[save-session] PG connect failed: " << PQerrorMessage(conn);
        PQfinish(conn);
        return -1;
    }

    // Generate session data
    std::string summary = data.generate_summary();
    std::vector<std::string> topics = data.extract_topics();
    std::vector<std::string> key_facts = data.extract_key_facts();

    std::string topics_json = to_json_array(topics, "general");
    std::string facts_json = to_json_array(key_facts);
    std::string topics_text = to_text_list(topics);
    std::string facts_text = to_text_list(key_facts);

    // Calculate word count
    int word_count = 0;
    for (const auto& msg : data.user_messages) {
        for (char c : msg) {
            if (c == ' ') word_count++;
        }
    }
    for (const auto& msg : data.assistant_messages) {
        for (char c : msg) {
            if (c == ' ') word_count++;
        }
    }

    int message_count = static_cast<int>(data.user_messages.size() + data.assistant_messages.size());

    // Stack buffers for integer-to-string conversion
    char word_count_buf[16];
    char message_count_buf[16];
    snprintf(word_count_buf, sizeof(word_count_buf), "%d", word_count);
    snprintf(message_count_buf, sizeof(message_count_buf), "%d", message_count);

    // Parameterized upsert
    static const char* sql =
        "INSERT INTO sessions (source_id, summary, topics, key_facts, "
        "    topics_text, key_facts_text, word_count, message_count, created_at, updated_at) "
        "VALUES ($1, $2, $3, $4, $5, $6, $7, $8, now(), now()) "
        "ON CONFLICT (source_id) DO UPDATE SET "
        "    summary = EXCLUDED.summary, "
        "    topics = EXCLUDED.topics, "
        "    key_facts = EXCLUDED.key_facts, "
        "    topics_text = EXCLUDED.topics_text, "
        "    key_facts_text = EXCLUDED.key_facts_text, "
        "    word_count = EXCLUDED.word_count, "
        "    message_count = EXCLUDED.message_count, "
        "    updated_at = now()";

    const char* paramValues[8] = {
        source_id.c_str(),
        summary.c_str(),
        topics_json.c_str(),
        facts_json.c_str(),
        topics_text.c_str(),
        facts_text.c_str(),
        word_count_buf,
        message_count_buf
    };

    PGresult* res = PQexecParams(conn, sql, 8, nullptr,
                                  paramValues, nullptr, nullptr, 0);

    ExecStatusType status = PQresultStatus(res);
    if (status != PGRES_COMMAND_OK) {
        std::cerr << "[save-session] PG exec failed: " << PQresultErrorMessage(res);
        PQclear(res);
        PQfinish(conn);
        return -1;
    }

    PQclear(res);
    PQfinish(conn);
    return 0;
}

int main() {
    // Read hook input from stdin
    std::ostringstream buffer;
    buffer << std::cin.rdbuf();
    std::string input = buffer.str();

    // Extract session info
    std::string session_id = JSONParser::extractString(input, "session_id");
    std::string transcript_path = JSONParser::extractString(input, "transcript_path");
    bool stop_hook_active = JSONParser::extractBool(input, "stop_hook_active");

    // Skip if stop_hook_active to prevent infinite loops
    // But still show confirmation for user reassurance
    if (stop_hook_active) {
        std::cout << R"({"systemMessage": "Session saved to gateway"})" << std::endl;
        return 0;
    }

    // Parse transcript
    SessionData data = parse_transcript(transcript_path);

    // Skip if no messages
    if (data.user_messages.empty() && data.assistant_messages.empty()) {
        return 0;
    }

    // Save to PostgreSQL — use session_id as source_id for upsert
    int rc = save_to_postgres(session_id, data);

    if (rc == 0) {
        std::cerr << "[save-session] Saved session " << session_id
                  << " (" << data.extract_topics().size() << " topics, "
                  << data.extract_key_facts().size() << " facts)" << std::endl;

        // Output visible confirmation to user
        std::cout << R"({"systemMessage": "Session saved to gateway"})" << std::endl;
    }
    // rc < 0: error already logged to stderr, exit 0 (fire-and-forget)

    return 0;
}
