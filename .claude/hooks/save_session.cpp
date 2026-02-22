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
#include <cctype>
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

// Extract an integer value after a given JSON key (e.g. "output_tokens":123)
static long long extract_int_after(const std::string& line, const std::string& key) {
    size_t pos = line.find(key);
    if (pos == std::string::npos) return 0;
    pos += key.length();
    // Skip to first digit
    while (pos < line.length() && !std::isdigit(static_cast<unsigned char>(line[pos]))) pos++;
    if (pos >= line.length()) return 0;
    // Extract digits
    size_t start = pos;
    while (pos < line.length() && std::isdigit(static_cast<unsigned char>(line[pos]))) pos++;
    return std::stoll(line.substr(start, pos - start));
}

// Extract summary from JSONL transcript
struct SessionData {
    std::vector<std::string> user_messages;
    std::vector<std::string> assistant_messages;
    std::unordered_set<std::string> tool_names;

    long long total_output_tokens = 0;    // sum of output_tokens from all non-sidechain assistant entries
    long long agent_output_tokens = 0;    // sum of output_tokens from all sidechain assistant entries
    long long last_input_tokens = 0;      // input_tokens from the LAST assistant entry (for context %)

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

// Look up session name from multiple sources
// Priority: 1) last /rename in history.jsonl
//           2) customTitle from sessions-index.json
//           3) summary from sessions-index.json
//           4) session_info entry in JSONL transcript (last one wins)
//           5) first user message from JSONL transcript (truncated to 60 chars)
//           6) slug from JSONL transcript
//           7) empty
std::string lookup_session_name(const std::string& session_id,
                                 const std::string& transcript_path = "") {
    // Step 1: Check history.jsonl for /rename entries
    std::string history_path = std::string(std::getenv("HOME")) + "/.claude/history.jsonl";
    std::ifstream file(history_path);

    std::string last_name;
    if (file.is_open()) {
        std::string line;
        while (std::getline(file, line)) {
            if (line.empty()) continue;

            // Check if this line contains our session_id
            if (strstr(line.c_str(), session_id.c_str()) == nullptr) continue;

            // Check if it contains a /rename command in the display field
            const char* rename_pos = strstr(line.c_str(), "\"/rename ");
            if (rename_pos == nullptr) continue;

            // Extract the name: everything after "/rename " up to the closing quote
            const char* name_start = rename_pos + 9; // strlen("\"/rename ") = 9
            const char* name_end = strchr(name_start, '"');
            if (name_end == nullptr) continue;

            last_name = std::string(name_start, name_end - name_start);
        }
    }

    if (!last_name.empty()) return last_name;

    // Step 2: Fallback to sessions-index.json (customTitle > summary)
    std::string index_path = std::string(std::getenv("HOME")) +
        "/.claude/projects/-home-bw/sessions-index.json";
    std::ifstream index_file(index_path);
    if (index_file.is_open()) {
        std::ostringstream index_buf;
        index_buf << index_file.rdbuf();
        std::string index_str = index_buf.str();

        // Find "sessionId":"<session_id>" in the JSON array
        std::string session_key = "\"sessionId\":\"" + session_id + "\"";
        size_t sid_pos = index_str.find(session_key);
        if (sid_pos != std::string::npos) {
            // Find the boundaries of this entry object (nearest { before and } after)
            size_t obj_start = index_str.rfind('{', sid_pos);
            size_t obj_end = index_str.find('}', sid_pos);
            if (obj_start != std::string::npos && obj_end != std::string::npos) {
                std::string entry = index_str.substr(obj_start, obj_end - obj_start + 1);

                // Prefer customTitle over summary
                std::string custom_title = JSONParser::extractString(entry, "customTitle");
                if (!custom_title.empty()) return custom_title;

                std::string summary = JSONParser::extractString(entry, "summary");
                if (!summary.empty()) {
                    if (summary.length() > 60) {
                        summary = summary.substr(0, 60) + "...";
                    }
                    return summary;
                }
            }
        }
    }

    // Step 3: Check JSONL for session_info entries (e.g. from /rename or auto-naming)
    // These are lines like: {"type":"session_info","id":"...","name":"descriptive name","timestamp":"..."}
    std::string tpath = transcript_path;
    if (tpath.empty()) {
        // Construct default transcript path
        tpath = std::string(std::getenv("HOME")) +
            "/.claude/projects/-home-bw/" + session_id + ".jsonl";
    }
    std::ifstream transcript(tpath);
    if (transcript.is_open()) {
        std::string info_name;
        std::string first_user_message;
        std::string first_slug;
        std::string line;
        while (std::getline(transcript, line)) {
            if (line.empty()) continue;

            // Look for session_info entries — take the LAST one (most recent rename)
            if (line.find("\"type\":\"session_info\"") != std::string::npos ||
                line.find("\"type\": \"session_info\"") != std::string::npos) {
                std::string name = JSONParser::extractString(line, "name");
                if (!name.empty()) {
                    info_name = name;
                }
            }

            // Capture first user message as fallback name
            if (first_user_message.empty()) {
                bool is_human = (line.find("\"type\":\"human\"") != std::string::npos ||
                                 line.find("\"type\": \"human\"") != std::string::npos);
                bool is_user = (line.find("\"role\":\"user\"") != std::string::npos ||
                                line.find("\"role\": \"user\"") != std::string::npos);
                if (is_human || is_user) {
                    std::string content = JSONParser::extractString(line, "content");
                    if (!content.empty()) {
                        // Truncate to 60 chars at a word boundary
                        if (content.length() > 60) {
                            size_t cut = content.rfind(' ', 60);
                            if (cut == std::string::npos || cut == 0) {
                                cut = 60; // no space found, hard cut
                            }
                            content = content.substr(0, cut) + "...";
                        }
                        first_user_message = content;
                    }
                }
            }

            // Also capture first slug as fallback
            if (first_slug.empty()) {
                std::string slug = JSONParser::extractString(line, "slug");
                if (!slug.empty()) {
                    first_slug = slug;
                }
            }
        }

        // Prefer session_info name > first user message > slug
        if (!info_name.empty()) return info_name;
        if (!first_user_message.empty()) return first_user_message;
        if (!first_slug.empty()) return first_slug;
    }

    return "";
}

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

            // Extract token usage from assistant entries with "usage":{
            if (line.find("\"usage\":{") != std::string::npos) {
                long long out_tokens = extract_int_after(line, "\"output_tokens\":");
                long long in_tokens = extract_int_after(line, "\"input_tokens\":");
                long long cache_read = extract_int_after(line, "\"cache_read_input_tokens\":");
                long long cache_create = extract_int_after(line, "\"cache_creation_input_tokens\":");
                bool is_sidechain = (line.find("\"isSidechain\":true") != std::string::npos);

                if (is_sidechain) {
                    data.agent_output_tokens += out_tokens;
                } else {
                    data.total_output_tokens += out_tokens;
                }
                // With prompt caching, input_tokens is just the uncached portion (often 1).
                // Real context usage = input_tokens + cache_read + cache_creation.
                data.last_input_tokens = in_tokens + cache_read + cache_create;
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

// Build a session-scoped SHM path: /dev/shm/{base}.{session_id[:8]}
static void shm_path(char* buf, int buf_len, const char* base, const char* session_id) {
    if (session_id && session_id[0] != '\0') {
        snprintf(buf, buf_len, "/dev/shm/%s.%.8s", base, session_id);
    } else {
        snprintf(buf, buf_len, "/dev/shm/%s", base);
    }
}

// Save session to PostgreSQL (canonical.sessions)
// On success, sets session_number to dense chronological position (or -1 if query fails)
int save_to_postgres(const std::string& source_id, const SessionData& data,
                     const std::string& transcript_path, long long& session_number) {
    session_number = -1;
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

    // Look up session name from history.jsonl / sessions-index / transcript slug
    std::string session_name = lookup_session_name(source_id, transcript_path);

    // Parameterized upsert
    static const char* sql =
        "INSERT INTO sessions (source_id, summary, topics, key_facts, "
        "    topics_text, key_facts_text, word_count, message_count, session_name, created_at, updated_at) "
        "VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, now(), now()) "
        "ON CONFLICT (source_id) DO UPDATE SET "
        "    summary = EXCLUDED.summary, "
        "    topics = EXCLUDED.topics, "
        "    key_facts = EXCLUDED.key_facts, "
        "    topics_text = EXCLUDED.topics_text, "
        "    key_facts_text = EXCLUDED.key_facts_text, "
        "    word_count = EXCLUDED.word_count, "
        "    message_count = EXCLUDED.message_count, "
        "    session_name = EXCLUDED.session_name, "
        "    updated_at = now()";

    const char* paramValues[9] = {
        source_id.c_str(),
        summary.c_str(),
        topics_json.c_str(),
        facts_json.c_str(),
        topics_text.c_str(),
        facts_text.c_str(),
        word_count_buf,
        message_count_buf,
        session_name.empty() ? nullptr : session_name.c_str()
    };

    PGresult* res = PQexecParams(conn, sql, 9, nullptr,
                                  paramValues, nullptr, nullptr, 0);

    ExecStatusType status = PQresultStatus(res);
    if (status != PGRES_COMMAND_OK) {
        std::cerr << "[save-session] PG exec failed: " << PQresultErrorMessage(res);
        PQclear(res);
        PQfinish(conn);
        return -1;
    }
    PQclear(res);

    // Query session_number (dense chronological position among main sessions only).
    // Exclude agent/sidechain sessions (source_id LIKE 'claude:agent-%') so that
    // spawning Task-tool agents doesn't inflate the session counter.
    static const char* id_sql =
        "SELECT COUNT(*) FROM sessions WHERE id <= "
        "(SELECT id FROM sessions WHERE source_id = $1) "
        "AND source_id NOT LIKE 'claude:agent-%'";
    const char* id_params[1] = { source_id.c_str() };
    PGresult* id_res = PQexecParams(conn, id_sql, 1, nullptr,
                                     id_params, nullptr, nullptr, 0);
    if (PQresultStatus(id_res) == PGRES_TUPLES_OK && PQntuples(id_res) > 0) {
        session_number = std::stoll(PQgetvalue(id_res, 0, 0));
    }
    PQclear(id_res);

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
    if (stop_hook_active) {
        // Write turn_saved SHM so mosaic_sync knows we ran
        char saved_path[128];
        shm_path(saved_path, sizeof(saved_path), "turn_saved", session_id.c_str());
        std::ofstream(saved_path) << "1";
        return 0;
    }

    // Parse transcript
    SessionData data = parse_transcript(transcript_path);

    // Skip if no messages — still signal completion so mosaic_sync doesn't hang
    if (data.user_messages.empty() && data.assistant_messages.empty()) {
        char saved_path[128];
        shm_path(saved_path, sizeof(saved_path), "turn_saved", session_id.c_str());
        std::ofstream(saved_path) << "1";
        return 0;
    }

    // Save to PostgreSQL — use session_id as source_id for upsert
    long long session_number = -1;
    int rc = save_to_postgres(session_id, data, transcript_path, session_number);

    if (rc == 0) {
        std::cerr << "[save-session] Saved session " << session_id
                  << " (" << data.extract_topics().size() << " topics, "
                  << data.extract_key_facts().size() << " facts)" << std::endl;

        // Write SHM files for mosaic_sync to read (session-scoped)
        char shm_buf[128];
        std::string session_name = lookup_session_name(session_id, transcript_path);
        shm_path(shm_buf, sizeof(shm_buf), "turn_session_name", session_id.c_str());
        std::ofstream(shm_buf) << session_name;

        std::vector<std::string> topics = data.extract_topics();
        std::ostringstream topics_csv;
        for (size_t i = 0; i < topics.size(); ++i) {
            if (i > 0) topics_csv << ",";
            topics_csv << topics[i];
        }
        shm_path(shm_buf, sizeof(shm_buf), "turn_topics", session_id.c_str());
        std::ofstream(shm_buf) << topics_csv.str();

        // Write session_number to SHM
        if (session_number >= 0) {
            shm_path(shm_buf, sizeof(shm_buf), "turn_session_number", session_id.c_str());
            std::ofstream(shm_buf) << session_number;
        }

        // Write token data to SHM
        {
            long long context_pct = (data.last_input_tokens * 100) / 200000;
            if (context_pct < 0) context_pct = 0;
            if (context_pct > 100) context_pct = 100;

            shm_path(shm_buf, sizeof(shm_buf), "turn_tokens", session_id.c_str());
            std::ofstream token_file(shm_buf);
            token_file << "total_out=" << data.total_output_tokens << "\n"
                       << "agent_out=" << data.agent_output_tokens << "\n"
                       << "context_pct=" << context_pct << "\n";
        }

        shm_path(shm_buf, sizeof(shm_buf), "turn_saved", session_id.c_str());
        std::ofstream(shm_buf) << "1";
    }
    // rc < 0: error already logged to stderr, exit 0 (fire-and-forget)

    return 0;
}
