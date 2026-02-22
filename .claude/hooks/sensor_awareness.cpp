// sensor_awareness.cpp — PostToolUse hook: inject sensor ambient state into Claude context
//
// Reads /dev/shm/sensor_ambient (written by sensor fusion engine at 100Hz)
// Returns JSON with additionalContext field for Claude Code hook system
//
// Raw POSIX I/O, single heap allocation for stdin (Edit/Write payloads can be large).
// Target latency: <0.5ms
//
// Compile:
//   g++ -O2 -o sensor_awareness sensor_awareness.cpp

#include <fcntl.h>
#include <unistd.h>
#include <cstdio>
#include <cstdlib>
#include <cstring>

static constexpr int SHM_BUF = 1024;
static constexpr int EMACS_BUF = 256;
static constexpr int I3_BUF = 256;
static constexpr int OUT_BUF = 2048;
static constexpr int STDIN_BUF = 262144;  // 256KB — Edit/Write tool_input can be large
static constexpr int STATS_BUF = 1024;
static constexpr int TOOLS_BUF = 512;
static constexpr int TOOL_NAME_BUF = 128;
static constexpr int EDIT_FILES_BUF = 4096;
static constexpr int FILE_PATH_BUF = 512;
static constexpr int MAX_EDIT_ENTRIES = 64;
static constexpr const char* SHM_PATH = "/dev/shm/sensor_ambient";
// STATS_PATH removed — now computed dynamically via shm_path()
static constexpr const char* EMACS_SHM_PATH = "/dev/shm/emacs_state";
static constexpr const char* I3_SHM_PATH = "/dev/shm/x11_state";

// JSON-escape src into dst. Returns bytes written (not including null terminator).
// dst must have room for at least 2*src_len + 1 bytes.
static int json_escape(const char* src, int src_len, char* dst, int dst_cap) {
    int j = 0;
    for (int i = 0; i < src_len && j < dst_cap - 1; i++) {
        char c = src[i];
        switch (c) {
            case '"':  if (j + 2 > dst_cap - 1) goto done; dst[j++] = '\\'; dst[j++] = '"'; break;
            case '\\': if (j + 2 > dst_cap - 1) goto done; dst[j++] = '\\'; dst[j++] = '\\'; break;
            case '\n': if (j + 2 > dst_cap - 1) goto done; dst[j++] = '\\'; dst[j++] = 'n'; break;
            case '\r': if (j + 2 > dst_cap - 1) goto done; dst[j++] = '\\'; dst[j++] = 'r'; break;
            case '\t': if (j + 2 > dst_cap - 1) goto done; dst[j++] = '\\'; dst[j++] = 't'; break;
            default:
                if ((unsigned char)c < 0x20) {
                    // Skip control characters
                } else {
                    dst[j++] = c;
                }
                break;
        }
    }
done:
    dst[j] = '\0';
    return j;
}

// Extract a JSON string value for a given key. Finds "key":"value" pattern.
// Returns length written to dst, or 0 if not found. Stack-only.
static int json_extract(const char* json, int json_len, const char* key, char* dst, int dst_len) {
    char pattern[128];
    int plen = snprintf(pattern, sizeof(pattern), "\"%s\":\"", key);
    if (plen <= 0 || plen >= (int)sizeof(pattern)) return 0;

    const char* pos = nullptr;
    const char* search = json;
    while ((pos = strstr(search, pattern)) != nullptr) {
        if (pos > json) {
            char prev = *(pos - 1);
            if (prev != '{' && prev != ',' && prev != ' ' && prev != '\t' && prev != '\n' && prev != '\r') {
                search = pos + 1;
                continue;
            }
        }
        break;
    }
    if (!pos) return 0;

    const char* start = pos + plen;
    const char* end = json + json_len;
    int i = 0;
    const char* p = start;
    while (p < end && i < dst_len - 1) {
        if (*p == '"') break;
        if (*p == '\\' && p + 1 < end) {
            p++;
            switch (*p) {
                case 'n': dst[i++] = '\n'; break;
                case 't': dst[i++] = '\t'; break;
                case 'r': dst[i++] = '\r'; break;
                case '"': dst[i++] = '"'; break;
                case '\\': dst[i++] = '\\'; break;
                case '/': dst[i++] = '/'; break;
                default: dst[i++] = *p; break;
            }
        } else {
            dst[i++] = *p;
        }
        p++;
    }
    dst[i] = '\0';
    return i;
}

// Build a session-scoped SHM path: /dev/shm/{base}.{first8chars} or /dev/shm/{base} if no session_id.
static void shm_path(char* buf, int buf_len, const char* base, const char* session_id) {
    if (session_id[0] != '\0') {
        snprintf(buf, buf_len, "/dev/shm/%s.%.8s", base, session_id);
    } else {
        snprintf(buf, buf_len, "/dev/shm/%s", base);
    }
}

// Check if tool_name exists as a whole token in a comma-separated list.
// Prevents partial matches (e.g., "Read" matching "ReadFile").
static bool tool_in_list(const char* list, const char* tool) {
    int tool_len = strlen(tool);
    if (tool_len == 0) return false;

    const char* p = list;
    while ((p = strstr(p, tool)) != nullptr) {
        // Check left boundary: must be start-of-string or preceded by comma
        if (p != list && *(p - 1) != ',') {
            p++;
            continue;
        }
        // Check right boundary: must be end-of-string, comma, or newline
        char after = p[tool_len];
        if (after == '\0' || after == ',' || after == '\n') {
            return true;
        }
        p++;
    }
    return false;
}

// Find the start of a JSON object value for a given key.
// Returns pointer to the opening '{' of the object, or nullptr.
static const char* find_json_object(const char* json, int json_len, const char* key) {
    char pattern[128];
    int plen = snprintf(pattern, sizeof(pattern), "\"%s\"", key);
    if (plen <= 0 || plen >= (int)sizeof(pattern)) return nullptr;

    const char* pos = strstr(json, pattern);
    if (!pos) return nullptr;

    // Skip past the key and find ':'
    const char* p = pos + plen;
    const char* end = json + json_len;
    while (p < end && (*p == ' ' || *p == '\t' || *p == '\n' || *p == '\r')) p++;
    if (p >= end || *p != ':') return nullptr;
    p++;
    while (p < end && (*p == ' ' || *p == '\t' || *p == '\n' || *p == '\r')) p++;
    if (p >= end || *p != '{') return nullptr;
    return p;
}

// Find the end of a JSON object (matching closing '}').
// Returns pointer past the '}', or nullptr on failure.
static const char* find_json_object_end(const char* start, const char* end) {
    if (!start || *start != '{') return nullptr;
    int depth = 0;
    bool in_string = false;
    for (const char* p = start; p < end; p++) {
        if (in_string) {
            if (*p == '\\') { p++; continue; }
            if (*p == '"') in_string = false;
            continue;
        }
        if (*p == '"') { in_string = true; continue; }
        if (*p == '{') depth++;
        else if (*p == '}') { depth--; if (depth == 0) return p + 1; }
    }
    return nullptr;
}

// Count newlines inside a JSON string value for a given key within a region.
// Scans the escaped JSON string in-place, counting literal '\n' (escaped as \\n in JSON).
// Returns the number of lines (newline count + 1 for non-empty, 0 for empty/not found).
static int count_lines_in_json_value(const char* json, int json_len, const char* key) {
    char pattern[128];
    int plen = snprintf(pattern, sizeof(pattern), "\"%s\":\"", key);
    if (plen <= 0 || plen >= (int)sizeof(pattern)) return 0;

    const char* pos = nullptr;
    const char* search = json;
    const char* end = json + json_len;
    while ((pos = strstr(search, pattern)) != nullptr) {
        if (pos > json) {
            char prev = *(pos - 1);
            if (prev != '{' && prev != ',' && prev != ' ' && prev != '\t' && prev != '\n' && prev != '\r') {
                search = pos + 1;
                continue;
            }
        }
        break;
    }
    if (!pos) return 0;

    const char* start = pos + plen;
    int newlines = 0;
    bool empty = true;
    for (const char* p = start; p < end; p++) {
        if (*p == '"') break;  // End of string value
        empty = false;
        if (*p == '\\' && p + 1 < end) {
            p++;
            if (*p == 'n') newlines++;
        }
    }
    return empty ? 0 : newlines + 1;
}

// Extract file_path from tool_input object in the JSON.
// Looks for "tool_input":{..."file_path":"value"...}
static int extract_tool_input_file_path(const char* json, int json_len, char* dst, int dst_len) {
    const char* obj_start = find_json_object(json, json_len, "tool_input");
    if (!obj_start) return 0;
    const char* end = json + json_len;
    const char* obj_end = find_json_object_end(obj_start, end);
    if (!obj_end) return 0;
    int obj_len = (int)(obj_end - obj_start);
    return json_extract(obj_start, obj_len, "file_path", dst, dst_len);
}

// Count lines in a tool_input string field.
static int count_tool_input_lines(const char* json, int json_len, const char* field) {
    const char* obj_start = find_json_object(json, json_len, "tool_input");
    if (!obj_start) return 0;
    const char* end = json + json_len;
    const char* obj_end = find_json_object_end(obj_start, end);
    if (!obj_end) return 0;
    int obj_len = (int)(obj_end - obj_start);
    return count_lines_in_json_value(obj_start, obj_len, field);
}

// Track which files were edited and their line counts.
// Reads existing SHM file, updates/adds entry, writes back.
static void update_edit_files(const char* tool_name, const char* session_id,
                               const char* json, int json_len) {
    // Only process Edit and Write tools
    bool is_edit = (strcmp(tool_name, "Edit") == 0);
    bool is_write = (strcmp(tool_name, "Write") == 0);
    if (!is_edit && !is_write) return;

    // Extract file_path from tool_input
    char file_path[FILE_PATH_BUF] = "";
    int fp_len = extract_tool_input_file_path(json, json_len, file_path, sizeof(file_path));
    if (fp_len == 0) return;

    // Calculate line count
    int line_count = 0;
    if (is_edit) {
        int new_lines = count_tool_input_lines(json, json_len, "new_string");
        int old_lines = count_tool_input_lines(json, json_len, "old_string");
        line_count = new_lines - old_lines;
        if (line_count < 0) line_count = -line_count;  // Absolute net change
    } else {
        // Write tool — count lines in content
        line_count = count_tool_input_lines(json, json_len, "content");
    }

    // Build SHM path
    char edit_path[256];
    shm_path(edit_path, sizeof(edit_path), "turn_edit_files", session_id);

    // Read existing file
    char edit_buf[EDIT_FILES_BUF] = "";
    int edit_fd = open(edit_path, O_RDONLY);
    if (edit_fd >= 0) {
        ssize_t en = read(edit_fd, edit_buf, sizeof(edit_buf) - 1);
        close(edit_fd);
        if (en > 0) edit_buf[en] = '\0';
    }

    // Parse existing entries into parallel arrays (stack-allocated)
    char paths[MAX_EDIT_ENTRIES][FILE_PATH_BUF];
    int counts[MAX_EDIT_ENTRIES];
    int num_entries = 0;

    const char* line = edit_buf;
    while (line && *line && num_entries < MAX_EDIT_ENTRIES) {
        const char* eol = strchr(line, '\n');
        int line_len = eol ? (int)(eol - line) : (int)strlen(line);
        if (line_len == 0) { line = eol ? eol + 1 : nullptr; continue; }

        // Find last colon (path may contain colons, count won't)
        const char* colon = nullptr;
        for (int i = line_len - 1; i >= 0; i--) {
            if (line[i] == ':') { colon = line + i; break; }
        }
        if (colon) {
            int path_len = (int)(colon - line);
            if (path_len > 0 && path_len < FILE_PATH_BUF) {
                memcpy(paths[num_entries], line, path_len);
                paths[num_entries][path_len] = '\0';
                counts[num_entries] = 0;
                sscanf(colon + 1, "%d", &counts[num_entries]);
                num_entries++;
            }
        }

        line = eol ? eol + 1 : nullptr;
    }

    // Check if file_path already exists — if so, update its count
    bool found = false;
    for (int i = 0; i < num_entries; i++) {
        if (strcmp(paths[i], file_path) == 0) {
            counts[i] += line_count;
            found = true;
            break;
        }
    }

    // If not found, add new entry
    if (!found && num_entries < MAX_EDIT_ENTRIES) {
        strncpy(paths[num_entries], file_path, FILE_PATH_BUF - 1);
        paths[num_entries][FILE_PATH_BUF - 1] = '\0';
        counts[num_entries] = line_count;
        num_entries++;
    }

    // Write updated file
    char new_buf[EDIT_FILES_BUF];
    int pos = 0;
    for (int i = 0; i < num_entries && pos < (int)sizeof(new_buf) - FILE_PATH_BUF - 16; i++) {
        pos += snprintf(new_buf + pos, sizeof(new_buf) - pos, "%s:%d\n", paths[i], counts[i]);
    }

    int wfd = open(edit_path, O_WRONLY | O_CREAT | O_TRUNC, 0644);
    if (wfd >= 0) {
        write(wfd, new_buf, pos);
        close(wfd);
    }
}

// Read current turn_stats, update counters for tool_name, write back.
// All stack buffers. Fails silently on any error.
static void update_turn_stats(const char* tool_name, const char* session_id) {
    if (tool_name[0] == '\0') return;

    char stats_path[256];
    shm_path(stats_path, sizeof(stats_path), "turn_stats", session_id);

    char stats_buf[STATS_BUF] = "";
    int stats_fd = open(stats_path, O_RDONLY);
    if (stats_fd >= 0) {
        ssize_t sn = read(stats_fd, stats_buf, sizeof(stats_buf) - 1);
        close(stats_fd);
        if (sn > 0) stats_buf[sn] = '\0';
    }

    int total = 0, agents = 0, edits = 0;
    char tools_list[TOOLS_BUF] = "";

    // Parse existing stats
    const char* line = stats_buf;
    while (line && *line) {
        if (strncmp(line, "total=", 6) == 0) {
            sscanf(line + 6, "%d", &total);
        } else if (strncmp(line, "agents=", 7) == 0) {
            sscanf(line + 7, "%d", &agents);
        } else if (strncmp(line, "edits=", 6) == 0) {
            sscanf(line + 6, "%d", &edits);
        } else if (strncmp(line, "tools=", 6) == 0) {
            const char* val = line + 6;
            const char* eol = strchr(val, '\n');
            int len = eol ? (int)(eol - val) : (int)strlen(val);
            if (len > 0 && len < (int)sizeof(tools_list)) {
                memcpy(tools_list, val, len);
                tools_list[len] = '\0';
            }
        }
        const char* next = strchr(line, '\n');
        line = next ? next + 1 : nullptr;
    }

    // Update counters
    total++;
    if (strcmp(tool_name, "Task") == 0) {
        agents++;

        // Increment session-level agent turn counter (never reset by capture-user-turn)
        char agent_total_path[256];
        shm_path(agent_total_path, sizeof(agent_total_path), "agent_turn_total", session_id);

        int agent_total = 0;
        int at_fd = open(agent_total_path, O_RDONLY);
        if (at_fd >= 0) {
            char at_buf[32] = "";
            ssize_t at_n = read(at_fd, at_buf, sizeof(at_buf) - 1);
            close(at_fd);
            if (at_n > 0) {
                at_buf[at_n] = '\0';
                sscanf(at_buf, "%d", &agent_total);
            }
        }
        agent_total++;

        char at_out[32];
        int at_out_len = snprintf(at_out, sizeof(at_out), "%d\n", agent_total);
        int at_wfd = open(agent_total_path, O_WRONLY | O_CREAT | O_TRUNC, 0644);
        if (at_wfd >= 0) {
            write(at_wfd, at_out, at_out_len);
            close(at_wfd);
        }
    }
    if (strcmp(tool_name, "Edit") == 0 || strcmp(tool_name, "Write") == 0) edits++;

    // Append tool_name to tools list if not already present
    if (!tool_in_list(tools_list, tool_name)) {
        int cur_len = strlen(tools_list);
        if (cur_len > 0) {
            snprintf(tools_list + cur_len, sizeof(tools_list) - cur_len, ",%s", tool_name);
        } else {
            snprintf(tools_list, sizeof(tools_list), "%s", tool_name);
        }
    }

    // Write updated stats
    char new_stats[STATS_BUF];
    int ns_len = snprintf(new_stats, sizeof(new_stats),
        "total=%d\nagents=%d\nedits=%d\ntools=%s\n",
        total, agents, edits, tools_list);

    int ws_fd = open(stats_path, O_WRONLY | O_CREAT | O_TRUNC, 0644);
    if (ws_fd >= 0) {
        write(ws_fd, new_stats, ns_len);
        close(ws_fd);
    }
}

int main() {
    // Read stdin — hooks receive JSON input that must be consumed
    // Heap-allocated because Edit/Write tool_input can be large
    char* input = (char*)malloc(STDIN_BUF);
    if (!input) return 0;
    int total_read = 0;
    {
        ssize_t nr;
        while ((nr = read(STDIN_FILENO, input + total_read,
                          STDIN_BUF - 1 - total_read)) > 0) {
            total_read += nr;
            if (total_read >= STDIN_BUF - 1) break;
        }
        input[total_read] = '\0';
    }

    // Extract tool_name and session_id from PostToolUse JSON input
    char tool_name[TOOL_NAME_BUF] = "";
    char session_id[256] = "";
    if (total_read > 0) {
        json_extract(input, total_read, "tool_name", tool_name, sizeof(tool_name));
        json_extract(input, total_read, "session_id", session_id, sizeof(session_id));
    }

    // Update turn_stats SHM (fire-and-forget, fails silently)
    if (tool_name[0] != '\0') {
        update_turn_stats(tool_name, session_id);
        update_edit_files(tool_name, session_id, input, total_read);
    }

    // Read /dev/shm/sensor_ambient
    int fd = open(SHM_PATH, O_RDONLY);
    if (fd < 0) { free(input); return 0; }  // File doesn't exist — silent exit

    char shm[SHM_BUF];
    ssize_t n = read(fd, shm, sizeof(shm) - 1);
    close(fd);

    if (n <= 0) { free(input); return 0; }  // Empty or read error — silent exit

    // Strip trailing newline/whitespace
    while (n > 0 && (shm[n - 1] == '\n' || shm[n - 1] == '\r' || shm[n - 1] == ' ')) {
        n--;
    }
    if (n == 0) { free(input); return 0; }
    shm[n] = '\0';

    // JSON-escape the sensor content
    char escaped[SHM_BUF * 2];
    int esc_len = json_escape(shm, n, escaped, sizeof(escaped));

    // Read /dev/shm/emacs_state (optional — may not exist)
    char emacs_escaped[EMACS_BUF * 2];
    int emacs_esc_len = 0;

    int efd = open(EMACS_SHM_PATH, O_RDONLY);
    if (efd >= 0) {
        char emacs_shm[EMACS_BUF];
        ssize_t en = read(efd, emacs_shm, sizeof(emacs_shm) - 1);
        close(efd);

        if (en > 0) {
            // Strip trailing whitespace
            while (en > 0 && (emacs_shm[en - 1] == '\n' || emacs_shm[en - 1] == '\r' || emacs_shm[en - 1] == ' ')) {
                en--;
            }
            if (en > 0) {
                emacs_shm[en] = '\0';
                emacs_esc_len = json_escape(emacs_shm, en, emacs_escaped, sizeof(emacs_escaped));
            }
        }
    }

    // Read /dev/shm/x11_state (optional — may not exist)
    char i3_escaped[I3_BUF * 2];
    int i3_esc_len = 0;

    int i3fd = open(I3_SHM_PATH, O_RDONLY);
    if (i3fd >= 0) {
        char i3_shm[I3_BUF];
        ssize_t i3n = read(i3fd, i3_shm, sizeof(i3_shm) - 1);
        close(i3fd);

        if (i3n > 0) {
            // Strip trailing whitespace
            while (i3n > 0 && (i3_shm[i3n - 1] == '\n' || i3_shm[i3n - 1] == '\r' || i3_shm[i3n - 1] == ' ')) {
                i3n--;
            }
            if (i3n > 0) {
                i3_shm[i3n] = '\0';
                i3_esc_len = json_escape(i3_shm, i3n, i3_escaped, sizeof(i3_escaped));
            }
        }
    }

    // Build output: {"additionalContext":"[env: ...] [emacs: ...] [i3: ...]"}
    char out[OUT_BUF];
    int out_len;

    // Start building the context string piece by piece
    char context[OUT_BUF];
    int ctx_len = snprintf(context, sizeof(context), "[env: %.*s]", esc_len, escaped);

    if (emacs_esc_len > 0) {
        ctx_len += snprintf(context + ctx_len, sizeof(context) - ctx_len,
            " [emacs: %.*s]", emacs_esc_len, emacs_escaped);
    }

    if (i3_esc_len > 0) {
        ctx_len += snprintf(context + ctx_len, sizeof(context) - ctx_len,
            " [i3: %.*s]", i3_esc_len, i3_escaped);
    }

    out_len = snprintf(out, sizeof(out),
        "{\"additionalContext\":\"%.*s\"}", ctx_len, context);

    if (out_len > 0 && out_len < (int)sizeof(out)) {
        write(STDOUT_FILENO, out, out_len);
    }

    free(input);
    return 0;
}
