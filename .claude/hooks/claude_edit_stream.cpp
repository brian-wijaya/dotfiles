// claude_edit_stream.cpp — PostToolUse hook: stream Edit/Write events to Emacs
//
// Fires on: PostToolUse (filtered to Edit and Write tool calls)
// Reads hook JSON from stdin, extracts file_path/old_string/new_string.
// Appends a JSON line to /dev/shm/claude_edit_stream (O_APPEND for atomicity).
// Forks a child to invoke emacsclient --eval, parent exits immediately.
//
// Raw POSIX I/O, zero heap allocation, stack buffers only.
// Target latency: <2ms parent exit time
//
// Compile:
//   g++ -O2 -o claude_edit_stream claude_edit_stream.cpp

#include <fcntl.h>
#include <unistd.h>
#include <cstdio>
#include <cstring>
#include <ctime>

static constexpr int INPUT_BUF = 65536;
static constexpr int EXTRACT_BUF = 32768;
static constexpr int ESCAPE_BUF = 65536;
static constexpr int OUTPUT_BUF = 131072;
static constexpr const char* SHM_PATH = "/dev/shm/claude_edit_stream";

// JSON-escape src into dst. Returns bytes written (not including null terminator).
// Handles: " \ newline carriage-return tab and control chars.
// dst must have room for at least 2*src_len + 1 bytes.
static int json_escape(const char* src, int src_len, char* dst, int dst_cap) {
    int j = 0;
    for (int i = 0; i < src_len && j < dst_cap - 2; i++) {
        unsigned char c = (unsigned char)src[i];
        switch (c) {
            case '"':
                if (j + 2 > dst_cap - 1) goto done;
                dst[j++] = '\\'; dst[j++] = '"';
                break;
            case '\\':
                if (j + 2 > dst_cap - 1) goto done;
                dst[j++] = '\\'; dst[j++] = '\\';
                break;
            case '\n':
                if (j + 2 > dst_cap - 1) goto done;
                dst[j++] = '\\'; dst[j++] = 'n';
                break;
            case '\r':
                if (j + 2 > dst_cap - 1) goto done;
                dst[j++] = '\\'; dst[j++] = 'r';
                break;
            case '\t':
                if (j + 2 > dst_cap - 1) goto done;
                dst[j++] = '\\'; dst[j++] = 't';
                break;
            case '\b':
                if (j + 2 > dst_cap - 1) goto done;
                dst[j++] = '\\'; dst[j++] = 'b';
                break;
            case '\f':
                if (j + 2 > dst_cap - 1) goto done;
                dst[j++] = '\\'; dst[j++] = 'f';
                break;
            default:
                if (c < 0x20) {
                    // Encode control characters as \u00XX
                    if (j + 6 > dst_cap - 1) goto done;
                    j += snprintf(dst + j, dst_cap - j, "\\u%04x", c);
                } else {
                    dst[j++] = (char)c;
                }
                break;
        }
    }
done:
    dst[j] = '\0';
    return j;
}

// Extract a JSON string value for a given key. Searches the entire JSON
// (works for nested objects since our keys are unique across the payload).
// Handles escaped characters in the value. Returns length written to dst, or 0 if not found.
static int json_extract(const char* json, int json_len, const char* key,
                         char* dst, int dst_len) {
    // Build search pattern: "key":"
    char pattern[128];
    int plen = snprintf(pattern, sizeof(pattern), "\"%s\":", key);
    if (plen <= 0 || plen >= (int)sizeof(pattern)) return 0;

    const char* pos = strstr(json, pattern);
    if (!pos) return 0;

    // Advance past the pattern to find the opening quote of the value
    const char* p = pos + plen;
    const char* end = json + json_len;

    // Skip whitespace between : and value
    while (p < end && (*p == ' ' || *p == '\t' || *p == '\n' || *p == '\r')) p++;

    // Must be a string value (starts with ")
    if (p >= end || *p != '"') return 0;
    p++; // skip opening quote

    int i = 0;
    while (p < end && i < dst_len - 1) {
        if (*p == '"') break; // unescaped quote = end of string
        if (*p == '\\' && p + 1 < end) {
            p++; // skip backslash
            switch (*p) {
                case '"':  dst[i++] = '"'; break;
                case '\\': dst[i++] = '\\'; break;
                case '/':  dst[i++] = '/'; break;
                case 'n':  dst[i++] = '\n'; break;
                case 'r':  dst[i++] = '\r'; break;
                case 't':  dst[i++] = '\t'; break;
                case 'b':  dst[i++] = '\b'; break;
                case 'f':  dst[i++] = '\f'; break;
                case 'u':
                    // \uXXXX — decode basic BMP characters
                    if (p + 4 < end) {
                        unsigned int cp = 0;
                        for (int k = 1; k <= 4; k++) {
                            char h = p[k];
                            cp <<= 4;
                            if (h >= '0' && h <= '9') cp |= (h - '0');
                            else if (h >= 'a' && h <= 'f') cp |= (h - 'a' + 10);
                            else if (h >= 'A' && h <= 'F') cp |= (h - 'A' + 10);
                        }
                        p += 4; // skip the 4 hex digits
                        // Encode as UTF-8
                        if (cp < 0x80) {
                            dst[i++] = (char)cp;
                        } else if (cp < 0x800 && i + 1 < dst_len - 1) {
                            dst[i++] = (char)(0xC0 | (cp >> 6));
                            dst[i++] = (char)(0x80 | (cp & 0x3F));
                        } else if (i + 2 < dst_len - 1) {
                            dst[i++] = (char)(0xE0 | (cp >> 12));
                            dst[i++] = (char)(0x80 | ((cp >> 6) & 0x3F));
                            dst[i++] = (char)(0x80 | (cp & 0x3F));
                        }
                    }
                    break;
                default:
                    dst[i++] = *p;
                    break;
            }
        } else {
            dst[i++] = *p;
        }
        p++;
    }
    dst[i] = '\0';
    return i;
}

// Check if a string equals a given value (avoids strcmp dependency ambiguity)
static bool streq(const char* a, const char* b) {
    while (*a && *b) {
        if (*a != *b) return false;
        a++; b++;
    }
    return *a == *b;
}

int main() {
    // Read stdin fully — hook contract delivers JSON on stdin
    char input[INPUT_BUF];
    int total = 0;
    while (total < INPUT_BUF - 1) {
        ssize_t n = read(STDIN_FILENO, input + total, INPUT_BUF - 1 - total);
        if (n <= 0) break;
        total += n;
    }
    input[total] = '\0';

    if (total == 0) return 0;

    // Extract tool_name — only process Edit and Write
    char tool_name[64];
    int tn_len = json_extract(input, total, "tool_name", tool_name, sizeof(tool_name));
    if (tn_len == 0) return 0;

    bool is_edit = streq(tool_name, "Edit");
    bool is_write = streq(tool_name, "Write");
    if (!is_edit && !is_write) return 0;

    // Extract file_path (inside tool_input, but key is unique in the payload)
    char file_path[4096];
    int fp_len = json_extract(input, total, "file_path", file_path, sizeof(file_path));
    if (fp_len == 0) return 0;

    // Extract old_string and new_string for Edit tool
    // For Write tool, we leave them as empty strings (Emacs handles diffing)
    char old_string[EXTRACT_BUF];
    char new_string[EXTRACT_BUF];
    int old_len = 0;
    int new_len = 0;

    if (is_edit) {
        old_len = json_extract(input, total, "old_string", old_string, sizeof(old_string));
        new_len = json_extract(input, total, "new_string", new_string, sizeof(new_string));
    } else {
        old_string[0] = '\0';
        new_string[0] = '\0';
    }

    // JSON-escape all extracted values for output
    char esc_file[8192];
    int esc_file_len = json_escape(file_path, fp_len, esc_file, sizeof(esc_file));

    char esc_old[ESCAPE_BUF];
    int esc_old_len = json_escape(old_string, old_len, esc_old, sizeof(esc_old));

    char esc_new[ESCAPE_BUF];
    int esc_new_len = json_escape(new_string, new_len, esc_new, sizeof(esc_new));

    // Get timestamp
    time_t ts = time(nullptr);

    // Build output JSON line
    char output[OUTPUT_BUF];
    int out_len = snprintf(output, sizeof(output),
        "{\"tool\":\"%s\",\"file\":\"%.*s\",\"old\":\"%.*s\",\"new\":\"%.*s\",\"ts\":%ld}\n",
        is_edit ? "Edit" : "Write",
        esc_file_len, esc_file,
        esc_old_len, esc_old,
        esc_new_len, esc_new,
        (long)ts);

    if (out_len <= 0 || out_len >= (int)sizeof(output)) return 0;

    // Write to SHM file with O_APPEND for atomic appends
    int fd = open(SHM_PATH, O_WRONLY | O_CREAT | O_APPEND, 0644);
    if (fd < 0) return 0;
    write(fd, output, out_len);
    close(fd);

    // Fork child to invoke emacsclient — parent exits immediately
    pid_t pid = fork();
    if (pid == 0) {
        // Child: detach from parent session, close inherited fds
        setsid();
        close(STDIN_FILENO);
        close(STDOUT_FILENO);
        close(STDERR_FILENO);
        alarm(3); // Kill child after 3 seconds if emacsclient hangs
        execlp("emacsclient", "emacsclient", "--eval",
               "(claude-edit-stream--process-pending)", nullptr);
        _exit(1); // exec failed — silent exit
    }
    // Parent: don't wait for child, just exit

    // No stdout output — this hook is silent
    return 0;
}
