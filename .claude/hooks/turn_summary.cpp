// turn_summary.cpp - End-of-turn ANSI summary hook
// Fires on: Stop (after save_session)
// Reads session-scoped SHM files and produces formatted ANSI-colored summary
// Output: JSON on stdout: {"systemMessage":"ANSI_CONTENT"}
//
// Width-adaptive layout (fits 60-240 cols without spilling):
//   [Edit section]: ✏️ ~/.claude/hooks/turn_summary.cpp (+47 lines)  (if edits exist)
//   Line 0: \n
//   Line 1: 📄 S#1924 · 320a6835 │ 🌐 ST#17 │ ⚡ AT#12 · abcd1234
//     FULL:   📄 S#1924 · 320a6835 │ 🌐 ST#37 │ ⚡️ AT#27 · abcd1234
//     NARROW: 📄S#1924·320a6835│🌐ST#37│⚡️AT#27·abcd1234  (drop spaces)
//     TINY:   S#1924·320a6835│ST#37│AT#27·abcd1234         (drop emojis too)
//     IDs are NEVER truncated — always full 8-char hashes.
//   Line 2: 💬 session name here...              🏷️ hooks, mcp, telemetry
//     Width-enforced: visible_width(line2) <= term_cols. Name truncated first,
//     then tags (trailing items removed) if name is already minimal.
//   Line 3: 🔧 17 calls (8 unique) · 🤖 3 agents · ✏️ 2 edits · ⏱ 42.3s
//   Line 4: [💾 Saved badge] [muted: 42K/200K tok · 78% ctx] [🕐 Sun 02-22::10:54:27]
//
// Compile: cd /home/bw/.claude/hooks && g++ -O2 -o turn_summary turn_summary.cpp

#include <cstdio>
#include <cstring>
#include <cstdlib>
#include <ctime>
#include <cstdint>
#include <unistd.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <wchar.h>
#include <locale.h>
#include <string>
#include <vector>

static const char ESC = '\033';

// -- SHM helpers -------------------------------------------------------------

static void shm_path(char* buf, int buflen, const char* base, const char* session_id) {
    if (session_id[0] != '\0')
        snprintf(buf, buflen, "/dev/shm/%s.%.8s", base, session_id);
    else
        snprintf(buf, buflen, "/dev/shm/%s", base);
}

// Read small SHM file into buf, strip trailing whitespace. Returns length (0 on failure).
static int read_shm(const char* path, char* buf, int buf_len) {
    int fd = open(path, O_RDONLY);
    if (fd < 0) { buf[0] = '\0'; return 0; }
    ssize_t n = read(fd, buf, buf_len - 1);
    close(fd);
    if (n <= 0) { buf[0] = '\0'; return 0; }
    while (n > 0 && (buf[n-1] == '\n' || buf[n-1] == '\r' || buf[n-1] == ' '))
        n--;
    buf[n] = '\0';
    return (int)n;
}

// -- Parsing helpers ---------------------------------------------------------

// Extract JSON string value for "key":"value" from raw JSON
static int json_extract(const char* json, int json_len, const char* key, char* dst, int dst_len) {
    char pattern[128];
    int plen = snprintf(pattern, sizeof(pattern), "\"%s\":\"", key);
    if (plen <= 0 || plen >= (int)sizeof(pattern)) return 0;
    const char* pos = strstr(json, pattern);
    if (!pos) return 0;
    const char* start = pos + plen;
    const char* end_ptr = json + json_len;
    int i = 0;
    for (const char* p = start; p < end_ptr && i < dst_len - 1; p++) {
        if (*p == '"') break;
        if (*p == '\\' && p + 1 < end_ptr) { p++; dst[i++] = *p; }
        else dst[i++] = *p;
    }
    dst[i] = '\0';
    return i;
}

// Parse "key=N\n" integer from multiline stats
static int parse_stat_int(const char* buf, const char* key) {
    char search[64];
    snprintf(search, sizeof(search), "%s=", key);
    const char* p = strstr(buf, search);
    if (!p) return 0;
    return atoi(p + strlen(search));
}

// Parse "key=value\n" string from multiline stats
static int parse_stat_str(const char* buf, const char* key, char* dst, int dst_len) {
    char search[64];
    snprintf(search, sizeof(search), "%s=", key);
    const char* p = strstr(buf, search);
    if (!p) { dst[0] = '\0'; return 0; }
    p += strlen(search);
    const char* end = strchr(p, '\n');
    int len = end ? (int)(end - p) : (int)strlen(p);
    if (len >= dst_len) len = dst_len - 1;
    memcpy(dst, p, len);
    dst[len] = '\0';
    return len;
}

// Count comma-separated items (0 for empty string)
static int count_csv(const char* s) {
    if (!s || s[0] == '\0') return 0;
    int count = 1;
    for (const char* p = s; *p; p++)
        if (*p == ',') count++;
    return count;
}

// -- Visible width helper ----------------------------------------------------
// Strip ANSI escape sequences from a string, return new string
static std::string strip_ansi(const char* s) {
    std::string result;
    while (*s) {
        if (*s == '\x1b') {
            s++;
            if (*s == '[') {
                s++;
                while (*s && !((unsigned char)*s >= 0x40 && (unsigned char)*s <= 0x7E)) s++;
                if (*s) s++;
            }
            continue;
        }
        result += *s++;
    }
    return result;
}

// Compute visible terminal column width of a string containing ANSI escapes.
// Uses POSIX wcswidth() which matches the system's Unicode width tables
// (same tables Ghostty terminal uses for rendering).
static int visible_width(const char* s) {
    static bool locale_set = false;
    if (!locale_set) { setlocale(LC_CTYPE, "C.UTF-8") || setlocale(LC_CTYPE, "C.utf8") || setlocale(LC_CTYPE, "en_US.UTF-8"); locale_set = true; }

    std::string clean = strip_ansi(s);

    // Convert UTF-8 to wide chars
    size_t wlen = mbstowcs(nullptr, clean.c_str(), 0);
    if (wlen == (size_t)-1) return (int)clean.length(); // fallback

    wchar_t* wbuf = new wchar_t[wlen + 1];
    mbstowcs(wbuf, clean.c_str(), wlen + 1);

    int w = wcswidth(wbuf, wlen);
    delete[] wbuf;

    return w >= 0 ? w : (int)clean.length();
}

// -- Path truncation ---------------------------------------------------------
// Truncates a file path for display. Replaces $HOME with ~.
// Never truncates before second directory component from start.
// Never truncates after second-to-last directory component.
// Ellipsis goes in the middle.
static std::string truncate_path(const std::string& path, int max_width) {
    // Replace home dir with ~
    std::string result = path;
    const char* home = getenv("HOME");
    if (home && home[0] != '\0') {
        size_t hlen = strlen(home);
        if (result.size() >= hlen && result.compare(0, hlen, home) == 0) {
            result = "~" + result.substr(hlen);
        }
    }

    int vis = (int)result.size(); // ASCII paths, 1 byte = 1 col
    if (vis <= max_width) return result;

    // Split into components
    std::vector<std::string> parts;
    size_t pos = 0;
    // Handle leading ~ or /
    std::string prefix;
    if (result[0] == '~') {
        prefix = "~";
        pos = 1;
        if (pos < result.size() && result[pos] == '/') { prefix += '/'; pos++; }
    } else if (result[0] == '/') {
        prefix = "/";
        pos = 1;
    }

    while (pos < result.size()) {
        size_t slash = result.find('/', pos);
        if (slash == std::string::npos) {
            parts.push_back(result.substr(pos));
            break;
        }
        parts.push_back(result.substr(pos, slash - pos));
        pos = slash + 1;
    }

    if (parts.size() <= 3) {
        // Too few parts to truncate middle; just hard-truncate
        if ((int)result.size() > max_width && max_width > 3) {
            return result.substr(0, max_width - 1) + "\xe2\x80\xa6"; // …
        }
        return result;
    }

    // Keep first component after prefix and last 2 components. Ellipsis replaces middle.
    // Try keeping more from each end until it fits.
    // Strategy: keep first N components + … + last M components
    // Start with N=1, M=2 (minimum). If still too wide, reduce M to 1.

    auto build = [&](int keep_front, int keep_back) -> std::string {
        std::string s = prefix;
        for (int i = 0; i < keep_front && i < (int)parts.size(); i++) {
            if (i > 0) s += '/';
            s += parts[i];
        }
        s += "/\xe2\x80\xa6"; // /…
        for (int i = (int)parts.size() - keep_back; i < (int)parts.size(); i++) {
            s += '/';
            s += parts[i];
        }
        return s;
    };

    // Try expanding from minimal to maximal
    std::string best = build(1, 2);
    if ((int)best.size() <= max_width) {
        // Try adding more from front
        for (int f = 2; f < (int)parts.size() - 2; f++) {
            std::string candidate = build(f, 2);
            if ((int)candidate.size() <= max_width)
                best = candidate;
            else
                break;
        }
        return best;
    }

    // Still too wide, try 1 front + 1 back
    best = build(1, 1);
    if ((int)best.size() <= max_width) return best;

    // Last resort: just filename with ellipsis prefix
    const std::string& filename = parts.back();
    if ((int)filename.size() + 2 <= max_width) {
        return "\xe2\x80\xa6/" + filename; // …/filename
    }

    // Absolute last resort: truncate filename
    if (max_width > 3) {
        return result.substr(0, max_width - 1) + "\xe2\x80\xa6";
    }
    return result.substr(0, max_width);
}

// -- Formatting helpers ------------------------------------------------------

// Compute elapsed from epoch string "SEC.NSEC" to now, format as "NN.Ns"
static int format_elapsed(char* buf, int buf_len, const char* start_epoch) {
    if (!start_epoch || start_epoch[0] == '\0')
        return snprintf(buf, buf_len, "?s");

    long start_sec = strtol(start_epoch, nullptr, 10);
    long start_nsec = 0;
    const char* dot = strchr(start_epoch, '.');
    if (dot) start_nsec = strtol(dot + 1, nullptr, 10);

    struct timespec now;
    clock_gettime(CLOCK_REALTIME, &now);

    long sec_diff = now.tv_sec - start_sec;
    long nsec_diff = now.tv_nsec - start_nsec;
    if (nsec_diff < 0) { sec_diff--; nsec_diff += 1000000000L; }
    if (sec_diff < 0) { sec_diff = 0; nsec_diff = 0; }

    int tenths = (int)(nsec_diff / 100000000L);

    if (sec_diff >= 3600) {
        int h = (int)(sec_diff / 3600);
        int m = (int)((sec_diff % 3600) / 60);
        return snprintf(buf, buf_len, "%dh %dm", h, m);
    } else if (sec_diff >= 60) {
        int m = (int)(sec_diff / 60);
        int s = (int)(sec_diff % 60);
        return snprintf(buf, buf_len, "%dm %ds", m, s);
    } else {
        return snprintf(buf, buf_len, "%ld.%ds", sec_diff, tenths);
    }
}

// Format elapsed for compact display (no spaces): "42s" or "1m12s" or "1h5m"
static int format_elapsed_compact(char* buf, int buf_len, const char* start_epoch) {
    if (!start_epoch || start_epoch[0] == '\0')
        return snprintf(buf, buf_len, "?s");

    long start_sec = strtol(start_epoch, nullptr, 10);
    long start_nsec = 0;
    const char* dot = strchr(start_epoch, '.');
    if (dot) start_nsec = strtol(dot + 1, nullptr, 10);

    struct timespec now;
    clock_gettime(CLOCK_REALTIME, &now);

    long sec_diff = now.tv_sec - start_sec;
    long nsec_diff = now.tv_nsec - start_nsec;
    if (nsec_diff < 0) { sec_diff--; nsec_diff += 1000000000L; }
    if (sec_diff < 0) { sec_diff = 0; nsec_diff = 0; }

    if (sec_diff >= 3600) {
        int h = (int)(sec_diff / 3600);
        int m = (int)((sec_diff % 3600) / 60);
        return snprintf(buf, buf_len, "%dh%dm", h, m);
    } else if (sec_diff >= 60) {
        int m = (int)(sec_diff / 60);
        int s = (int)(sec_diff % 60);
        return snprintf(buf, buf_len, "%dm%ds", m, s);
    } else {
        return snprintf(buf, buf_len, "%lds", sec_diff);
    }
}

// Format token count: <1000 -> "N", >=1000 -> "NK", >=1000000 -> "N.NM"
static int format_tokens(char* buf, int buf_len, int n) {
    if (n >= 1000000) {
        int whole = n / 1000000;
        int frac = (n % 1000000) / 100000;
        return snprintf(buf, buf_len, "%d.%dM", whole, frac);
    } else if (n >= 1000) {
        return snprintf(buf, buf_len, "%dK", n / 1000);
    } else {
        return snprintf(buf, buf_len, "%d", n);
    }
}

// Format timestamp variants
// Full: "Sun 02-22::10:35:05"
static int format_timestamp_full(char* buf, int buf_len) {
    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);
    time_t t = ts.tv_sec - 6 * 3600; // CST = UTC-6
    struct tm tm;
    gmtime_r(&t, &tm);
    return (int)strftime(buf, buf_len, "%a %m-%d::%H:%M:%S", &tm);
}

// Medium: "02-22::10:35" (no day name, no seconds)
static int format_timestamp_medium(char* buf, int buf_len) {
    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);
    time_t t = ts.tv_sec - 6 * 3600;
    struct tm tm;
    gmtime_r(&t, &tm);
    return (int)strftime(buf, buf_len, "%m-%d::%H:%M", &tm);
}

// Narrow: "10:35:05" (time only with seconds)
static int format_timestamp_narrow(char* buf, int buf_len) {
    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);
    time_t t = ts.tv_sec - 6 * 3600;
    struct tm tm;
    gmtime_r(&t, &tm);
    return (int)strftime(buf, buf_len, "%H:%M:%S", &tm);
}

// Tiny: "10:35" (time only, no seconds)
static int format_timestamp_tiny(char* buf, int buf_len) {
    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);
    time_t t = ts.tv_sec - 6 * 3600;
    struct tm tm;
    gmtime_r(&t, &tm);
    return (int)strftime(buf, buf_len, "%H:%M", &tm);
}

// -- JSON escape -------------------------------------------------------------
// Escapes for JSON string output. ALL control characters (0x00-0x1f) including
// ESC (0x1b) are escaped as \uXXXX per the JSON spec. Backslash and double-quote
// are also escaped. Printable ASCII and multi-byte UTF-8 pass through unchanged.
static int json_escape(const char* src, char* dst, int dst_len) {
    int j = 0;
    for (int i = 0; src[i] && j < dst_len - 6; i++) {
        unsigned char c = (unsigned char)src[i];
        if (c == '"') {
            dst[j++] = '\\'; dst[j++] = '"';
        } else if (c == '\\') {
            dst[j++] = '\\'; dst[j++] = '\\';
        } else if (c == '\n') {
            dst[j++] = '\\'; dst[j++] = 'n';
        } else if (c == '\r') {
            dst[j++] = '\\'; dst[j++] = 'r';
        } else if (c == '\t') {
            dst[j++] = '\\'; dst[j++] = 't';
        } else if (c < 0x20) {
            // Control char (including ESC 0x1b) -- encode as \uXXXX
            j += snprintf(dst + j, dst_len - j, "\\u%04x", c);
        } else {
            dst[j++] = (char)c;
        }
    }
    dst[j] = '\0';
    return j;
}

// -- ANSI builder helpers ----------------------------------------------------
// These write raw ESC bytes into the content buffer.

// Append raw string
static int a_str(char* buf, int pos, int cap, const char* s) {
    int len = (int)strlen(s);
    if (pos + len >= cap) return pos;
    memcpy(buf + pos, s, len);
    return pos + len;
}

// Append ANSI SGR sequence: ESC [ params m
static int a_sgr(char* buf, int pos, int cap, const char* params) {
    int need = 2 + (int)strlen(params) + 1; // ESC [ params m
    if (pos + need >= cap) return pos;
    buf[pos++] = ESC;
    buf[pos++] = '[';
    int plen = (int)strlen(params);
    memcpy(buf + pos, params, plen);
    pos += plen;
    buf[pos++] = 'm';
    return pos;
}

// Append a single character
static int a_ch(char* buf, int pos, int cap, char c) {
    if (pos + 1 >= cap) return pos;
    buf[pos++] = c;
    return pos;
}

// Append N copies of a character
static int a_fill(char* buf, int pos, int cap, char c, int n) {
    for (int i = 0; i < n && pos < cap - 1; i++)
        buf[pos++] = c;
    return pos;
}

// Append integer as decimal string
static int a_int(char* buf, int pos, int cap, int val) {
    char tmp[32];
    snprintf(tmp, sizeof(tmp), "%d", val);
    return a_str(buf, pos, cap, tmp);
}

// -- Terminal width from ancestor process PTY ---------------------------------
// Hooks run with piped stdin/stdout/stderr — no direct TTY access.
// Walk the process tree via /proc to find the parent Claude Code process's PTY
// and read its terminal width via ioctl.
static int get_terminal_width_from_ancestors() {
    pid_t pid = getpid();
    char path[256];

    for (int depth = 0; depth < 10; depth++) {
        snprintf(path, sizeof(path), "/proc/%d/stat", (int)pid);
        int fd = open(path, O_RDONLY);
        if (fd < 0) break;
        char stat_buf[512];
        ssize_t n = read(fd, stat_buf, sizeof(stat_buf) - 1);
        close(fd);
        if (n <= 0) break;
        stat_buf[n] = '\0';

        // Parse: pid (comm) state ppid ...
        // Find closing ')' to skip comm (may contain spaces)
        const char* cp = strrchr(stat_buf, ')');
        if (!cp || !cp[1] || !cp[2]) break;
        // After ')' comes: " state ppid ..."
        int ppid = 0;
        char state;
        if (sscanf(cp + 2, "%c %d", &state, &ppid) != 2 || ppid <= 1) break;

        // Try opening the ancestor's stdin fd
        snprintf(path, sizeof(path), "/proc/%d/fd/0", ppid);
        int tty_fd = open(path, O_RDONLY);
        if (tty_fd >= 0) {
            struct winsize w;
            if (ioctl(tty_fd, TIOCGWINSZ, &w) == 0 && w.ws_col > 0) {
                close(tty_fd);
                return w.ws_col;
            }
            close(tty_fd);
        }

        pid = ppid;
    }
    return 0;
}

// -- Width tier classification -----------------------------------------------
enum WidthTier { TIER_TINY, TIER_NARROW, TIER_MEDIUM, TIER_FULL };

static WidthTier classify_width(int cols) {
    if (cols < 40) return TIER_TINY;
    if (cols < 55) return TIER_NARROW;
    if (cols < 70) return TIER_MEDIUM;
    return TIER_FULL;
}

// -- Main --------------------------------------------------------------------

int main() {
    // Read stdin JSON
    char input[4096];
    int input_len = 0;
    while (input_len < (int)sizeof(input) - 1) {
        ssize_t n = read(STDIN_FILENO, input + input_len, sizeof(input) - 1 - input_len);
        if (n <= 0) break;
        input_len += n;
    }
    input[input_len] = '\0';

    // Extract session_id
    char session_id[256] = "";
    json_extract(input, input_len, "session_id", session_id, sizeof(session_id));
    if (session_id[0] == '\0') return 0;

    // -- Read all SHM files --------------------------------------------------
    char path[512];
    char turn_id[256] = "";
    char start_epoch[64] = "";
    char stats_buf[512] = "";
    char session_name[256] = "";
    char topics[512] = "";
    char turn_saved[16] = "";
    char tokens_buf[256] = "";
    char turn_number_str[32] = "";
    char session_number_str[32] = "";
    char agent_turn_total_str[32] = "";

    shm_path(path, sizeof(path), "turn_id", session_id);
    read_shm(path, turn_id, sizeof(turn_id));
    shm_path(path, sizeof(path), "turn_start_epoch", session_id);
    read_shm(path, start_epoch, sizeof(start_epoch));
    shm_path(path, sizeof(path), "turn_stats", session_id);
    read_shm(path, stats_buf, sizeof(stats_buf));
    shm_path(path, sizeof(path), "turn_session_name", session_id);
    read_shm(path, session_name, sizeof(session_name));
    shm_path(path, sizeof(path), "turn_topics", session_id);
    read_shm(path, topics, sizeof(topics));
    // turn_saved: written by save_session (also a Stop hook). Race condition possible —
    // save_session may not have finished the PG write + SHM write yet. Poll briefly.
    // Hook ordering: save_session runs before turn_summary, but Claude Code may run
    // hooks concurrently. save_session can take up to 5s (its timeout). Poll up to 1.5s.
    shm_path(path, sizeof(path), "turn_saved", session_id);
    read_shm(path, turn_saved, sizeof(turn_saved));
    if (turn_saved[0] != '1') {
        // Poll up to 1500ms in 50ms increments for save_session to finish
        for (int retry = 0; retry < 30 && turn_saved[0] != '1'; retry++) {
            usleep(50000); // 50ms
            read_shm(path, turn_saved, sizeof(turn_saved));
        }
    }
    shm_path(path, sizeof(path), "turn_tokens", session_id);
    read_shm(path, tokens_buf, sizeof(tokens_buf));
    shm_path(path, sizeof(path), "turn_number", session_id);
    read_shm(path, turn_number_str, sizeof(turn_number_str));
    shm_path(path, sizeof(path), "turn_session_number", session_id);
    read_shm(path, session_number_str, sizeof(session_number_str));
    shm_path(path, sizeof(path), "agent_turn_total", session_id);
    read_shm(path, agent_turn_total_str, sizeof(agent_turn_total_str));

    // Read edit files SHM (turn_edit_files.{first8})
    char edit_files_buf[4096] = "";
    shm_path(path, sizeof(path), "turn_edit_files", session_id);
    read_shm(path, edit_files_buf, sizeof(edit_files_buf));

    // -- Parse data ----------------------------------------------------------
    int total_calls = parse_stat_int(stats_buf, "total");
    int agents = parse_stat_int(stats_buf, "agents");
    int edits = parse_stat_int(stats_buf, "edits");
    char tools_list[256] = "";
    parse_stat_str(stats_buf, "tools", tools_list, sizeof(tools_list));
    int unique_tools = count_csv(tools_list);

    int total_out = parse_stat_int(tokens_buf, "total_out");
    int agent_out = parse_stat_int(tokens_buf, "agent_out");
    int context_pct = parse_stat_int(tokens_buf, "context_pct");
    (void)agent_out; // suppress unused warning

    int turn_number = atoi(turn_number_str);
    int session_number = atoi(session_number_str);
    int agent_turn_total = atoi(agent_turn_total_str);
    int global_turn = turn_number + agent_turn_total;

    // Compute deterministic agent turn ID: hash(session_id + ":" + agent_turn_total)
    unsigned int agent_hash = 0;
    {
        char agent_key[512];
        snprintf(agent_key, sizeof(agent_key), "%s:%d", session_id, agent_turn_total);
        for (int i = 0; agent_key[i]; i++)
            agent_hash = agent_hash * 31 + (unsigned char)agent_key[i];
    }
    char agent_turn_id[9];
    snprintf(agent_turn_id, sizeof(agent_turn_id), "%08x", agent_hash);

    char elapsed[64] = "";
    format_elapsed(elapsed, sizeof(elapsed), start_epoch);

    char elapsed_compact[64] = "";
    format_elapsed_compact(elapsed_compact, sizeof(elapsed_compact), start_epoch);

    // -- Get terminal width --------------------------------------------------
    int term_cols = 0;
    {
        struct winsize w;
        if (ioctl(STDERR_FILENO, TIOCGWINSZ, &w) == 0 && w.ws_col > 0) {
            term_cols = w.ws_col;
        } else if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &w) == 0 && w.ws_col > 0) {
            term_cols = w.ws_col;
        } else {
            int tty_fd = open("/dev/tty", O_RDONLY);
            if (tty_fd >= 0) {
                if (ioctl(tty_fd, TIOCGWINSZ, &w) == 0 && w.ws_col > 0)
                    term_cols = w.ws_col;
                close(tty_fd);
            }
        }
        if (term_cols <= 0) {
            const char* cols_env = getenv("COLUMNS");
            if (cols_env && cols_env[0] != '\0') {
                int env_cols = atoi(cols_env);
                if (env_cols > 0) term_cols = env_cols;
            }
        }
        if (term_cols <= 0) {
            term_cols = get_terminal_width_from_ancestors();
        }
        if (term_cols <= 0) term_cols = 80; // conservative fallback
    term_cols -= 6; // Claude Code indents hook systemMessage output by ~5-6 cols
    }

    // DEBUG: write detected term_cols to SHM for diagnostics
    {
        char debug_path[256];
        shm_path(debug_path, sizeof(debug_path), "turn_debug_cols", session_id);
        FILE* f = fopen(debug_path, "w");
        if (f) { fprintf(f, "%d", term_cols); fclose(f); }
    }

    WidthTier tier = classify_width(term_cols);

    // -- Build ANSI content with raw ESC bytes -------------------------------
    char raw[16384];
    int p = 0;
    const int cap = (int)sizeof(raw);

    // -- Edit files section (prepended before summary if edits exist) --------
    if (edit_files_buf[0] != '\0') {
        // Leading blank line before edit list
        p = a_ch(raw, p, cap, '\n');
        // Parse edit_files_buf: one line per file "path:lines_edited"
        const char* cursor = edit_files_buf;
        while (*cursor) {
            const char* line_end = strchr(cursor, '\n');
            int line_len = line_end ? (int)(line_end - cursor) : (int)strlen(cursor);
            if (line_len == 0) { cursor = line_end ? line_end + 1 : cursor + line_len; continue; }

            char file_line[1024];
            if (line_len >= (int)sizeof(file_line)) line_len = (int)sizeof(file_line) - 1;
            memcpy(file_line, cursor, line_len);
            file_line[line_len] = '\0';

            // Split on last ':'
            char* colon = strrchr(file_line, ':');
            int lines_edited = 0;
            std::string filepath;
            if (colon && colon != file_line) {
                *colon = '\0';
                lines_edited = atoi(colon + 1);
                filepath = file_line;
            } else {
                filepath = file_line;
            }

            // Build the line: ✏️ truncated_path (+N lines)
            char suffix[64];
            snprintf(suffix, sizeof(suffix), " (+%d lines)", lines_edited);
            int suffix_vis = (int)strlen(suffix);
            // emoji ✏️ = 2 cols + space = 3 cols prefix
            int prefix_vis = 3;
            int max_path_width = term_cols - prefix_vis - suffix_vis;
            if (max_path_width < 10) max_path_width = 10;

            std::string trunc = truncate_path(filepath, max_path_width);

            p = a_str(raw, p, cap, "\xe2\x9c\x8f\xef\xb8\x8f "); // ✏️ + space
            p = a_sgr(raw, p, cap, "38;2;255;200;60"); // warm yellow
            p = a_str(raw, p, cap, trunc.c_str());
            p = a_sgr(raw, p, cap, "0");
            p = a_sgr(raw, p, cap, "2"); // dim
            p = a_str(raw, p, cap, suffix);
            p = a_sgr(raw, p, cap, "0");
            p = a_ch(raw, p, cap, '\n');

            cursor = line_end ? line_end + 1 : cursor + line_len;
        }
        // Blank line after edit list before summary
        p = a_ch(raw, p, cap, '\n');
    }

    // Line 0: leading newline (break after "Stop says:")
    p = a_ch(raw, p, cap, '\n');

    // -- Line 1: Identity -- width-adaptive ----------------------------------
    {
        char line1[2048];
        int l1 = 0;
        const int l1cap = (int)sizeof(line1);

        auto build_line1 = [&](WidthTier t) {
            l1 = 0;
            char sid8[9]; snprintf(sid8, sizeof(sid8), "%.8s", session_id);
            if (t == TIER_TINY) {
                // S#1924·320a6835│ST#21│AT#16·abcd1234  (no emojis, no spaces, full 8-char IDs)
                l1 = a_sgr(line1, l1, l1cap, "1;37");
                l1 = a_str(line1, l1, l1cap, "S#");
                l1 = a_int(line1, l1, l1cap, session_number);
                l1 = a_sgr(line1, l1, l1cap, "0");
                l1 = a_str(line1, l1, l1cap, "\xC2\xB7"); // ·
                l1 = a_sgr(line1, l1, l1cap, "2");
                l1 = a_str(line1, l1, l1cap, sid8);
                l1 = a_sgr(line1, l1, l1cap, "0");
                l1 = a_sgr(line1, l1, l1cap, "38;2;0;220;255");
                l1 = a_str(line1, l1, l1cap, "\xE2\x94\x82"); // │
                l1 = a_sgr(line1, l1, l1cap, "0");
                l1 = a_sgr(line1, l1, l1cap, "1;37");
                l1 = a_str(line1, l1, l1cap, "ST#");
                l1 = a_int(line1, l1, l1cap, global_turn);
                l1 = a_sgr(line1, l1, l1cap, "0");
                l1 = a_sgr(line1, l1, l1cap, "38;2;0;220;255");
                l1 = a_str(line1, l1, l1cap, "\xE2\x94\x82"); // │
                l1 = a_sgr(line1, l1, l1cap, "0");
                l1 = a_sgr(line1, l1, l1cap, "1;37");
                l1 = a_str(line1, l1, l1cap, "AT#");
                l1 = a_int(line1, l1, l1cap, agent_turn_total);
                l1 = a_sgr(line1, l1, l1cap, "0");
                l1 = a_str(line1, l1, l1cap, "\xC2\xB7"); // ·
                l1 = a_sgr(line1, l1, l1cap, "2");
                l1 = a_str(line1, l1, l1cap, agent_turn_id);
                l1 = a_sgr(line1, l1, l1cap, "0");
            } else if (t == TIER_NARROW) {
                // 📄S#1924·320a6835│🌐ST#21│⚡️AT#16·abcd1234  (drop spaces, keep emojis + full IDs)
                l1 = a_str(line1, l1, l1cap, "\xf0\x9f\x93\x84"); // 📄 (no trailing space)
                l1 = a_sgr(line1, l1, l1cap, "1;37");
                l1 = a_str(line1, l1, l1cap, "S#");
                l1 = a_int(line1, l1, l1cap, session_number);
                l1 = a_sgr(line1, l1, l1cap, "0");
                l1 = a_str(line1, l1, l1cap, "\xC2\xB7"); // ·
                l1 = a_sgr(line1, l1, l1cap, "2");
                l1 = a_str(line1, l1, l1cap, sid8);
                l1 = a_sgr(line1, l1, l1cap, "0");
                l1 = a_sgr(line1, l1, l1cap, "38;2;0;220;255");
                l1 = a_str(line1, l1, l1cap, "\xE2\x94\x82"); // │
                l1 = a_sgr(line1, l1, l1cap, "0");
                l1 = a_str(line1, l1, l1cap, "\xf0\x9f\x8c\x90"); // 🌐 (no trailing space)
                l1 = a_sgr(line1, l1, l1cap, "1;37");
                l1 = a_str(line1, l1, l1cap, "ST#");
                l1 = a_int(line1, l1, l1cap, global_turn);
                l1 = a_sgr(line1, l1, l1cap, "0");
                l1 = a_sgr(line1, l1, l1cap, "38;2;0;220;255");
                l1 = a_str(line1, l1, l1cap, "\xE2\x94\x82"); // │
                l1 = a_sgr(line1, l1, l1cap, "0");
                l1 = a_str(line1, l1, l1cap, "\xe2\x9a\xa1\xef\xb8\x8f"); // ⚡️ (with VS16, no trailing space)
                l1 = a_sgr(line1, l1, l1cap, "1;37");
                l1 = a_str(line1, l1, l1cap, "AT#");
                l1 = a_int(line1, l1, l1cap, agent_turn_total);
                l1 = a_sgr(line1, l1, l1cap, "0");
                l1 = a_str(line1, l1, l1cap, "\xC2\xB7"); // ·
                l1 = a_sgr(line1, l1, l1cap, "2");
                l1 = a_str(line1, l1, l1cap, agent_turn_id);
                l1 = a_sgr(line1, l1, l1cap, "0");
            } else {
                // FULL (also used for MEDIUM): 📄 S#1924 · 320a6835 │ 🌐 ST#21 │ ⚡ AT#16 · abcd1234
                l1 = a_str(line1, l1, l1cap, "\xf0\x9f\x93\x84 "); // 📄
                l1 = a_sgr(line1, l1, l1cap, "1;37");
                l1 = a_str(line1, l1, l1cap, "S#");
                l1 = a_int(line1, l1, l1cap, session_number);
                l1 = a_sgr(line1, l1, l1cap, "0");
                l1 = a_str(line1, l1, l1cap, " \xC2\xB7 "); // " · "
                l1 = a_sgr(line1, l1, l1cap, "2");
                l1 = a_str(line1, l1, l1cap, sid8);
                l1 = a_sgr(line1, l1, l1cap, "0");
                l1 = a_str(line1, l1, l1cap, " ");
                l1 = a_sgr(line1, l1, l1cap, "38;2;0;220;255");
                l1 = a_str(line1, l1, l1cap, "\xE2\x94\x82"); // │
                l1 = a_sgr(line1, l1, l1cap, "0");
                l1 = a_str(line1, l1, l1cap, " ");
                l1 = a_str(line1, l1, l1cap, "\xf0\x9f\x8c\x90 "); // 🌐
                l1 = a_sgr(line1, l1, l1cap, "1;37");
                l1 = a_str(line1, l1, l1cap, "ST#");
                l1 = a_int(line1, l1, l1cap, global_turn);
                l1 = a_sgr(line1, l1, l1cap, "0");
                l1 = a_str(line1, l1, l1cap, " ");
                l1 = a_sgr(line1, l1, l1cap, "38;2;0;220;255");
                l1 = a_str(line1, l1, l1cap, "\xE2\x94\x82"); // │
                l1 = a_sgr(line1, l1, l1cap, "0");
                l1 = a_str(line1, l1, l1cap, " ");
                l1 = a_str(line1, l1, l1cap, "\xe2\x9a\xa1\xef\xb8\x8f "); // ⚡️ (with VS16)
                l1 = a_sgr(line1, l1, l1cap, "1;37");
                l1 = a_str(line1, l1, l1cap, "AT#");
                l1 = a_int(line1, l1, l1cap, agent_turn_total);
                l1 = a_sgr(line1, l1, l1cap, "0");
                l1 = a_str(line1, l1, l1cap, " \xC2\xB7 "); // " · "
                l1 = a_sgr(line1, l1, l1cap, "2");
                l1 = a_str(line1, l1, l1cap, agent_turn_id);
                l1 = a_sgr(line1, l1, l1cap, "0");
            }
            line1[l1] = '\0';
        };

        // Try from current tier down to TINY
        WidthTier try_tier = tier;
        build_line1(try_tier);
        while (visible_width(line1) > term_cols && try_tier > TIER_TINY) {
            try_tier = (WidthTier)((int)try_tier - 1);
            build_line1(try_tier);
        }

        p = a_str(raw, p, cap, line1);
        p = a_ch(raw, p, cap, '\n');
    }

    // -- Line 2: Session name (left) + tags (right-aligned) ------------------
    // INVARIANT: visible_width(line2) <= term_cols. Never wrap.
    {
        // Session name portion: 💬 + space + name
        // emoji visible width = 2, space = 1 => prefix_vis = 3
        const int prefix_vis = 3;

        // Compute tags text and its visible width
        // Drop tags entirely at TINY tier
        char topics_text[512] = "";
        int topics_text_len = 0;
        if (topics[0] != '\0' && tier != TIER_TINY) {
            // Copy topics so we can truncate by removing trailing items
            strncpy(topics_text, topics, sizeof(topics_text) - 1);
            topics_text[sizeof(topics_text) - 1] = '\0';
            topics_text_len = (int)strlen(topics_text);
        }

        // tags_vis = emoji(2) + space(1) + topic text length (if topics present)
        int tags_vis = (topics_text_len > 0) ? (3 + topics_text_len) : 0;
        // min_gap: minimum space between name and tags
        const int min_gap = 2;

        // Determine session name source
        const char* name_src = (session_name[0] != '\0') ? session_name : "unnamed session";
        int name_src_len = (int)strlen(name_src);

        // Compute available width for name:
        // total = prefix_vis + name_len + gap + tags_vis <= term_cols
        // name_len <= term_cols - prefix_vis - tags_vis - gap
        int avail_for_name = term_cols - prefix_vis;
        if (tags_vis > 0) avail_for_name -= (tags_vis + min_gap);
        if (avail_for_name < 3) avail_for_name = 3; // absolute minimum "..."

        // Truncate name if needed
        char name_display[256];
        int name_display_len;
        if (name_src_len <= avail_for_name) {
            memcpy(name_display, name_src, name_src_len);
            name_display[name_src_len] = '\0';
            name_display_len = name_src_len;
        } else {
            int trunc_len = avail_for_name - 3; // room for "..."
            if (trunc_len < 0) trunc_len = 0;
            memcpy(name_display, name_src, trunc_len);
            name_display[trunc_len] = '\0';
            strcat(name_display, "...");
            name_display_len = trunc_len + 3;
        }

        // Now check: does the full line fit? (prefix + name + gap + tags <= term_cols)
        // If name is at minimum ("...") and still doesn't fit, truncate tags
        // by removing trailing comma-separated items
        if (tags_vis > 0) {
            int total_vis = prefix_vis + name_display_len + min_gap + tags_vis;
            while (total_vis > term_cols && topics_text_len > 0) {
                // If tags alone exceed 50% of width, start truncating tags
                // Also truncate if name is already at minimum and still overflows
                // Remove last comma-separated item from topics_text
                char* last_comma = strrchr(topics_text, ',');
                if (last_comma) {
                    *last_comma = '\0';
                    topics_text_len = (int)strlen(topics_text);
                    // Trim trailing space
                    while (topics_text_len > 0 && topics_text[topics_text_len - 1] == ' ') {
                        topics_text[--topics_text_len] = '\0';
                    }
                    tags_vis = 3 + topics_text_len;
                } else {
                    // Single tag remaining and still doesn't fit -- drop tags entirely
                    topics_text[0] = '\0';
                    topics_text_len = 0;
                    tags_vis = 0;
                }
                total_vis = prefix_vis + name_display_len + (tags_vis > 0 ? min_gap : 0) + tags_vis;

                // If dropping tags freed space, re-expand name
                if (tags_vis == 0) {
                    avail_for_name = term_cols - prefix_vis;
                    if (name_src_len <= avail_for_name) {
                        memcpy(name_display, name_src, name_src_len);
                        name_display[name_src_len] = '\0';
                        name_display_len = name_src_len;
                    } else {
                        int trunc_len = avail_for_name - 3;
                        if (trunc_len < 0) trunc_len = 0;
                        memcpy(name_display, name_src, trunc_len);
                        name_display[trunc_len] = '\0';
                        strcat(name_display, "...");
                        name_display_len = trunc_len + 3;
                    }
                }
            }
        }

        // Build tags ANSI string
        char tags_raw[512];
        int tp = 0;
        const int tcap = (int)sizeof(tags_raw);
        if (topics_text_len > 0) {
            tp = a_str(tags_raw, tp, tcap, "\xf0\x9f\x8f\xb7\xef\xb8\x8f "); // 🏷️ + space
            tp = a_sgr(tags_raw, tp, tcap, "38;2;0;220;255");
            tp = a_str(tags_raw, tp, tcap, topics_text);
            tp = a_sgr(tags_raw, tp, tcap, "0");
        }
        tags_raw[tp] = '\0';

        // Emit session name
        p = a_str(raw, p, cap, "\xf0\x9f\x92\xac "); // 💬 + space
        if (session_name[0] != '\0') {
            p = a_sgr(raw, p, cap, "1;38;2;0;220;255");
            p = a_str(raw, p, cap, name_display);
            p = a_sgr(raw, p, cap, "0");
        } else {
            p = a_sgr(raw, p, cap, "2;3");
            p = a_str(raw, p, cap, name_display);
            p = a_sgr(raw, p, cap, "0");
        }

        // Right-align tags with exact padding (never exceed term_cols)
        if (tags_vis > 0) {
            int gap = term_cols - prefix_vis - name_display_len - tags_vis;
            if (gap < min_gap) gap = min_gap;
            // Final safety: clamp gap so total doesn't exceed term_cols
            int total_check = prefix_vis + name_display_len + gap + tags_vis;
            if (total_check > term_cols) {
                gap = term_cols - prefix_vis - name_display_len - tags_vis;
                if (gap < 1) gap = 1;
            }
            p = a_fill(raw, p, cap, ' ', gap);
            p = a_str(raw, p, cap, tags_raw);
        }

        p = a_ch(raw, p, cap, '\n');
    }

    // -- Line 3: Stats (skip entirely if 0 calls) -- width-adaptive ----------
    if (total_calls > 0) {
        char line3[2048];
        int l3 = 0;
        const int l3cap = (int)sizeof(line3);

        auto build_line3 = [&](WidthTier t) {
            l3 = 0;
            if (t == TIER_TINY) {
                // 🔧17·🤖3·✏️2·⏱42s
                l3 = a_str(line3, l3, l3cap, "\xf0\x9f\x94\xa7"); // 🔧
                l3 = a_sgr(line3, l3, l3cap, "1;37");
                l3 = a_int(line3, l3, l3cap, total_calls);
                l3 = a_sgr(line3, l3, l3cap, "0");
                if (agents > 0) {
                    l3 = a_str(line3, l3, l3cap, "\xC2\xB7"); // ·
                    l3 = a_str(line3, l3, l3cap, "\xf0\x9f\xa4\x96"); // 🤖
                    l3 = a_sgr(line3, l3, l3cap, "1;37");
                    l3 = a_int(line3, l3, l3cap, agents);
                    l3 = a_sgr(line3, l3, l3cap, "0");
                }
                if (edits > 0) {
                    l3 = a_str(line3, l3, l3cap, "\xC2\xB7"); // ·
                    l3 = a_str(line3, l3, l3cap, "\xe2\x9c\x8f\xef\xb8\x8f"); // ✏️
                    l3 = a_sgr(line3, l3, l3cap, "1;37");
                    l3 = a_int(line3, l3, l3cap, edits);
                    l3 = a_sgr(line3, l3, l3cap, "0");
                }
                l3 = a_str(line3, l3, l3cap, "\xC2\xB7"); // ·
                l3 = a_str(line3, l3, l3cap, "\xe2\x8f\xb1"); // ⏱
                l3 = a_sgr(line3, l3, l3cap, "1;37");
                l3 = a_str(line3, l3, l3cap, elapsed_compact);
                l3 = a_sgr(line3, l3, l3cap, "0");
            } else if (t == TIER_NARROW) {
                // 🔧 17·🤖 3·✏️ 2·⏱ 42s  (no parenthetical, no labels)
                l3 = a_str(line3, l3, l3cap, "\xf0\x9f\x94\xa7 "); // 🔧
                l3 = a_sgr(line3, l3, l3cap, "1;37");
                l3 = a_int(line3, l3, l3cap, total_calls);
                l3 = a_sgr(line3, l3, l3cap, "0");
                if (agents > 0) {
                    l3 = a_str(line3, l3, l3cap, " \xC2\xB7 "); // " · "
                    l3 = a_str(line3, l3, l3cap, "\xf0\x9f\xa4\x96 "); // 🤖
                    l3 = a_sgr(line3, l3, l3cap, "1;37");
                    l3 = a_int(line3, l3, l3cap, agents);
                    l3 = a_sgr(line3, l3, l3cap, "0");
                }
                if (edits > 0) {
                    l3 = a_str(line3, l3, l3cap, " \xC2\xB7 "); // " · "
                    l3 = a_str(line3, l3, l3cap, "\xe2\x9c\x8f\xef\xb8\x8f "); // ✏️
                    l3 = a_sgr(line3, l3, l3cap, "1;37");
                    l3 = a_int(line3, l3, l3cap, edits);
                    l3 = a_sgr(line3, l3, l3cap, "0");
                }
                l3 = a_str(line3, l3, l3cap, " \xC2\xB7 "); // " · "
                l3 = a_str(line3, l3, l3cap, "\xe2\x8f\xb1 "); // ⏱
                l3 = a_sgr(line3, l3, l3cap, "1;37");
                l3 = a_str(line3, l3, l3cap, elapsed_compact);
                l3 = a_sgr(line3, l3, l3cap, "0");
            } else if (t == TIER_MEDIUM) {
                // 🔧 17 (8u) · 🤖 3 · ✏️ 2 · ⏱ 42s
                l3 = a_str(line3, l3, l3cap, "\xf0\x9f\x94\xa7 "); // 🔧
                l3 = a_sgr(line3, l3, l3cap, "1;37");
                l3 = a_int(line3, l3, l3cap, total_calls);
                l3 = a_sgr(line3, l3, l3cap, "0");
                if (unique_tools > 0) {
                    l3 = a_str(line3, l3, l3cap, " (");
                    l3 = a_sgr(line3, l3, l3cap, "1;37");
                    l3 = a_int(line3, l3, l3cap, unique_tools);
                    l3 = a_sgr(line3, l3, l3cap, "0");
                    l3 = a_str(line3, l3, l3cap, "u)");
                }
                if (agents > 0) {
                    l3 = a_str(line3, l3, l3cap, " \xC2\xB7 "); // " · "
                    l3 = a_str(line3, l3, l3cap, "\xf0\x9f\xa4\x96 "); // 🤖
                    l3 = a_sgr(line3, l3, l3cap, "1;37");
                    l3 = a_int(line3, l3, l3cap, agents);
                    l3 = a_sgr(line3, l3, l3cap, "0");
                }
                if (edits > 0) {
                    l3 = a_str(line3, l3, l3cap, " \xC2\xB7 "); // " · "
                    l3 = a_str(line3, l3, l3cap, "\xe2\x9c\x8f\xef\xb8\x8f "); // ✏️
                    l3 = a_sgr(line3, l3, l3cap, "1;37");
                    l3 = a_int(line3, l3, l3cap, edits);
                    l3 = a_sgr(line3, l3, l3cap, "0");
                }
                l3 = a_str(line3, l3, l3cap, " \xC2\xB7 "); // " · "
                l3 = a_str(line3, l3, l3cap, "\xe2\x8f\xb1 "); // ⏱
                l3 = a_sgr(line3, l3, l3cap, "1;37");
                l3 = a_str(line3, l3, l3cap, elapsed_compact);
                l3 = a_sgr(line3, l3, l3cap, "0");
            } else {
                // FULL: 🔧 17 calls (8 unique) · 🤖 3 agents · ✏️ 2 edits · ⏱ 42.3s
                l3 = a_str(line3, l3, l3cap, "\xf0\x9f\x94\xa7 "); // 🔧
                l3 = a_sgr(line3, l3, l3cap, "1;37");
                l3 = a_int(line3, l3, l3cap, total_calls);
                l3 = a_sgr(line3, l3, l3cap, "0");
                l3 = a_str(line3, l3, l3cap, " calls");
                if (unique_tools > 0) {
                    l3 = a_str(line3, l3, l3cap, " (");
                    l3 = a_sgr(line3, l3, l3cap, "1;37");
                    l3 = a_int(line3, l3, l3cap, unique_tools);
                    l3 = a_sgr(line3, l3, l3cap, "0");
                    l3 = a_str(line3, l3, l3cap, " unique)");
                }
                if (agents > 0) {
                    l3 = a_str(line3, l3, l3cap, " \xC2\xB7 "); // " · "
                    l3 = a_str(line3, l3, l3cap, "\xf0\x9f\xa4\x96 "); // 🤖
                    l3 = a_sgr(line3, l3, l3cap, "1;37");
                    l3 = a_int(line3, l3, l3cap, agents);
                    l3 = a_sgr(line3, l3, l3cap, "0");
                    l3 = a_str(line3, l3, l3cap, agents == 1 ? " agent" : " agents");
                }
                if (edits > 0) {
                    l3 = a_str(line3, l3, l3cap, " \xC2\xB7 "); // " · "
                    l3 = a_str(line3, l3, l3cap, "\xe2\x9c\x8f\xef\xb8\x8f "); // ✏️
                    l3 = a_sgr(line3, l3, l3cap, "1;37");
                    l3 = a_int(line3, l3, l3cap, edits);
                    l3 = a_sgr(line3, l3, l3cap, "0");
                    l3 = a_str(line3, l3, l3cap, edits == 1 ? " edit" : " edits");
                }
                l3 = a_str(line3, l3, l3cap, " \xC2\xB7 "); // " · "
                l3 = a_str(line3, l3, l3cap, "\xe2\x8f\xb1 "); // ⏱
                l3 = a_sgr(line3, l3, l3cap, "1;37");
                l3 = a_str(line3, l3, l3cap, elapsed);
                l3 = a_sgr(line3, l3, l3cap, "0");
            }
            line3[l3] = '\0';
        };

        // Try from current tier down to TINY
        WidthTier try_tier = tier;
        build_line3(try_tier);
        while (visible_width(line3) > term_cols && try_tier > TIER_TINY) {
            try_tier = (WidthTier)((int)try_tier - 1);
            build_line3(try_tier);
        }

        p = a_str(raw, p, cap, line3);
        p = a_ch(raw, p, cap, '\n');
    }

    // -- Line 4: Status line -- THREE-SWATCH design, width-adaptive ----------
    {
        // Determine content for each zone based on tier
        // LEFT: save badge
        char left_raw[512];
        int lp = 0;
        const int lcap = (int)sizeof(left_raw);
        int left_vis = 0;

        bool saved = (turn_saved[0] == '1');

        auto build_left = [&](WidthTier t) {
            lp = 0;
            left_vis = 0;
            if (t == TIER_TINY) {
                // [Saved] or [!!]
                if (saved) {
                    lp = a_sgr(left_raw, lp, lcap, "48;2;30;120;50");
                    lp = a_sgr(left_raw, lp, lcap, "38;2;255;255;255");
                    lp = a_str(left_raw, lp, lcap, " Saved ");
                    left_vis = 7;
                } else {
                    lp = a_sgr(left_raw, lp, lcap, "48;2;140;50;30");
                    lp = a_sgr(left_raw, lp, lcap, "38;2;220;160;120");
                    lp = a_str(left_raw, lp, lcap, " !! ");
                    left_vis = 4;
                }
            } else {
                // Full/Medium/Narrow all get emoji version
                if (saved) {
                    lp = a_sgr(left_raw, lp, lcap, "48;2;30;120;50");
                    lp = a_sgr(left_raw, lp, lcap, "38;2;255;255;255");
                    lp = a_str(left_raw, lp, lcap, " \xf0\x9f\x92\xbe Saved "); // " 💾 Saved "
                    left_vis = 10; // sp(1) + 💾(2) + sp(1) + "Saved"(5) + sp(1)
                } else {
                    lp = a_sgr(left_raw, lp, lcap, "48;2;140;50;30");
                    lp = a_sgr(left_raw, lp, lcap, "38;2;220;160;120");
                    lp = a_str(left_raw, lp, lcap, " \xe2\x9a\xa0 Not saved "); // " ⚠ Not saved "
                    left_vis = 14;
                }
            }
            left_raw[lp] = '\0';
        };

        // MIDDLE: token/context info
        char mid_raw[512];
        int mp_zone = 0;
        const int mcap = (int)sizeof(mid_raw);
        int mid_vis = 0;

        auto build_mid = [&](WidthTier t) {
            mp_zone = 0;
            mid_vis = 0;
            char mid_text[128];
            int mt = 0;

            if (total_out > 0) {
                char tok_fmt[32];
                format_tokens(tok_fmt, sizeof(tok_fmt), total_out);
                int remaining = 100 - context_pct;
                if (remaining < 0) remaining = 0;

                if (t == TIER_TINY) {
                    // Just token count
                    mt = snprintf(mid_text, sizeof(mid_text), "%s", tok_fmt);
                } else if (t == TIER_NARROW) {
                    // NK/200K (no tok/ctx labels)
                    mt = snprintf(mid_text, sizeof(mid_text), "%s/200K", tok_fmt);
                } else if (t == TIER_MEDIUM) {
                    // NK/200K·N%ctx
                    mt = snprintf(mid_text, sizeof(mid_text), "%s/200K\xC2\xB7%d%%ctx", tok_fmt, remaining);
                } else {
                    // Full: NK/200K tok · N% ctx
                    mt = snprintf(mid_text, sizeof(mid_text), "%s/200K tok \xC2\xB7 %d%% ctx", tok_fmt, remaining);
                }
            } else {
                mid_text[0] = '\0';
                mt = 0;
            }

            // Count visible width
            mid_vis = 0;
            for (int i = 0; i < mt; i++) {
                unsigned char c = (unsigned char)mid_text[i];
                if ((c & 0xC0) != 0x80) mid_vis++;
            }

            mp_zone = a_sgr(mid_raw, mp_zone, mcap, "48;2;200;118;0");
            mp_zone = a_sgr(mid_raw, mp_zone, mcap, "38;2;80;50;0");
            mp_zone = a_str(mid_raw, mp_zone, mcap, mid_text);
            mid_raw[mp_zone] = '\0';
        };

        // RIGHT: timestamp
        char right_raw[512];
        int rp = 0;
        const int rcap = (int)sizeof(right_raw);
        int right_vis = 0;

        auto build_right = [&](WidthTier t) {
            rp = 0;
            right_vis = 0;
            char ts_buf[128];

            rp = a_sgr(right_raw, rp, rcap, "48;2;255;152;0");
            rp = a_sgr(right_raw, rp, rcap, "38;2;30;10;0");

            if (t == TIER_TINY) {
                // Just HH:MM
                format_timestamp_tiny(ts_buf, sizeof(ts_buf));
                rp = a_str(right_raw, rp, rcap, " ");
                rp = a_str(right_raw, rp, rcap, ts_buf);
                rp = a_str(right_raw, rp, rcap, " ");
                right_vis = 1 + (int)strlen(ts_buf) + 1;
            } else if (t == TIER_NARROW) {
                // HH:MM:SS, no emoji
                format_timestamp_narrow(ts_buf, sizeof(ts_buf));
                rp = a_str(right_raw, rp, rcap, " ");
                rp = a_str(right_raw, rp, rcap, ts_buf);
                rp = a_str(right_raw, rp, rcap, " ");
                right_vis = 1 + (int)strlen(ts_buf) + 1;
            } else if (t == TIER_MEDIUM) {
                // 🕐 02-22::10:54 (no day name, no seconds)
                format_timestamp_medium(ts_buf, sizeof(ts_buf));
                rp = a_str(right_raw, rp, rcap, " \xf0\x9f\x95\x90 "); // " 🕐 "
                right_vis += 4;
                rp = a_str(right_raw, rp, rcap, ts_buf);
                right_vis += (int)strlen(ts_buf);
                rp = a_str(right_raw, rp, rcap, " ");
                right_vis += 1;
            } else {
                // Full: 🕐 Sun 02-22::10:54:27
                format_timestamp_full(ts_buf, sizeof(ts_buf));
                rp = a_str(right_raw, rp, rcap, " \xf0\x9f\x95\x90 "); // " 🕐 "
                right_vis += 4;
                rp = a_str(right_raw, rp, rcap, ts_buf);
                right_vis += (int)strlen(ts_buf);
                rp = a_str(right_raw, rp, rcap, " ");
                right_vis += 1;
            }
            right_raw[rp] = '\0';
        };

        // Build all three zones at current tier, then check if they fit.
        // If total left+right exceeds term_cols, downgrade tier.
        WidthTier bar_tier = tier;
        for (;;) {
            build_left(bar_tier);
            build_mid(bar_tier);
            build_right(bar_tier);

            int min_width = left_vis + mid_vis + right_vis;
            if (min_width <= term_cols || bar_tier == TIER_TINY) break;
            bar_tier = (WidthTier)((int)bar_tier - 1);
        }

        // Assemble: left badge + muted middle (centered token info + fill) + bright right + reset
        p = a_str(raw, p, cap, left_raw);

        int mid_total = term_cols - left_vis - right_vis;
        if (mid_total < 0) mid_total = 0;
        int pad_left = (mid_total - mid_vis) / 2;
        int pad_right = mid_total - mid_vis - pad_left;
        if (pad_left < 0) pad_left = 0;
        if (pad_right < 0) pad_right = 0;

        p = a_sgr(raw, p, cap, "48;2;200;118;0");
        p = a_sgr(raw, p, cap, "38;2;80;50;0");
        p = a_fill(raw, p, cap, ' ', pad_left);
        p = a_str(raw, p, cap, mid_raw);
        p = a_sgr(raw, p, cap, "48;2;200;118;0");
        p = a_fill(raw, p, cap, ' ', pad_right);

        p = a_str(raw, p, cap, right_raw);

        p = a_sgr(raw, p, cap, "0");
        p = a_ch(raw, p, cap, '\n');
    }

    // Null terminate
    raw[p] = '\0';

    // -- JSON-escape and output ----------------------------------------------
    char escaped[32768];
    json_escape(raw, escaped, sizeof(escaped));

    char output[32768];
    int len = snprintf(output, sizeof(output), "{\"systemMessage\":\"%s\"}", escaped);
    if (len > 0 && len < (int)sizeof(output))
        write(STDOUT_FILENO, output, len);

    return 0;
}
