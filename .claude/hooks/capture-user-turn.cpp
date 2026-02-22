// capture-user-turn.cpp — UserPromptSubmit hook: capture user prompts.
//
// Reads JSON from stdin (session_id, prompt), writes SHM files for mosaic/hooks,
// then produces a protobuf-serialized Turn to the chronicle.turns Kafka topic via librdkafka.
// The pipeline (TurnSearchConsumer) is the single owner of context_turns writes.
// Fire-and-forget: exits 0 on any error.
//
// Compile:
//   g++ -O2 -o capture-user-turn capture-user-turn.cpp $(pkg-config --cflags --libs rdkafka)

#include <librdkafka/rdkafka.h>
#include <sys/ioctl.h>
#include <cstdio>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <ctime>
#include <fcntl.h>
#include <unistd.h>
#include <wchar.h>
#include <locale.h>
#include <string>
#include <vector>

static constexpr int MAX_INPUT_SIZE = 8192;
static constexpr int MAX_PROMPT = 500;
static constexpr int UUID_BUF = 37; // 36 chars + null

// ─── Minimal protobuf encoder ───────────────────────────────────────────────
// Supports only what we need: varint, length-delimited (string/bytes/submessage).
// Wire types: 0 = varint, 2 = length-delimited.

static void pb_encode_varint(std::vector<uint8_t>& buf, uint64_t value) {
    while (value > 0x7F) {
        buf.push_back(static_cast<uint8_t>((value & 0x7F) | 0x80));
        value >>= 7;
    }
    buf.push_back(static_cast<uint8_t>(value));
}

static void pb_encode_tag(std::vector<uint8_t>& buf, int field, int wire_type) {
    pb_encode_varint(buf, static_cast<uint64_t>((field << 3) | wire_type));
}

// Field with varint value (int32, int64, enum).
static void pb_encode_varint_field(std::vector<uint8_t>& buf, int field, uint64_t value) {
    pb_encode_tag(buf, field, 0);
    pb_encode_varint(buf, value);
}

// Field with length-delimited value (string, bytes, embedded message).
static void pb_encode_bytes_field(std::vector<uint8_t>& buf, int field,
                                  const uint8_t* data, size_t len) {
    pb_encode_tag(buf, field, 2);
    pb_encode_varint(buf, len);
    buf.insert(buf.end(), data, data + len);
}

static void pb_encode_string_field(std::vector<uint8_t>& buf, int field,
                                   const char* str, size_t len) {
    pb_encode_bytes_field(buf, field, reinterpret_cast<const uint8_t*>(str), len);
}

// ─── Minimal JSON string extraction ─────────────────────────────────────────
// Finds "key":"value" and writes value into dst (up to dst_len-1 chars).
// Returns length written, or 0 if not found.
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

// Generate UUID v4 from /dev/urandom. Returns false on failure.
static bool generate_uuid(char* buf) {
    unsigned char bytes[16];
    int fd = open("/dev/urandom", O_RDONLY);
    if (fd < 0) return false;
    ssize_t n = read(fd, bytes, 16);
    close(fd);
    if (n != 16) return false;

    bytes[6] = (bytes[6] & 0x0F) | 0x40;
    bytes[8] = (bytes[8] & 0x3F) | 0x80;

    snprintf(buf, UUID_BUF, "%02x%02x%02x%02x-%02x%02x-%02x%02x-%02x%02x-%02x%02x%02x%02x%02x%02x",
             bytes[0], bytes[1], bytes[2], bytes[3],
             bytes[4], bytes[5], bytes[6], bytes[7],
             bytes[8], bytes[9], bytes[10], bytes[11],
             bytes[12], bytes[13], bytes[14], bytes[15]);
    return true;
}

// Build session-scoped SHM path: /dev/shm/{base}.{first8_of_session_id}
static void shm_path(char* buf, int buf_len, const char* base, const char* session_id) {
    if (session_id[0] != '\0') {
        snprintf(buf, buf_len, "/dev/shm/%s.%.8s", base, session_id);
    } else {
        snprintf(buf, buf_len, "/dev/shm/%s", base);
    }
}

// Walk ancestor process tree to find a parent with a TTY and read its width.
// Hooks run with piped stdin/stdout — no direct TTY access. The parent Claude Code
// process holds the real terminal, so we open /proc/<ancestor>/fd/0 and ioctl it.
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

        const char* cp = strrchr(stat_buf, ')');
        if (!cp || !cp[1] || !cp[2]) break;
        int ppid = 0;
        char state;
        if (sscanf(cp + 2, "%c %d", &state, &ppid) != 2 || ppid <= 1) break;

        snprintf(path, sizeof(path), "/proc/%d/fd/0", ppid);
        int tty_fd = open(path, O_RDONLY);
        if (tty_fd >= 0) {
            struct winsize ws;
            if (ioctl(tty_fd, TIOCGWINSZ, &ws) == 0 && ws.ws_col > 0) {
                close(tty_fd);
                return ws.ws_col;
            }
            close(tty_fd);
        }

        pid = ppid;
    }
    return 0;
}

// Detect terminal width: try STDERR, STDOUT, /dev/tty, $COLUMNS, ancestor PTY, fallback 80
static int get_terminal_width() {
    struct winsize ws;
    if (ioctl(STDERR_FILENO, TIOCGWINSZ, &ws) == 0 && ws.ws_col > 0) return ws.ws_col;
    if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == 0 && ws.ws_col > 0) return ws.ws_col;
    int tty_fd = open("/dev/tty", O_RDONLY);
    if (tty_fd >= 0) {
        bool ok = (ioctl(tty_fd, TIOCGWINSZ, &ws) == 0 && ws.ws_col > 0);
        close(tty_fd);
        if (ok) return ws.ws_col;
    }
    const char* cols = getenv("COLUMNS");
    if (cols) {
        int c = atoi(cols);
        if (c > 0) return c;
    }
    int ancestor_width = get_terminal_width_from_ancestors();
    if (ancestor_width > 0) return ancestor_width;
    return 80; // conservative fallback
}

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

int main() {
    // Read stdin with raw POSIX I/O
    char input[MAX_INPUT_SIZE];
    int total = 0;
    while (total < MAX_INPUT_SIZE - 1) {
        ssize_t n = read(STDIN_FILENO, input + total, MAX_INPUT_SIZE - 1 - total);
        if (n <= 0) break;
        total += n;
    }
    input[total] = '\0';
    if (total == 0) return 0;

    // Extract fields
    char session_id[256];
    char prompt[MAX_INPUT_SIZE];
    int sid_len = json_extract(input, total, "session_id", session_id, sizeof(session_id));
    int prompt_len = json_extract(input, total, "prompt", prompt, sizeof(prompt));

    if (sid_len == 0) {
        strcpy(session_id, "unknown");
    }

    char path[384];

    // ─── Interrupt detection ─────────────────────────────────────────────
    // Check if this is an interrupt (previous turn not yet finished)
    bool is_interrupt = false;
    {
        char saved_path[256];
        shm_path(saved_path, sizeof(saved_path), "turn_saved", session_id);
        char saved_buf[16] = "";
        int sfd = open(saved_path, O_RDONLY);
        if (sfd >= 0) {
            ssize_t n = read(sfd, saved_buf, sizeof(saved_buf) - 1);
            close(sfd);
            if (n > 0) {
                saved_buf[n] = '\0';
                // If turn_saved is "0", previous turn hasn't finished → this is an interrupt
                is_interrupt = (saved_buf[0] == '0');
            }
            // If file exists but empty, or value is "1" → new turn
        }
        // If file doesn't exist → new turn (first message of session)
    }

    // Generate UUID for this turn (only for new turns)
    char turn_id[UUID_BUF];

    if (!is_interrupt) {
        if (!generate_uuid(turn_id)) return 0;

        // Write turn_id to SHM for mosaic (unscoped — backward compat)
        int shm_fd = open("/dev/shm/mosaic_turn_id", O_WRONLY | O_CREAT | O_TRUNC, 0644);
        if (shm_fd >= 0) {
            write(shm_fd, turn_id, strlen(turn_id));
            close(shm_fd);
        }
    } else {
        // Interrupt: read existing turn_id from session-scoped SHM
        shm_path(path, sizeof(path), "turn_id", session_id);
        int tid_fd = open(path, O_RDONLY);
        if (tid_fd >= 0) {
            ssize_t n = read(tid_fd, turn_id, UUID_BUF - 1);
            close(tid_fd);
            if (n > 0) {
                turn_id[n] = '\0';
            } else {
                // Fallback: generate new UUID if read fails
                if (!generate_uuid(turn_id)) return 0;
            }
        } else {
            // Fallback: generate new UUID if file doesn't exist
            if (!generate_uuid(turn_id)) return 0;
        }
    }

    // Write session_id to SHM for mosaic (unscoped — backward compat)
    if (sid_len > 0) {
        int shm_fd2 = open("/dev/shm/mosaic_session_id", O_WRONLY | O_CREAT | O_TRUNC, 0644);
        if (shm_fd2 >= 0) {
            write(shm_fd2, session_id, strlen(session_id));
            close(shm_fd2);
        }
    }

    // Capture wall-clock time once — used for SHM display and Kafka protobuf
    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);

    // Generate human-readable CST timestamp for terminal display
    // Concise format: "Sun 02-22::10:54:27"
    char display_ts[80];
    // Full format for SHM (backward compat with Mosaic): "Sat 02-22-26::14:23:41:137 CST"
    char shm_ts[80];
    {
        struct tm cst;
        time_t cst_time = ts.tv_sec - 6 * 3600;
        gmtime_r(&cst_time, &cst);
        int ms = (int)(ts.tv_nsec / 1000000);

        // Short format for banner display (no year, no ms, no CST)
        strftime(display_ts, sizeof(display_ts), "%a %m-%d::%H:%M:%S", &cst);

        // Full format for SHM (Mosaic backward compat)
        char date_part[48];
        strftime(date_part, sizeof(date_part), "%a %m-%d-%y::%H:%M:%S", &cst);
        snprintf(shm_ts, sizeof(shm_ts), "%s:%03d CST", date_part, ms);

        // Unscoped — backward compat with Mosaic Tauri app (uses full format)
        int ts_fd = open("/dev/shm/mosaic_timestamp", O_WRONLY | O_CREAT | O_TRUNC, 0644);
        if (ts_fd >= 0) {
            write(ts_fd, shm_ts, strlen(shm_ts));
            close(ts_fd);
        }
    }

    // ─── Conditional SHM writes (new turns only, not interrupts) ─────────
    if (!is_interrupt) {
        // Write raw epoch to session-scoped SHM for duration tracking
        {
            char epoch_buf[32];
            snprintf(epoch_buf, sizeof(epoch_buf), "%ld.%09ld", (long)ts.tv_sec, (long)ts.tv_nsec);
            shm_path(path, sizeof(path), "turn_start_epoch", session_id);
            int epoch_fd = open(path, O_WRONLY | O_CREAT | O_TRUNC, 0644);
            if (epoch_fd >= 0) {
                write(epoch_fd, epoch_buf, strlen(epoch_buf));
                close(epoch_fd);
            }
        }

        // Initialize turn_stats SHM (session-scoped) — zero counters for the new turn
        {
            const char* stats_init = "total=0\nagents=0\nedits=0\ntools=\n";
            shm_path(path, sizeof(path), "turn_stats", session_id);
            int stats_fd = open(path, O_WRONLY | O_CREAT | O_TRUNC, 0644);
            if (stats_fd >= 0) {
                write(stats_fd, stats_init, strlen(stats_init));
                close(stats_fd);
            }
        }

        // Reset turn_saved SHM (session-scoped) — mark turn as unsaved
        {
            shm_path(path, sizeof(path), "turn_saved", session_id);
            int saved_fd = open(path, O_WRONLY | O_CREAT | O_TRUNC, 0644);
            if (saved_fd >= 0) {
                write(saved_fd, "0", 1);
                close(saved_fd);
            }
        }

        // Reset turn_edit_files SHM (session-scoped) — clear edit file list for new turn
        {
            shm_path(path, sizeof(path), "turn_edit_files", session_id);
            int efd = open(path, O_WRONLY | O_CREAT | O_TRUNC, 0644);
            if (efd >= 0) close(efd);
        }

        // Write turn_id to session-scoped SHM (for mosaic_sync and other hooks)
        {
            shm_path(path, sizeof(path), "turn_id", session_id);
            int tid_fd = open(path, O_WRONLY | O_CREAT | O_TRUNC, 0644);
            if (tid_fd >= 0) {
                write(tid_fd, turn_id, strlen(turn_id));
                close(tid_fd);
            }
        }
    }

    // ─── Read/increment turn number from SHM ──────────────────────────────
    int turn_number = 1;
    if (!is_interrupt) {
        shm_path(path, sizeof(path), "turn_number", session_id);
        int tn_fd = open(path, O_RDWR);
        if (tn_fd >= 0) {
            char tn_buf[32] = {0};
            ssize_t n = read(tn_fd, tn_buf, sizeof(tn_buf) - 1);
            if (n > 0) {
                tn_buf[n] = '\0';
                turn_number = atoi(tn_buf) + 1;
                if (turn_number < 1) turn_number = 1;
            }
            // Write back incremented value
            lseek(tn_fd, 0, SEEK_SET);
            if (ftruncate(tn_fd, 0) != 0) { /* best effort */ }
            char new_val[32];
            int new_len = snprintf(new_val, sizeof(new_val), "%d", turn_number);
            write(tn_fd, new_val, new_len);
            close(tn_fd);
        } else {
            // File doesn't exist — create with initial value 1
            tn_fd = open(path, O_WRONLY | O_CREAT | O_TRUNC, 0644);
            if (tn_fd >= 0) {
                write(tn_fd, "1", 1);
                close(tn_fd);
            }
        }
    } else {
        // Interrupt: read current turn_number without incrementing
        shm_path(path, sizeof(path), "turn_number", session_id);
        int tn_fd = open(path, O_RDONLY);
        if (tn_fd >= 0) {
            char tn_buf[32] = {0};
            ssize_t n = read(tn_fd, tn_buf, sizeof(tn_buf) - 1);
            close(tn_fd);
            if (n > 0) {
                tn_buf[n] = '\0';
                turn_number = atoi(tn_buf);
                if (turn_number < 1) turn_number = 1;
            }
        }
    }

    // Skip empty prompts (SHM already written above — Kafka/banner skipped)
    if (prompt_len == 0 || (prompt_len == 4 && strcmp(prompt, "null") == 0)) {
        return 0;
    }

    // Truncate prompt to MAX_PROMPT chars
    if (prompt_len > MAX_PROMPT) {
        prompt[MAX_PROMPT] = '\0';
        prompt_len = MAX_PROMPT;
    }

    // Read session name from session-scoped SHM (may not exist on first turn)
    char session_name[256] = {0};
    int session_name_len = 0;
    {
        shm_path(path, sizeof(path), "turn_session_name", session_id);
        int sn_fd = open(path, O_RDONLY);
        if (sn_fd >= 0) {
            ssize_t n = read(sn_fd, session_name, sizeof(session_name) - 1);
            close(sn_fd);
            if (n > 0) {
                session_name[n] = '\0';
                // Trim trailing whitespace/newlines
                while (n > 0 && (session_name[n-1] == '\n' || session_name[n-1] == '\r'
                                 || session_name[n-1] == ' ')) {
                    session_name[--n] = '\0';
                }
                session_name_len = (int)n;
            }
        }
    }

    // Read session number from session-scoped SHM
    char session_number[32] = {0};
    bool has_session_number = false;
    {
        shm_path(path, sizeof(path), "turn_session_number", session_id);
        int snum_fd = open(path, O_RDONLY);
        if (snum_fd >= 0) {
            ssize_t n = read(snum_fd, session_number, sizeof(session_number) - 1);
            close(snum_fd);
            if (n > 0) {
                session_number[n] = '\0';
                // Trim trailing whitespace
                while (n > 0 && (session_number[n-1] == '\n' || session_number[n-1] == '\r'
                                 || session_number[n-1] == ' ')) {
                    session_number[--n] = '\0';
                }
                if (n > 0) has_session_number = true;
            }
        }
    }

    // ─── Produce to Kafka chronicle.turns (best-effort) ─────────────────────
    // TurnSearchConsumer is the single owner of context_turns writes.
    //
    // Build a protobuf-serialized Turn message matching turn.proto:
    //   field 1 (string): turn_id
    //   field 2 (string): session_id
    //   field 3 (int32):  pass_number = 1
    //   field 4 (enum):   role = ROLE_USER (1)
    //   field 5 (string): content
    //   field 7 (message): timestamp = google.protobuf.Timestamp { seconds, nanos }
    {
        // Encode google.protobuf.Timestamp submessage first
        std::vector<uint8_t> ts_msg;
        pb_encode_varint_field(ts_msg, 1, static_cast<uint64_t>(ts.tv_sec));   // seconds
        pb_encode_varint_field(ts_msg, 2, static_cast<uint64_t>(ts.tv_nsec));  // nanos

        // Encode the Turn message
        std::vector<uint8_t> turn_msg;
        turn_msg.reserve(512);
        pb_encode_string_field(turn_msg, 1, turn_id, strlen(turn_id));
        pb_encode_string_field(turn_msg, 2, session_id, strlen(session_id));
        pb_encode_varint_field(turn_msg, 3, 1);           // pass_number = 1
        pb_encode_varint_field(turn_msg, 4, 1);           // ROLE_USER = 1
        pb_encode_string_field(turn_msg, 5, prompt, prompt_len);
        // field 6 (repeated ToolCall) omitted — no tool calls for user turns
        pb_encode_bytes_field(turn_msg, 7, ts_msg.data(), ts_msg.size());  // Timestamp submessage

        // Create Kafka producer
        char errstr[256];
        rd_kafka_conf_t* conf = rd_kafka_conf_new();
        if (rd_kafka_conf_set(conf, "bootstrap.servers", "localhost:9092",
                              errstr, sizeof(errstr)) != RD_KAFKA_CONF_OK) {
            fprintf(stderr, "capture-user-turn: kafka conf error: %s\n", errstr);
            rd_kafka_conf_destroy(conf);
            return 0;
        }
        // Optimize for short-lived producer: small queue, fast linger
        rd_kafka_conf_set(conf, "queue.buffering.max.messages", "10", errstr, sizeof(errstr));
        rd_kafka_conf_set(conf, "linger.ms", "0", errstr, sizeof(errstr));

        rd_kafka_t* rk = rd_kafka_new(RD_KAFKA_PRODUCER, conf, errstr, sizeof(errstr));
        if (!rk) {
            fprintf(stderr, "capture-user-turn: kafka producer creation failed: %s\n", errstr);
            // conf is destroyed by rd_kafka_new on failure
            return 0;
        }

        // Produce the message
        rd_kafka_resp_err_t err = rd_kafka_producev(
            rk,
            RD_KAFKA_V_TOPIC("chronicle.turns"),
            RD_KAFKA_V_KEY(turn_id, strlen(turn_id)),
            RD_KAFKA_V_VALUE(turn_msg.data(), turn_msg.size()),
            RD_KAFKA_V_MSGFLAGS(RD_KAFKA_MSG_F_COPY),
            RD_KAFKA_V_END
        );
        if (err != RD_KAFKA_RESP_ERR_NO_ERROR) {
            fprintf(stderr, "capture-user-turn: kafka produce failed: %s\n",
                    rd_kafka_err2str(err));
        }

        // Flush with 1-second timeout to ensure delivery
        rd_kafka_flush(rk, 1000);

        rd_kafka_destroy(rk);
    }

    // ─── Emit start-of-turn banner as systemMessage JSON on stdout ──────────
    if (is_interrupt) {
        // Interrupt: minimal timestamp only, no background color, no banner
        // \xf0\x9f\x95\x90 = 🕐
        printf("{\"systemMessage\":\"\\n\xf0\x9f\x95\x90 %s\"}\n", display_ts);
    } else {
        // New turn: full-width two-zone bar
        // Left zone: muted blue bg with session/turn info
        // Right zone: bright neon blue bg with timestamp badge
        char first8[9] = {0};
        int sid_copy = (int)strlen(session_id);
        if (sid_copy > 8) sid_copy = 8;
        memcpy(first8, session_id, sid_copy);
        first8[sid_copy] = '\0';

        // Read agent turn total from SHM for global turn count
        int agent_turns = 0;
        {
            shm_path(path, sizeof(path), "agent_turn_total", session_id);
            int at_fd = open(path, O_RDONLY);
            if (at_fd >= 0) {
                char at_buf[32] = {0};
                ssize_t n = read(at_fd, at_buf, sizeof(at_buf) - 1);
                close(at_fd);
                if (n > 0) {
                    at_buf[n] = '\0';
                    agent_turns = atoi(at_buf);
                    if (agent_turns < 0) agent_turns = 0;
                }
            }
        }
        int global_turn = turn_number + agent_turns;

        int term_width = get_terminal_width();
        term_width -= 6; // Claude Code indents hook systemMessage output by ~5-6 cols

        // DEBUG: write detected term_width to SHM for diagnostics
        {
            char debug_path[256];
            shm_path(debug_path, sizeof(debug_path), "capture_debug_cols", session_id);
            FILE* f = fopen(debug_path, "w");
            if (f) { fprintf(f, "%d", term_width); fclose(f); }
        }

        // ANSI codes (JSON-escaped: \\u001b = ESC)
        // Muted/desaturated blue: bg 40;60;120, fg 180;200;230
        #define CLR_MUTED_BG    "\\u001b[48;2;40;60;120m"
        #define CLR_MUTED_FG    "\\u001b[38;2;180;200;230m"
        // Bright neon blue badge: bg 60;140;255, fg 255;255;255
        #define CLR_BADGE_BG    "\\u001b[48;2;60;140;255m"
        #define CLR_BADGE_FG    "\\u001b[38;2;255;255;255m"
        #define CLR_BOLD        "\\u001b[1m"
        #define CLR_NBOLD       "\\u001b[22m"
        #define CLR_RESET       "\\u001b[0m"

        // ─── Width-adaptive layout tiers ──────────────────────────────────
        // Build multiple left/right candidates, pick the widest that fits.
        // Emojis: \xf0\x9f\x93\x84 = 📄 (session), \xf0\x9f\x8c\x90 = 🌐 (ST), \xf0\x9f\x93\x9d = 📝 (UT), \xf0\x9f\x95\x90 = 🕐 (time)
        //
        // Time format components (from struct tm cst, already computed above):
        //   display_ts = "Sun 02-22::10:54:27" (full short — no year, no ms, no tz)
        //   We derive shorter variants below.

        // Prepare time strings for each tier
        char ts_time_only[16];   // "10:54"
        char ts_hms[16];         // "10:54:27"
        char ts_date_hm[24];    // "02-22::10:54"
        {
            struct tm cst;
            time_t cst_time = ts.tv_sec - 6 * 3600;
            gmtime_r(&cst_time, &cst);
            strftime(ts_time_only, sizeof(ts_time_only), "%H:%M", &cst);
            strftime(ts_hms, sizeof(ts_hms), "%H:%M:%S", &cst);
            strftime(ts_date_hm, sizeof(ts_date_hm), "%m-%d::%H:%M", &cst);
            // display_ts already has "Sun 02-22::10:54:27"
        }

        // First 4 chars of session_id / turn_id (for MEDIUM tier)
        char first4_sid[5] = {0};
        {
            int n4 = (int)strlen(session_id);
            if (n4 > 4) n4 = 4;
            memcpy(first4_sid, session_id, n4);
            first4_sid[n4] = '\0';
        }
        char first4_tid[5] = {0};
        {
            int n4 = (int)strlen(turn_id);
            if (n4 > 4) n4 = 4;
            memcpy(first4_tid, turn_id, n4);
            first4_tid[n4] = '\0';
        }

        const char* snum = has_session_number ? session_number : "--";

        // Build candidates from widest (FULL) to narrowest (TINY)
        // Each: left_text + right_text. We check visible_width(left) + visible_width(right) <= term_width.
        struct Candidate {
            char left[256];
            char right[128];
        };
        Candidate cands[4];
        int ncands = 0;

        // FULL (140+): " 📄 S#1924 · 320a6835 │ 🌐 ST#10 │ 📝 UT#7 · bd351f5a"  right: " 🕐 Sun 02-22::10:54:27 "
        snprintf(cands[ncands].left, sizeof(cands[ncands].left),
            " \xf0\x9f\x93\x84 S#%s \xc2\xb7 %s \xe2\x94\x82 \xf0\x9f\x8c\x90 ST#%d \xe2\x94\x82 \xf0\x9f\x93\x9d UT#%d \xc2\xb7 %.8s",
            snum, first8, global_turn, turn_number, turn_id);
        snprintf(cands[ncands].right, sizeof(cands[ncands].right),
            " \xf0\x9f\x95\x90 %s ", display_ts);
        ncands++;

        // MEDIUM (90-139): " 📄 S#1924 · 320a │ 🌐 ST#10 │ 📝 UT#7 · bd35"  right: " 🕐 02-22::10:54 "
        snprintf(cands[ncands].left, sizeof(cands[ncands].left),
            " \xf0\x9f\x93\x84 S#%s \xc2\xb7 %s \xe2\x94\x82 \xf0\x9f\x8c\x90 ST#%d \xe2\x94\x82 \xf0\x9f\x93\x9d UT#%d \xc2\xb7 %s",
            snum, first4_sid, global_turn, turn_number, first4_tid);
        snprintf(cands[ncands].right, sizeof(cands[ncands].right),
            " \xf0\x9f\x95\x90 %s ", ts_date_hm);
        ncands++;

        // NARROW (60-89): " 📄 S#1924 │ 🌐 ST#10 │ 📝 UT#7"  right: " 10:54:27 "
        snprintf(cands[ncands].left, sizeof(cands[ncands].left),
            " \xf0\x9f\x93\x84 S#%s \xe2\x94\x82 \xf0\x9f\x8c\x90 ST#%d \xe2\x94\x82 \xf0\x9f\x93\x9d UT#%d",
            snum, global_turn, turn_number);
        snprintf(cands[ncands].right, sizeof(cands[ncands].right),
            " %s ", ts_hms);
        ncands++;

        // TINY (<60): "S#1924│🌐ST#10│📝UT#7"  right: " 10:54 "
        snprintf(cands[ncands].left, sizeof(cands[ncands].left),
            "S#%s\xe2\x94\x82\xf0\x9f\x8c\x90ST#%d\xe2\x94\x82\xf0\x9f\x93\x9dUT#%d",
            snum, global_turn, turn_number);
        snprintf(cands[ncands].right, sizeof(cands[ncands].right),
            " %s ", ts_time_only);
        ncands++;

        // Pick the widest candidate that fits
        int chosen = ncands - 1; // default to TINY
        for (int i = 0; i < ncands; i++) {
            int lw = visible_width(cands[i].left);
            int rw = visible_width(cands[i].right);
            if (lw + rw <= term_width) {
                chosen = i;
                break;
            }
        }

        const char* left_text = cands[chosen].left;
        const char* right_text = cands[chosen].right;
        int left_vis = visible_width(left_text);
        int right_vis = visible_width(right_text);

        // Padding between left content and right badge
        int pad = term_width - left_vis - right_vis;
        if (pad < 1) pad = 1;

        // Build padding string of spaces
        char pad_str[512];
        if (pad >= (int)sizeof(pad_str)) pad = (int)sizeof(pad_str) - 1;
        memset(pad_str, ' ', pad);
        pad_str[pad] = '\0';

        // Emit: \n + [muted bg][left content][padding] + [bright bg][right content] + [reset]
        printf("{\"systemMessage\":\"\\n"
               CLR_MUTED_BG CLR_MUTED_FG
               "%s%s"
               CLR_BADGE_BG CLR_BADGE_FG CLR_BOLD
               "%s"
               CLR_RESET
               "\"}\n",
               left_text, pad_str,
               right_text);
    }

    return 0;
}
