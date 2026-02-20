// capture-user-turn.cpp — UserPromptSubmit hook: capture user prompts as context_turns.
//
// Reads JSON from stdin (session_id, prompt), inserts into PostgreSQL canonical.context_turns.
// Fire-and-forget: exits 0 on any error.
//
// Compile:
//   g++ -O2 -o capture-user-turn capture-user-turn.cpp $(pkg-config --cflags --libs libpq)

#include <libpq-fe.h>
#include <cstdio>
#include <cstring>
#include <ctime>
#include <fcntl.h>
#include <unistd.h>

static constexpr int MAX_INPUT = 8192;
static constexpr int MAX_PROMPT = 500;
static constexpr int UUID_BUF = 37; // 36 chars + null

// Minimal JSON string extraction — no allocations, no library.
// Finds "key":"value" and writes value into dst (up to dst_len-1 chars).
// Returns length written, or 0 if not found.
static int json_extract(const char* json, int json_len, const char* key, char* dst, int dst_len) {
    // Build the search pattern: "key":"
    char pattern[128];
    int plen = snprintf(pattern, sizeof(pattern), "\"%s\":\"", key);
    if (plen <= 0 || plen >= (int)sizeof(pattern)) return 0;

    const char* pos = nullptr;
    const char* search = json;
    while ((pos = strstr(search, pattern)) != nullptr) {
        // Verify this is a top-level key (preceded by { or , or whitespace)
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
            p++; // skip backslash
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

    // Set version 4 and variant bits
    bytes[6] = (bytes[6] & 0x0F) | 0x40;
    bytes[8] = (bytes[8] & 0x3F) | 0x80;

    snprintf(buf, UUID_BUF, "%02x%02x%02x%02x-%02x%02x-%02x%02x-%02x%02x-%02x%02x%02x%02x%02x%02x",
             bytes[0], bytes[1], bytes[2], bytes[3],
             bytes[4], bytes[5], bytes[6], bytes[7],
             bytes[8], bytes[9], bytes[10], bytes[11],
             bytes[12], bytes[13], bytes[14], bytes[15]);
    return true;
}

int main() {
    // Read stdin with raw POSIX I/O
    char input[MAX_INPUT];
    int total = 0;
    while (total < MAX_INPUT - 1) {
        ssize_t n = read(STDIN_FILENO, input + total, MAX_INPUT - 1 - total);
        if (n <= 0) break;
        total += n;
    }
    input[total] = '\0';
    if (total == 0) return 0;

    // Extract fields
    char session_id[256];
    char prompt[MAX_INPUT]; // extract full, then truncate
    int sid_len = json_extract(input, total, "session_id", session_id, sizeof(session_id));
    int prompt_len = json_extract(input, total, "prompt", prompt, sizeof(prompt));

    if (sid_len == 0) {
        // Default session_id
        strcpy(session_id, "unknown");
    }

    // Skip empty prompts
    if (prompt_len == 0 || (prompt_len == 4 && strcmp(prompt, "null") == 0)) {
        return 0;
    }

    // Truncate prompt to MAX_PROMPT chars (byte truncation, good enough for ASCII-dominant content)
    if (prompt_len > MAX_PROMPT) {
        prompt[MAX_PROMPT] = '\0';
        prompt_len = MAX_PROMPT;
    }

    // Generate UUID
    char turn_id[UUID_BUF];
    if (!generate_uuid(turn_id)) return 0;

    // Write turn_id to SHM for mosaic to pick up
    int shm_fd = open("/dev/shm/mosaic_turn_id", O_WRONLY | O_CREAT | O_TRUNC, 0644);
    if (shm_fd >= 0) {
        write(shm_fd, turn_id, strlen(turn_id));
        close(shm_fd);
    }

    // Write session_id to SHM for mosaic to pick up
    if (sid_len > 0) {
        int shm_fd2 = open("/dev/shm/mosaic_session_id", O_WRONLY | O_CREAT | O_TRUNC, 0644);
        if (shm_fd2 >= 0) {
            write(shm_fd2, session_id, strlen(session_id));
            close(shm_fd2);
        }
    }

    // Generate ISO 8601 UTC timestamp for Postgres
    char timestamp[32];
    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);
    struct tm utc;
    gmtime_r(&ts.tv_sec, &utc);
    strftime(timestamp, sizeof(timestamp), "%Y-%m-%dT%H:%M:%S.000Z", &utc);

    // Generate human-readable CST timestamp for terminal display
    // Format: "Fri Feb 20, 2026 - 15:34:41:123 CST"
    {
        struct tm cst;
        // CST = UTC-6
        time_t cst_time = ts.tv_sec - 6 * 3600;
        gmtime_r(&cst_time, &cst);
        int ms = (int)(ts.tv_nsec / 1000000);

        char display_ts[80];
        char date_part[48];
        strftime(date_part, sizeof(date_part), "%a %b %d, %Y - %H:%M:%S", &cst);
        snprintf(display_ts, sizeof(display_ts), "%s:%03d CST", date_part, ms);

        int ts_fd = open("/dev/shm/mosaic_timestamp", O_WRONLY | O_CREAT | O_TRUNC, 0644);
        if (ts_fd >= 0) {
            write(ts_fd, display_ts, strlen(display_ts));
            close(ts_fd);
        }
    }

    // Connect to PostgreSQL
    PGconn* conn = PQconnectdb(
        "host=localhost port=5432 user=actual password=actual dbname=worldview "
        "options='-c search_path=canonical,public' connect_timeout=2"
    );
    if (!conn || PQstatus(conn) != CONNECTION_OK) {
        if (conn) PQfinish(conn);
        return 0;
    }

    // Parameterized INSERT
    const char* sql =
        "INSERT INTO context_turns (turn_id, session_id, pass_number, role, content, timestamp) "
        "VALUES ($1, $2, 1, 'user', $3, $4) "
        "ON CONFLICT (turn_id) DO NOTHING";

    const char* params[4] = { turn_id, session_id, prompt, timestamp };
    int param_lengths[4] = { (int)strlen(turn_id), (int)strlen(session_id), prompt_len, (int)strlen(timestamp) };
    int param_formats[4] = { 0, 0, 0, 0 }; // text format

    PGresult* res = PQexecParams(conn, sql, 4, nullptr, params, param_lengths, param_formats, 0);
    if (res) PQclear(res);
    PQfinish(conn);

    return 0;
}
