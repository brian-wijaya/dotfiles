// mosaic_sync.cpp - Session state display hook (SHM reader)
// Fires on: Stop (every response)
// Target latency: <1ms
//
// Reads authoritative session state from three SHM files:
//   /dev/shm/mosaic_session_id  (written by capture-user-turn)
//   /dev/shm/mosaic_turn_id     (written by capture-user-turn)
//   /dev/shm/mosaic_timestamp   (written by capture-user-turn)
//
// These are independent of the Mosaic app — always available.
// Outputs systemMessage JSON to stdout. Never blocks — exits 0 always.

#include <cstdio>
#include <cstring>
#include <unistd.h>
#include <fcntl.h>

// Read a small SHM file into buf. Returns bytes read (0 on failure).
static int read_shm(const char* path, char* buf, int buf_len) {
    int fd = open(path, O_RDONLY);
    if (fd < 0) return 0;
    ssize_t n = read(fd, buf, buf_len - 1);
    close(fd);
    if (n <= 0) return 0;

    // Strip trailing whitespace
    while (n > 0 && (buf[n - 1] == '\n' || buf[n - 1] == '\r' || buf[n - 1] == ' '))
        n--;
    buf[n] = '\0';
    return (int)n;
}

// Drain stdin (hooks receive JSON on stdin — must consume it)
static void drain_stdin() {
    char buf[4096];
    int flags = fcntl(STDIN_FILENO, F_GETFL, 0);
    fcntl(STDIN_FILENO, F_SETFL, flags | O_NONBLOCK);
    while (read(STDIN_FILENO, buf, sizeof(buf)) > 0) {}
}

int main() {
    drain_stdin();

    char session[256] = "";
    char turn[256] = "";
    char timestamp[128] = "";

    read_shm("/dev/shm/mosaic_session_id", session, sizeof(session));
    read_shm("/dev/shm/mosaic_turn_id", turn, sizeof(turn));
    int ts_len = read_shm("/dev/shm/mosaic_timestamp", timestamp, sizeof(timestamp));

    // Need at least a timestamp to display anything useful
    if (ts_len == 0) return 0;

    // Build context string: " [s:abcdef01 t:abcdef01]"
    char ctx[256] = "";
    int cx = 0;
    bool has_session = session[0] != '\0';
    bool has_turn = turn[0] != '\0';

    if (has_session || has_turn) {
        cx += snprintf(ctx + cx, sizeof(ctx) - cx, " [");
        if (has_session)
            cx += snprintf(ctx + cx, sizeof(ctx) - cx, "s:%.8s", session);
        if (has_session && has_turn)
            cx += snprintf(ctx + cx, sizeof(ctx) - cx, " ");
        if (has_turn)
            cx += snprintf(ctx + cx, sizeof(ctx) - cx, "t:%.8s", turn);
        cx += snprintf(ctx + cx, sizeof(ctx) - cx, "]");
    }

    // Output: {"systemMessage":"mosaic · Fri Feb 20, 2026 - 15:34:41:123 CST [s:abcdef01 t:abcdef01]"}
    char output[512];
    int len = snprintf(output, sizeof(output),
        R"({"systemMessage":"\u00b7 %s%s"})",
        timestamp, ctx);

    if (len > 0 && len < (int)sizeof(output))
        write(STDOUT_FILENO, output, len);

    return 0;
}
