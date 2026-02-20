// sensor_awareness.cpp — PostToolUse hook: inject sensor ambient state into Claude context
//
// Reads /dev/shm/sensor_ambient (written by sensor fusion engine at 100Hz)
// Returns JSON with additionalContext field for Claude Code hook system
//
// Raw POSIX I/O, zero heap allocation, stack buffers only.
// Target latency: <0.5ms
//
// Compile:
//   g++ -O2 -o sensor_awareness sensor_awareness.cpp

#include <fcntl.h>
#include <unistd.h>
#include <cstdio>
#include <cstring>

static constexpr int SHM_BUF = 1024;
static constexpr int EMACS_BUF = 256;
static constexpr int I3_BUF = 256;
static constexpr int OUT_BUF = 2048;
static constexpr int DRAIN_BUF = 4096;
static constexpr const char* SHM_PATH = "/dev/shm/sensor_ambient";
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

int main() {
    // Drain stdin — hooks receive input that must be consumed
    {
        char drain[DRAIN_BUF];
        while (read(STDIN_FILENO, drain, sizeof(drain)) > 0) {}
    }

    // Read /dev/shm/sensor_ambient
    int fd = open(SHM_PATH, O_RDONLY);
    if (fd < 0) return 0;  // File doesn't exist — silent exit

    char shm[SHM_BUF];
    ssize_t n = read(fd, shm, sizeof(shm) - 1);
    close(fd);

    if (n <= 0) return 0;  // Empty or read error — silent exit

    // Strip trailing newline/whitespace
    while (n > 0 && (shm[n - 1] == '\n' || shm[n - 1] == '\r' || shm[n - 1] == ' ')) {
        n--;
    }
    if (n == 0) return 0;
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

    return 0;
}
