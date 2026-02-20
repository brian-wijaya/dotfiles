// claude_edit_stream_pre.cpp — PreToolUse hook: prepare Emacs surface before Edit/Write
//
// Fires on: PreToolUse (filtered to Edit and Write tool calls)
// Reads /dev/shm/x11_state to check if Emacs is on the user's current workspace.
// If not, sends a notification, moves Emacs to the focused workspace, and arranges layout.
//
// ALWAYS exits 0 — this hook prepares the surface, never blocks the edit.
//
// Raw POSIX I/O, stack buffers only.
//
// Compile:
//   g++ -O2 -o claude_edit_stream_pre claude_edit_stream_pre.cpp

#include <fcntl.h>
#include <unistd.h>
#include <cstdio>
#include <cstring>
#include <cstdlib>

static constexpr int INPUT_BUF = 65536;
static constexpr int STATE_BUF = 512;
static constexpr int CMD_BUF = 1024;
static constexpr const char* X11_STATE_PATH = "/dev/shm/x11_state";

// Extract a JSON string value for a given key from flat JSON.
// Returns length written to dst, or 0 if not found.
static int json_extract(const char* json, int json_len, const char* key,
                         char* dst, int dst_len) {
    char pattern[128];
    int plen = snprintf(pattern, sizeof(pattern), "\"%s\":", key);
    if (plen <= 0 || plen >= (int)sizeof(pattern)) return 0;

    const char* pos = strstr(json, pattern);
    if (!pos) return 0;

    const char* p = pos + plen;
    const char* end = json + json_len;

    // Skip whitespace
    while (p < end && (*p == ' ' || *p == '\t' || *p == '\n' || *p == '\r')) p++;

    if (p >= end || *p != '"') return 0;
    p++; // skip opening quote

    int i = 0;
    while (p < end && i < dst_len - 1) {
        if (*p == '"') break;
        if (*p == '\\' && p + 1 < end) {
            p++;
            switch (*p) {
                case '"':  dst[i++] = '"'; break;
                case '\\': dst[i++] = '\\'; break;
                case 'n':  dst[i++] = '\n'; break;
                case 'r':  dst[i++] = '\r'; break;
                case 't':  dst[i++] = '\t'; break;
                default:   dst[i++] = *p; break;
            }
        } else {
            dst[i++] = *p;
        }
        p++;
    }
    dst[i] = '\0';
    return i;
}

// Parse "key=value" from space-separated state string.
// Returns pointer to value start within state, or nullptr.
static const char* state_value(const char* state, const char* key) {
    char search[64];
    snprintf(search, sizeof(search), "%s=", key);
    const char* pos = strstr(state, search);
    if (!pos) return nullptr;
    return pos + strlen(search);
}

// Extract a value token (until space or newline or null)
static int extract_token(const char* start, char* dst, int dst_len) {
    int i = 0;
    while (start[i] && start[i] != ' ' && start[i] != '\n' && start[i] != '\r' && i < dst_len - 1) {
        dst[i] = start[i];
        i++;
    }
    dst[i] = '\0';
    return i;
}

int main() {
    // Unset stale I3SOCK — Claude's environment may have a dead socket path.
    // i3-msg will then use i3 --get-socketpath or scan /run/user/ to find the live one.
    unsetenv("I3SOCK");
    unsetenv("I3_SOCKET_PATH");

    // Read stdin — hook contract delivers JSON on stdin
    char input[INPUT_BUF];
    int total = 0;
    while (total < INPUT_BUF - 1) {
        ssize_t n = read(STDIN_FILENO, input + total, INPUT_BUF - 1 - total);
        if (n <= 0) break;
        total += n;
    }
    input[total] = '\0';

    // Extract file_path from tool_input for notification
    char file_path[4096];
    int fp_len = json_extract(input, total, "file_path", file_path, sizeof(file_path));

    // Get just the filename for the notification
    const char* filename = file_path;
    if (fp_len > 0) {
        const char* slash = strrchr(file_path, '/');
        if (slash) filename = slash + 1;
    } else {
        filename = "unknown file";
    }

    // Read /dev/shm/x11_state
    int fd = open(X11_STATE_PATH, O_RDONLY);
    if (fd < 0) return 0;  // No state file — can't do anything, don't block

    char state[STATE_BUF];
    ssize_t sn = read(fd, state, sizeof(state) - 1);
    close(fd);

    if (sn <= 0) return 0;
    state[sn] = '\0';

    // Check emacs_visible
    const char* vis = state_value(state, "emacs_visible");
    if (!vis) return 0;

    char vis_val[8];
    extract_token(vis, vis_val, sizeof(vis_val));

    // If Emacs is already visible, nothing to do
    if (strcmp(vis_val, "yes") == 0) return 0;

    // Get focused workspace number (needed for both paths below)
    const char* wksp = state_value(state, "workspace");
    if (!wksp) return 0;

    char wksp_val[8];
    extract_token(wksp, wksp_val, sizeof(wksp_val));
    int wksp_num = atoi(wksp_val);
    if (wksp_num <= 0) return 0;

    // Check emacs_wksp — if -1, the daemon's parser missed it.
    // Try xdotool as fallback to detect Emacs before giving up.
    const char* ewksp = state_value(state, "emacs_wksp");
    if (ewksp) {
        char ewksp_val[8];
        extract_token(ewksp, ewksp_val, sizeof(ewksp_val));
        if (strcmp(ewksp_val, "-1") == 0) {
            // Daemon says Emacs not found — try xdotool as fallback
            FILE* fp = popen("DISPLAY=:0 xdotool search --class Emacs 2>/dev/null", "r");
            if (!fp) return 0;
            char xdo_buf[64];
            char* got = fgets(xdo_buf, sizeof(xdo_buf), fp);
            pclose(fp);
            if (!got || xdo_buf[0] == '\0' || xdo_buf[0] == '\n') {
                // xdotool also found nothing — Emacs truly isn't running
                return 0;
            }
            // xdotool found an Emacs window that the daemon missed.
            // Fall through to move it to the focused workspace.
        }
    }

    // Emacs is NOT visible but IS running — prepare the surface

    // 1. Send desktop notification
    char cmd[CMD_BUF];
    snprintf(cmd, sizeof(cmd),
        "DISPLAY=:0 notify-send -u normal -t 3000 'Claude Edit Stream' 'Editing %s — bringing up editor'",
        filename);
    system(cmd);

    // 2. Move Emacs to the focused workspace
    snprintf(cmd, sizeof(cmd),
        "DISPLAY=:0 i3-msg '[class=\"Emacs\"] move container to workspace %d'",
        wksp_num);
    system(cmd);

    // 3. Arrange 50/50 split
    system("DISPLAY=:0 i3-msg 'layout splith'");

    // 4. Brief settle time for i3
    usleep(200000);  // 200ms

    return 0;  // ALWAYS exit 0 — never block the edit
}
