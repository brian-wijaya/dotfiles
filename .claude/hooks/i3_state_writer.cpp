// i3_state_writer.cpp — Daemon: subscribe to i3 events, write workspace/window state to SHM
//
// Subscribes to i3 workspace+window events via `i3-msg -t subscribe -m`.
// On each event, queries i3 for current workspaces and tree, then writes
// a single-line state summary to /dev/shm/x11_state.
//
// Output format:
//   workspace=N emacs_visible=yes|no emacs_wksp=M focused_class=ClassName
//
// Raw POSIX I/O, minimal heap usage (popen buffers only).
//
// Compile:
//   g++ -O2 -o i3_state_writer i3_state_writer.cpp

#include <fcntl.h>
#include <unistd.h>
#include <cstdio>
#include <cstring>
#include <cstdlib>
#include <signal.h>
#include <errno.h>
#include <sys/wait.h>

static constexpr const char* SHM_PATH = "/dev/shm/x11_state";
static constexpr int CMD_BUF = 131072;  // i3 tree can be large
static constexpr int OUT_BUF = 512;
static constexpr int SOCK_BUF = 256;

static volatile sig_atomic_t g_running = 1;
static char g_i3_socket[SOCK_BUF] = "";

static void signal_handler(int) {
    g_running = 0;
}

// Run a command via popen, read output into buf. Returns bytes read.
static int run_cmd(const char* cmd, char* buf, int buf_size) {
    FILE* fp = popen(cmd, "r");
    if (!fp) return -1;

    int total = 0;
    while (total < buf_size - 1) {
        size_t n = fread(buf + total, 1, buf_size - 1 - total, fp);
        if (n == 0) break;
        total += (int)n;
    }
    buf[total] = '\0';
    pclose(fp);
    return total;
}

// Discover the i3 IPC socket for the host display (:0).
// Uses `i3 --get-socketpath` which reads I3_SOCKET_PATH or finds running i3.
// Falls back to scanning /run/user/UID/i3/ for ipc-socket.* files.
// Stores result in g_i3_socket. Returns true if found.
static bool discover_i3_socket() {
    // Try i3 --get-socketpath first
    char buf[SOCK_BUF];
    int len = run_cmd("i3 --get-socketpath 2>/dev/null", buf, sizeof(buf));
    if (len > 0) {
        // Strip trailing whitespace
        while (len > 0 && (buf[len - 1] == '\n' || buf[len - 1] == '\r' || buf[len - 1] == ' '))
            len--;
        if (len > 0 && len < SOCK_BUF) {
            buf[len] = '\0';
            // Verify socket exists
            if (access(buf, F_OK) == 0) {
                strncpy(g_i3_socket, buf, SOCK_BUF - 1);
                g_i3_socket[SOCK_BUF - 1] = '\0';
                return true;
            }
        }
    }

    // Fallback: scan /run/user/1000/i3/ for ipc-socket.* (host i3, not :99)
    // Find the i3 PID that owns DISPLAY=:0 by checking process list
    len = run_cmd("ls -1 /run/user/1000/i3/ipc-socket.* 2>/dev/null | head -1", buf, sizeof(buf));
    if (len > 0) {
        while (len > 0 && (buf[len - 1] == '\n' || buf[len - 1] == '\r' || buf[len - 1] == ' '))
            len--;
        if (len > 0 && len < SOCK_BUF) {
            buf[len] = '\0';
            strncpy(g_i3_socket, buf, SOCK_BUF - 1);
            g_i3_socket[SOCK_BUF - 1] = '\0';
            return true;
        }
    }

    return false;
}

// Build an i3-msg command with the discovered socket
static int build_i3_cmd(char* cmd, int cmd_size, const char* args) {
    if (g_i3_socket[0]) {
        return snprintf(cmd, cmd_size, "i3-msg -s '%s' %s 2>/dev/null", g_i3_socket, args);
    } else {
        return snprintf(cmd, cmd_size, "i3-msg %s 2>/dev/null", args);
    }
}

// Find the focused workspace number from get_workspaces JSON.
// Searches for "focused":true and then looks backward for "num":N
static int find_focused_workspace(const char* json, int len) {
    const char* focused = strstr(json, "\"focused\":true");
    if (!focused) return -1;

    // Search backward from "focused":true for "num":
    const char* p = focused;
    while (p > json) {
        if (strncmp(p, "\"num\":", 6) == 0) {
            return atoi(p + 6);
        }
        p--;
    }
    return -1;
}

// Find the matching closing brace for an opening brace at json[start_pos].
// Returns pointer to the closing '}', or nullptr if not found.
static const char* find_matching_brace(const char* json, const char* open_brace, const char* end) {
    int depth = 0;
    const char* p = open_brace;
    while (p < end) {
        if (*p == '{') depth++;
        else if (*p == '}') {
            depth--;
            if (depth == 0) return p;
        }
        p++;
    }
    return nullptr;
}

// Find which workspace number contains an Emacs window.
// Strategy: find each "type":"workspace" marker, locate the opening '{' of that
// workspace object, find its matching '}' via brace counting, then search within
// those exact bounds for "class":"Emacs". This uses actual JSON structure rather
// than linear position heuristics.
// Also find the focused node's class (node with "focused":true in the tree).
static void parse_tree(const char* json, int len,
                       int* emacs_wksp, bool* emacs_found,
                       char* focused_class, int fc_size) {
    *emacs_wksp = -1;
    *emacs_found = false;
    focused_class[0] = '\0';

    const char* end = json + len;

    // --- Find Emacs workspace using brace-matched workspace objects ---
    const char* search = json;
    while (search < end && !*emacs_found) {
        const char* wtype = strstr(search, "\"type\":\"workspace\"");
        if (!wtype) break;

        // Find the opening '{' of this workspace object by scanning backward
        int brace_depth = 0;
        const char* obj_start = wtype;
        while (obj_start > json) {
            if (*obj_start == '}') brace_depth++;
            if (*obj_start == '{') {
                if (brace_depth == 0) break;
                brace_depth--;
            }
            obj_start--;
        }

        // Find the matching closing brace — this is the exact boundary of the workspace object
        const char* obj_end = find_matching_brace(json, obj_start, end);
        if (!obj_end) {
            search = wtype + 18;
            continue;
        }

        // Extract "num":N from within this workspace object (search forward from marker)
        // "num" can be 400-500+ chars from "type":"workspace" in i3's JSON, so use
        // the brace-matched object end rather than an arbitrary forward limit.
        const char* num_search = wtype;
        const char* num_limit = obj_end;
        int ws_num = -1;
        while (num_search < num_limit) {
            if (strncmp(num_search, "\"num\":", 6) == 0) {
                ws_num = atoi(num_search + 6);
                break;
            }
            num_search++;
        }

        if (ws_num >= 0) {
            // Search for "class":"Emacs" within the brace-matched workspace bounds
            const char* s = obj_start;
            while (s < obj_end) {
                const char* emacs_pos = strstr(s, "\"class\":\"Emacs\"");
                if (!emacs_pos || emacs_pos >= obj_end) break;
                *emacs_found = true;
                *emacs_wksp = ws_num;
                break;
            }
        }

        // Advance past this workspace marker
        search = wtype + 18;
    }

    // --- Pass 3: Find focused node's class ---
    // In i3 tree, focused leaves have "focused":true. Find the last occurrence
    // (deepest in tree) that has a "class" nearby.
    search = json;
    const char* last_focused = nullptr;
    while ((search = strstr(search, "\"focused\":true")) != nullptr) {
        last_focused = search;
        search += 14;
    }

    if (last_focused) {
        // Look for "class":"..." near this focused node.
        // The class field should be within the same JSON object.
        // Search backward for the opening { of this node, then forward for "class".
        int brace_depth = 0;
        const char* obj_start = last_focused;
        while (obj_start > json) {
            if (*obj_start == '}') brace_depth++;
            if (*obj_start == '{') {
                if (brace_depth == 0) break;
                brace_depth--;
            }
            obj_start--;
        }

        // Now search forward from obj_start for "class":"
        const char* class_search = obj_start;
        // But don't go past the next closing brace at this depth
        brace_depth = 0;
        while (class_search < json + len) {
            if (*class_search == '{') brace_depth++;
            if (*class_search == '}') {
                brace_depth--;
                if (brace_depth < 0) break;
            }
            if (strncmp(class_search, "\"class\":\"", 9) == 0) {
                const char* val = class_search + 9;
                int i = 0;
                while (val[i] && val[i] != '"' && i < fc_size - 1) {
                    focused_class[i] = val[i];
                    i++;
                }
                focused_class[i] = '\0';
                break;
            }
            class_search++;
        }
    }
}

// Write state to /dev/shm/x11_state atomically (write to .tmp, rename)
static void write_state(int focused_wksp, bool emacs_visible, int emacs_wksp,
                        const char* focused_class) {
    char out[OUT_BUF];
    int len = snprintf(out, sizeof(out),
        "workspace=%d emacs_visible=%s emacs_wksp=%d focused_class=%s\n",
        focused_wksp,
        emacs_visible ? "yes" : "no",
        emacs_wksp,
        focused_class[0] ? focused_class : "unknown");

    if (len <= 0 || len >= (int)sizeof(out)) return;

    // Write to tmp then rename for atomicity
    static constexpr const char* TMP_PATH = "/dev/shm/x11_state.tmp";
    int fd = open(TMP_PATH, O_WRONLY | O_CREAT | O_TRUNC, 0644);
    if (fd < 0) return;
    write(fd, out, len);
    close(fd);
    rename(TMP_PATH, SHM_PATH);
}

// Query i3 and update SHM state
static void update_state() {
    char* wksp_buf = (char*)malloc(CMD_BUF);
    char* tree_buf = (char*)malloc(CMD_BUF);
    if (!wksp_buf || !tree_buf) {
        free(wksp_buf);
        free(tree_buf);
        return;
    }

    char cmd[512];
    build_i3_cmd(cmd, sizeof(cmd), "-t get_workspaces");
    int wksp_len = run_cmd(cmd, wksp_buf, CMD_BUF);

    build_i3_cmd(cmd, sizeof(cmd), "-t get_tree");
    int tree_len = run_cmd(cmd, tree_buf, CMD_BUF);

    if (wksp_len <= 0 || tree_len <= 0) {
        free(wksp_buf);
        free(tree_buf);
        return;
    }

    int focused_wksp = find_focused_workspace(wksp_buf, wksp_len);
    if (focused_wksp < 0) focused_wksp = 1;

    int emacs_wksp = -1;
    bool emacs_found = false;
    char focused_class[256];
    parse_tree(tree_buf, tree_len, &emacs_wksp, &emacs_found, focused_class, sizeof(focused_class));

    bool emacs_visible = emacs_found && (emacs_wksp == focused_wksp);

    // Safety net: if Emacs is the focused window, it's obviously visible.
    // This catches any tree-parsing edge cases where workspace detection fails.
    if (!emacs_visible && strcmp(focused_class, "Emacs") == 0) {
        emacs_visible = true;
        if (emacs_wksp < 0) emacs_wksp = focused_wksp;
    }

    write_state(focused_wksp, emacs_visible, emacs_wksp, focused_class);

    free(wksp_buf);
    free(tree_buf);
}

int main() {
    // Set up signal handlers for clean shutdown
    signal(SIGTERM, signal_handler);
    signal(SIGINT, signal_handler);
    signal(SIGPIPE, SIG_IGN);

    // Discover i3 socket for host display
    if (!discover_i3_socket()) {
        // Try again after a delay — i3 might not be up yet
        sleep(5);
        discover_i3_socket();
    }

    // Write initial state
    update_state();

    // Main loop: subscribe to i3 events, update on each event
    while (g_running) {
        // Subscribe to workspace and window events
        char sub_cmd[512];
        build_i3_cmd(sub_cmd, sizeof(sub_cmd), "-t subscribe -m '[\"workspace\",\"window\"]'");
        FILE* sub = popen(sub_cmd, "r");
        if (!sub) {
            // i3 not available — retry after delay
            sleep(5);
            continue;
        }

        char line[4096];
        while (g_running && fgets(line, sizeof(line), sub)) {
            // Each line is a JSON event — we don't need to parse it,
            // just use it as a trigger to re-query state
            update_state();
        }

        pclose(sub);

        // If we got here, the subscription ended (i3 restart, etc.)
        // Rediscover socket (PID may have changed) and reconnect
        if (g_running) {
            sleep(2);
            discover_i3_socket();  // Re-discover in case i3 restarted
            update_state();
        }
    }

    // Cleanup: remove SHM file
    unlink(SHM_PATH);
    return 0;
}
