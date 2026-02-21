// tmux-tui-nav.cpp — Fast TUI navigation primitives for tmux panes
// Build: g++ -O2 -std=c++20 -o ~/.local/bin/tmux-tui-nav tmux-tui-nav.cpp

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <chrono>
#include <string>
#include <vector>
#include <regex>
#include <optional>
#include <thread>
#include <algorithm>
#include <unistd.h>
#include <sys/wait.h>

// ─── Timing ────────────────────────────────────────────────────────────────

using Clock = std::chrono::steady_clock;
using Ms = std::chrono::milliseconds;

static long elapsed_ms(Clock::time_point start) {
    return std::chrono::duration_cast<Ms>(Clock::now() - start).count();
}

// ─── Shell execution ───────────────────────────────────────────────────────

static std::string exec_cmd(const std::string& cmd, int* exit_code = nullptr) {
    std::string result;
    FILE* fp = popen(cmd.c_str(), "r");
    if (!fp) {
        if (exit_code) *exit_code = -1;
        return "";
    }
    char buf[4096];
    while (fgets(buf, sizeof(buf), fp)) {
        result += buf;
    }
    int status = pclose(fp);
    if (exit_code) {
        *exit_code = WIFEXITED(status) ? WEXITSTATUS(status) : -1;
    }
    return result;
}

// Escape a string for use inside single quotes in shell commands
static std::string shell_escape(const std::string& s) {
    std::string out = "'";
    for (char c : s) {
        if (c == '\'') out += "'\\''";
        else out += c;
    }
    out += "'";
    return out;
}

// ─── ANSI parsing state machine ────────────────────────────────────────────

struct AnsiAttrs {
    bool bold = false;
    bool reverse_video = false;
    bool has_bg_color = false;   // any background set (SGR 40-47, 48;5;N, 48;2;R;G;B)
    bool has_fg_color = false;
    int fg_color = -1;           // -1 = default. For 256-color: 0-255. For basic: 30-37.
    int bg_color = -1;
    bool dim = false;
    bool underline = false;
    bool italic = false;
};

// Count how many "interesting" formatting attributes are active
static int attr_weight(const AnsiAttrs& a) {
    int w = 0;
    if (a.bold) w += 2;
    if (a.reverse_video) w += 3;
    if (a.has_bg_color) w += 2;
    if (a.dim) w -= 1;  // dim is anti-selection
    return w;
}

struct ParsedLine {
    std::string raw;        // original line with ANSI
    std::string plain;      // ANSI-stripped text
    AnsiAttrs attrs;        // attributes active at start of visible content
    AnsiAttrs peak_attrs;   // highest-weight attrs seen anywhere on line
};

// Parse ANSI escape sequences and extract attributes + plain text
static ParsedLine parse_ansi_line(const std::string& line) {
    ParsedLine result;
    result.raw = line;

    AnsiAttrs current;
    bool first_visible = true;

    size_t i = 0;
    while (i < line.size()) {
        if (line[i] == '\033' && i + 1 < line.size() && line[i+1] == '[') {
            // CSI sequence: ESC [ <params> <final byte>
            i += 2;  // skip ESC [
            std::string params;
            while (i < line.size() && ((line[i] >= '0' && line[i] <= '9') || line[i] == ';' || line[i] == ':')) {
                params += line[i++];
            }
            char final_byte = (i < line.size()) ? line[i++] : 0;

            if (final_byte == 'm') {
                // SGR — parse semicolon-separated parameters
                if (params.empty()) {
                    current = AnsiAttrs{};  // reset
                } else {
                    std::vector<int> nums;
                    std::string num_str;
                    for (char c : params) {
                        if (c == ';' || c == ':') {
                            nums.push_back(num_str.empty() ? 0 : std::stoi(num_str));
                            num_str.clear();
                        } else {
                            num_str += c;
                        }
                    }
                    nums.push_back(num_str.empty() ? 0 : std::stoi(num_str));

                    for (size_t j = 0; j < nums.size(); j++) {
                        int n = nums[j];
                        if (n == 0) { current = AnsiAttrs{}; }
                        else if (n == 1) { current.bold = true; }
                        else if (n == 2) { current.dim = true; }
                        else if (n == 3) { current.italic = true; }
                        else if (n == 4) { current.underline = true; }
                        else if (n == 7) { current.reverse_video = true; }
                        else if (n == 22) { current.bold = false; current.dim = false; }
                        else if (n == 27) { current.reverse_video = false; }
                        else if (n >= 30 && n <= 37) {
                            current.has_fg_color = true;
                            current.fg_color = n;
                        }
                        else if (n == 38 && j + 1 < nums.size()) {
                            // Extended foreground: 38;5;N or 38;2;R;G;B
                            if (nums[j+1] == 5 && j + 2 < nums.size()) {
                                current.has_fg_color = true;
                                current.fg_color = nums[j+2];
                                j += 2;
                            } else if (nums[j+1] == 2 && j + 4 < nums.size()) {
                                current.has_fg_color = true;
                                current.fg_color = 256;  // sentinel for truecolor
                                j += 4;
                            }
                        }
                        else if (n == 39) { current.has_fg_color = false; current.fg_color = -1; }
                        else if (n >= 40 && n <= 47) {
                            current.has_bg_color = true;
                            current.bg_color = n;
                        }
                        else if (n == 48 && j + 1 < nums.size()) {
                            // Extended background: 48;5;N or 48;2;R;G;B
                            if (nums[j+1] == 5 && j + 2 < nums.size()) {
                                current.has_bg_color = true;
                                current.bg_color = nums[j+2];
                                j += 2;
                            } else if (nums[j+1] == 2 && j + 4 < nums.size()) {
                                current.has_bg_color = true;
                                current.bg_color = 256;
                                j += 4;
                            }
                        }
                        else if (n == 49) { current.has_bg_color = false; current.bg_color = -1; }
                        else if (n >= 90 && n <= 97) {
                            current.has_fg_color = true;
                            current.fg_color = n;
                        }
                        else if (n >= 100 && n <= 107) {
                            current.has_bg_color = true;
                            current.bg_color = n;
                        }
                    }
                }

                // Track peak attributes
                if (attr_weight(current) > attr_weight(result.peak_attrs)) {
                    result.peak_attrs = current;
                }
            }
            // Skip other CSI sequences (cursor movement, etc.)
        } else if (line[i] == '\033') {
            // Other escape sequences — skip ESC + next char
            i += 2;
        } else {
            // Visible character
            if (first_visible && line[i] != ' ' && line[i] != '\t') {
                result.attrs = current;
                first_visible = false;
            }
            result.plain += line[i];
            i++;
        }
    }

    return result;
}

// ─── Pane capture ──────────────────────────────────────────────────────────

static std::string capture_pane(const std::string& pane_id) {
    std::string cmd = "tmux capture-pane -t " + shell_escape(pane_id) + " -p -e";
    int rc;
    std::string out = exec_cmd(cmd, &rc);
    if (rc != 0) {
        fprintf(stderr, "error: tmux capture-pane failed (exit %d)\n", rc);
        return "";
    }
    return out;
}

// Get cursor position from tmux
static std::pair<int,int> get_cursor(const std::string& pane_id) {
    std::string cmd = "tmux display-message -t " + shell_escape(pane_id) +
                      " -p '#{cursor_y},#{cursor_x}'";
    std::string out = exec_cmd(cmd);
    int y = 0, x = 0;
    sscanf(out.c_str(), "%d,%d", &y, &x);
    return {y, x};
}

// Split output into lines
static std::vector<std::string> split_lines(const std::string& s) {
    std::vector<std::string> lines;
    std::string line;
    for (char c : s) {
        if (c == '\n') {
            lines.push_back(line);
            line.clear();
        } else {
            line += c;
        }
    }
    if (!line.empty()) lines.push_back(line);
    return lines;
}

// Trim trailing whitespace
static std::string rtrim(const std::string& s) {
    size_t end = s.find_last_not_of(" \t\r\n");
    return (end == std::string::npos) ? "" : s.substr(0, end + 1);
}

// Trim leading whitespace
static std::string ltrim(const std::string& s) {
    size_t start = s.find_first_not_of(" \t");
    return (start == std::string::npos) ? "" : s.substr(start);
}

static std::string trim(const std::string& s) {
    return ltrim(rtrim(s));
}

// ─── Menu/TUI state detection ──────────────────────────────────────────────

struct MenuItem {
    int line_number;
    std::string text;
    bool selected;
    int attr_weight_val;
};

struct PaneState {
    enum Type { TERMINAL, PROMPT, MENU, EDITOR };
    Type type = TERMINAL;

    // Common
    int total_lines = 0;
    int cursor_y = 0;
    int cursor_x = 0;

    // Menu
    std::vector<MenuItem> items;
    int selected_index = -1;
    std::string selected_text;

    // Prompt
    std::string prompt_text;

    // Editor
    std::string editor_name;
};

// Check if a line looks like an fzf/TUI item (has ▌ prefix or > prefix)
static bool has_item_prefix(const std::string& plain) {
    std::string t = ltrim(plain);
    // Check for ▌ (U+258C: 0xE2 0x96 0x8C)
    if (t.size() >= 3 && (unsigned char)t[0] == 0xE2 &&
        (unsigned char)t[1] == 0x96 && (unsigned char)t[2] == 0x8C) {
        return true;
    }
    // Check for > or ❯ prefix
    if (!t.empty() && (t[0] == '>' || t[0] == '*')) return true;
    if (t.size() >= 3 && (unsigned char)t[0] == 0xE2 &&
        (unsigned char)t[1] == 0x9D && (unsigned char)t[2] == 0xAF) {
        return true;
    }
    return false;
}

// Strip item prefix to get clean text
static std::string strip_item_prefix(const std::string& plain) {
    std::string t = ltrim(plain);
    // Strip ▌
    if (t.size() >= 3 && (unsigned char)t[0] == 0xE2 &&
        (unsigned char)t[1] == 0x96 && (unsigned char)t[2] == 0x8C) {
        return trim(t.substr(3));
    }
    // Strip > or *
    if (!t.empty() && (t[0] == '>' || t[0] == '*')) {
        return trim(t.substr(1));
    }
    // Strip ❯
    if (t.size() >= 3 && (unsigned char)t[0] == 0xE2 &&
        (unsigned char)t[1] == 0x9D && (unsigned char)t[2] == 0xAF) {
        return trim(t.substr(3));
    }
    return trim(t);
}

static PaneState analyze_pane(const std::string& pane_id, const std::string& raw_output) {
    PaneState state;
    auto lines = split_lines(raw_output);
    state.total_lines = (int)lines.size();

    auto [cy, cx] = get_cursor(pane_id);
    state.cursor_y = cy;
    state.cursor_x = cx;

    // Parse all lines
    std::vector<ParsedLine> parsed;
    parsed.reserve(lines.size());
    for (auto& l : lines) {
        parsed.push_back(parse_ansi_line(l));
    }

    // --- Detect menu/list items ---
    // Strategy 1: Look for lines with consistent item prefixes (▌, >, ❯)
    std::vector<int> prefixed_lines;
    for (int i = 0; i < (int)parsed.size(); i++) {
        if (has_item_prefix(parsed[i].plain) && !trim(parsed[i].plain).empty()) {
            prefixed_lines.push_back(i);
        }
    }

    // Strategy 2: Look for blocks of consistently-formatted lines
    // (same indentation, similar structure, 3+ consecutive)
    std::vector<int> formatted_lines;
    if (prefixed_lines.size() < 2) {
        // Look for consecutive non-empty lines with similar formatting
        for (int i = 0; i < (int)parsed.size(); i++) {
            std::string t = trim(parsed[i].plain);
            if (!t.empty() && (parsed[i].peak_attrs.has_fg_color ||
                               parsed[i].peak_attrs.bold ||
                               parsed[i].peak_attrs.reverse_video ||
                               parsed[i].peak_attrs.has_bg_color)) {
                formatted_lines.push_back(i);
            }
        }
    }

    // Decide which lines are menu items
    std::vector<int>& item_lines = prefixed_lines.size() >= 2 ? prefixed_lines : formatted_lines;

    if (item_lines.size() >= 2) {
        // Find consecutive groups
        std::vector<std::vector<int>> groups;
        std::vector<int> current_group = {item_lines[0]};
        for (size_t i = 1; i < item_lines.size(); i++) {
            if (item_lines[i] - item_lines[i-1] <= 2) {
                current_group.push_back(item_lines[i]);
            } else {
                if (current_group.size() >= 2) groups.push_back(current_group);
                current_group = {item_lines[i]};
            }
        }
        if (current_group.size() >= 2) groups.push_back(current_group);

        // Take the largest group
        std::vector<int>* best_group = nullptr;
        for (auto& g : groups) {
            if (!best_group || g.size() > best_group->size()) {
                best_group = &g;
            }
        }

        if (best_group && best_group->size() >= 2) {
            state.type = PaneState::MENU;

            // Calculate median attribute weight to find the baseline
            std::vector<int> weights;
            for (int idx : *best_group) {
                weights.push_back(attr_weight(parsed[idx].peak_attrs));
            }
            std::sort(weights.begin(), weights.end());
            int median_weight = weights[weights.size() / 2];

            int max_weight = -999;
            int max_weight_idx = -1;

            for (int idx : *best_group) {
                MenuItem item;
                item.line_number = idx;

                if (&item_lines == &prefixed_lines) {
                    item.text = strip_item_prefix(parsed[idx].plain);
                } else {
                    item.text = trim(parsed[idx].plain);
                }

                int w = attr_weight(parsed[idx].peak_attrs);
                item.attr_weight_val = w;
                item.selected = false;

                if (w > max_weight) {
                    max_weight = w;
                    max_weight_idx = (int)state.items.size();
                }

                state.items.push_back(item);
            }

            // The selected item is the one with highest attribute weight,
            // but only if it's notably higher than the median
            if (max_weight_idx >= 0 && max_weight > median_weight) {
                state.items[max_weight_idx].selected = true;
                state.selected_index = max_weight_idx;
                state.selected_text = state.items[max_weight_idx].text;
            }

            // Also check for reverse video specifically — strong selection signal
            for (int i = 0; i < (int)state.items.size(); i++) {
                int idx = (*best_group)[i];
                if (parsed[idx].peak_attrs.reverse_video) {
                    // Clear previous selection, set this one
                    for (auto& it : state.items) it.selected = false;
                    state.items[i].selected = true;
                    state.selected_index = i;
                    state.selected_text = state.items[i].text;
                    break;
                }
            }
        }
    }

    // --- Detect prompt ---
    if (state.type == PaneState::TERMINAL) {
        // Check last non-empty line for prompt patterns
        for (int i = (int)parsed.size() - 1; i >= 0; i--) {
            std::string t = rtrim(parsed[i].plain);
            if (t.empty()) continue;

            // Common prompt endings: $, #, >, ❯, %, ?
            // Also check if cursor is on this line
            if (i == state.cursor_y) {
                // Check for typical prompt patterns
                if (t.find('$') != std::string::npos ||
                    t.find('#') != std::string::npos ||
                    t.find('%') != std::string::npos ||
                    t.find("Select") != std::string::npos ||
                    t.find("select") != std::string::npos ||
                    t.find("Choose") != std::string::npos ||
                    t.find("choose") != std::string::npos ||
                    t.find(": ") != std::string::npos ||
                    t.find("? ") != std::string::npos ||
                    t.back() == '>' || t.back() == ':' || t.back() == '?') {
                    state.type = PaneState::PROMPT;
                    state.prompt_text = t;
                }
            }
            break;
        }
    }

    // --- Detect editor ---
    if (state.type == PaneState::TERMINAL) {
        for (int i = 0; i < std::min((int)parsed.size(), 3); i++) {
            std::string t = trim(parsed[i].plain);
            if (t.find("GNU nano") != std::string::npos) {
                state.type = PaneState::EDITOR;
                state.editor_name = "nano";
                break;
            }
            if (t.find("VIM") != std::string::npos || t.find("NVIM") != std::string::npos) {
                state.type = PaneState::EDITOR;
                state.editor_name = "vim";
                break;
            }
        }
        // Also check bottom lines for vim/nano status
        for (int i = std::max(0, (int)parsed.size() - 3); i < (int)parsed.size(); i++) {
            std::string t = trim(parsed[i].plain);
            if (t.find("-- INSERT --") != std::string::npos ||
                t.find("-- NORMAL --") != std::string::npos ||
                t.find("-- VISUAL --") != std::string::npos) {
                state.type = PaneState::EDITOR;
                state.editor_name = "vim";
                break;
            }
        }
    }

    return state;
}

// ─── Subcommands ───────────────────────────────────────────────────────────

static int cmd_capture(int argc, char** argv) {
    if (argc < 3) {
        fprintf(stderr, "usage: tmux-tui-nav capture <pane_id>\n");
        return 1;
    }
    std::string pane_id = argv[2];
    auto start = Clock::now();

    std::string output = capture_pane(pane_id);
    if (output.empty()) return 1;

    fputs(output.c_str(), stdout);
    fprintf(stderr, "captured in %ldms\n", elapsed_ms(start));
    return 0;
}

static int cmd_state(int argc, char** argv) {
    if (argc < 3) {
        fprintf(stderr, "usage: tmux-tui-nav state <pane_id>\n");
        return 1;
    }
    std::string pane_id = argv[2];
    auto start = Clock::now();

    std::string output = capture_pane(pane_id);
    if (output.empty()) return 1;

    PaneState state = analyze_pane(pane_id, output);

    switch (state.type) {
        case PaneState::MENU:
            printf("type=menu items=%d", (int)state.items.size());
            if (state.selected_index >= 0) {
                printf(" selected=%d selected_text=\"%s\"",
                       state.selected_index, state.selected_text.c_str());
            }
            printf("\n");
            // Print all items for reference
            for (int i = 0; i < (int)state.items.size(); i++) {
                printf("  [%d] %s%s\n", i, state.items[i].text.c_str(),
                       state.items[i].selected ? " *" : "");
            }
            break;

        case PaneState::PROMPT:
            printf("type=prompt text=\"%s\" cursor=%d,%d\n",
                   state.prompt_text.c_str(), state.cursor_y, state.cursor_x);
            break;

        case PaneState::EDITOR:
            printf("type=editor name=%s lines=%d cursor=%d,%d\n",
                   state.editor_name.c_str(), state.total_lines,
                   state.cursor_y, state.cursor_x);
            break;

        case PaneState::TERMINAL:
            printf("type=terminal lines=%d cursor=%d,%d\n",
                   state.total_lines, state.cursor_y, state.cursor_x);
            break;
    }

    fprintf(stderr, "analyzed in %ldms\n", elapsed_ms(start));
    return 0;
}

static int cmd_navigate(int argc, char** argv) {
    if (argc < 4) {
        fprintf(stderr, "usage: tmux-tui-nav navigate <pane_id> <target_regex>\n");
        return 1;
    }
    std::string pane_id = argv[2];
    std::string target_pattern = argv[3];
    auto start = Clock::now();

    std::regex target_re;
    try {
        target_re = std::regex(target_pattern, std::regex_constants::icase);
    } catch (const std::regex_error& e) {
        fprintf(stderr, "error: invalid regex '%s': %s\n", target_pattern.c_str(), e.what());
        return 1;
    }

    // Initial capture and analysis
    std::string output = capture_pane(pane_id);
    if (output.empty()) return 1;

    PaneState state = analyze_pane(pane_id, output);

    if (state.type != PaneState::MENU || state.items.empty()) {
        fprintf(stderr, "error: no menu detected in pane %s\n", pane_id.c_str());
        return 1;
    }

    // Find target item
    int target_idx = -1;
    for (int i = 0; i < (int)state.items.size(); i++) {
        if (std::regex_search(state.items[i].text, target_re)) {
            target_idx = i;
            break;
        }
    }

    if (target_idx < 0) {
        fprintf(stderr, "error: no item matching '%s' found in menu\n", target_pattern.c_str());
        fprintf(stderr, "available items:\n");
        for (auto& item : state.items) {
            fprintf(stderr, "  %s%s\n", item.text.c_str(), item.selected ? " (selected)" : "");
        }
        return 1;
    }

    // If already on target, just press Enter
    if (state.selected_index == target_idx) {
        std::string cmd = "tmux send-keys -t " + shell_escape(pane_id) + " Enter";
        exec_cmd(cmd);
        printf("navigated to \"%s\" in 0 steps, %ldms\n",
               state.items[target_idx].text.c_str(), elapsed_ms(start));
        return 0;
    }

    // Calculate direction and distance
    int current = state.selected_index >= 0 ? state.selected_index : 0;
    int steps = 0;
    int max_steps = (int)state.items.size() + 2;  // safety limit

    while (current != target_idx && steps < max_steps) {
        const char* key = (target_idx > current) ? "Down" : "Up";

        std::string cmd = "tmux send-keys -t " + shell_escape(pane_id) + " " + key;
        exec_cmd(cmd);
        steps++;

        // Wait for TUI to update
        std::this_thread::sleep_for(Ms(30));

        // Re-capture and verify movement
        output = capture_pane(pane_id);
        state = analyze_pane(pane_id, output);

        if (state.type == PaneState::MENU && state.selected_index >= 0) {
            current = state.selected_index;

            // Check if we've reached the target by regex match on current selection
            if (std::regex_search(state.selected_text, target_re)) {
                break;
            }
        } else {
            // Lost menu state — keep going based on calculated direction
            if (target_idx > current) current++;
            else current--;
        }
    }

    if (steps >= max_steps) {
        fprintf(stderr, "error: failed to navigate to target after %d steps\n", steps);
        return 1;
    }

    // Send Enter
    std::string cmd = "tmux send-keys -t " + shell_escape(pane_id) + " Enter";
    exec_cmd(cmd);

    printf("navigated to \"%s\" in %d steps, %ldms\n",
           state.selected_text.c_str(), steps, elapsed_ms(start));
    return 0;
}

static int cmd_wait_for(int argc, char** argv) {
    if (argc < 4) {
        fprintf(stderr, "usage: tmux-tui-nav wait-for <pane_id> <regex> [timeout_ms]\n");
        return 1;
    }
    std::string pane_id = argv[2];
    std::string pattern = argv[3];
    int timeout_ms = 5000;
    if (argc >= 5) {
        timeout_ms = std::atoi(argv[4]);
        if (timeout_ms <= 0) timeout_ms = 5000;
    }

    std::regex re;
    try {
        re = std::regex(pattern, std::regex_constants::icase);
    } catch (const std::regex_error& e) {
        fprintf(stderr, "error: invalid regex '%s': %s\n", pattern.c_str(), e.what());
        return 1;
    }

    auto start = Clock::now();

    while (elapsed_ms(start) < timeout_ms) {
        std::string output = capture_pane(pane_id);
        auto lines = split_lines(output);

        for (auto& line : lines) {
            auto parsed = parse_ansi_line(line);
            std::string t = trim(parsed.plain);
            if (!t.empty() && std::regex_search(t, re)) {
                printf("%s\n", t.c_str());
                fprintf(stderr, "matched in %ldms\n", elapsed_ms(start));
                return 0;
            }
        }

        std::this_thread::sleep_for(Ms(50));
    }

    fprintf(stderr, "timeout after %dms\n", timeout_ms);
    return 1;
}

static int cmd_send(int argc, char** argv) {
    if (argc < 4) {
        fprintf(stderr, "usage: tmux-tui-nav send <pane_id> <key> [key...]\n");
        return 1;
    }
    std::string pane_id = argv[2];
    auto start = Clock::now();

    for (int i = 3; i < argc; i++) {
        std::string key = argv[i];

        // Check if it's a special key or raw text
        bool is_special = (key == "Enter" || key == "Up" || key == "Down" ||
                          key == "Left" || key == "Right" || key == "Escape" ||
                          key == "Tab" || key == "Space" || key == "BSpace" ||
                          key == "Home" || key == "End" || key == "PageUp" ||
                          key == "PageDown" || key == "DC" ||
                          (key.size() >= 2 && key[0] == 'C' && key[1] == '-') ||
                          (key.size() >= 2 && key[0] == 'M' && key[1] == '-'));

        std::string cmd;
        if (is_special) {
            cmd = "tmux send-keys -t " + shell_escape(pane_id) + " " + key;
        } else {
            // Literal text — use -l flag
            cmd = "tmux send-keys -t " + shell_escape(pane_id) + " -l " + shell_escape(key);
        }

        int rc;
        exec_cmd(cmd, &rc);
        if (rc != 0) {
            fprintf(stderr, "error: send-keys failed for '%s' (exit %d)\n", key.c_str(), rc);
            return 1;
        }
    }

    fprintf(stderr, "sent %d key(s) in %ldms\n", argc - 3, elapsed_ms(start));
    return 0;
}

static int cmd_type(int argc, char** argv) {
    if (argc < 4) {
        fprintf(stderr, "usage: tmux-tui-nav type <pane_id> <text>\n");
        return 1;
    }
    std::string pane_id = argv[2];
    std::string text = argv[3];
    auto start = Clock::now();

    // Type character by character with 5ms delay
    for (size_t i = 0; i < text.size(); i++) {
        std::string ch(1, text[i]);
        std::string cmd = "tmux send-keys -t " + shell_escape(pane_id) + " -l " + shell_escape(ch);
        exec_cmd(cmd);
        std::this_thread::sleep_for(Ms(5));
    }

    // Wait for TUI to process
    std::this_thread::sleep_for(Ms(50));

    // Capture and show state
    std::string output = capture_pane(pane_id);
    PaneState state = analyze_pane(pane_id, output);

    fprintf(stderr, "typed %zu chars in %ldms\n", text.size(), elapsed_ms(start));

    // Print resulting state
    switch (state.type) {
        case PaneState::MENU:
            printf("type=menu items=%d", (int)state.items.size());
            if (state.selected_index >= 0) {
                printf(" selected=%d selected_text=\"%s\"",
                       state.selected_index, state.selected_text.c_str());
            }
            printf("\n");
            break;
        case PaneState::PROMPT:
            printf("type=prompt text=\"%s\"\n", state.prompt_text.c_str());
            break;
        case PaneState::EDITOR:
            printf("type=editor name=%s\n", state.editor_name.c_str());
            break;
        case PaneState::TERMINAL:
            printf("type=terminal lines=%d cursor=%d,%d\n",
                   state.total_lines, state.cursor_y, state.cursor_x);
            break;
    }

    return 0;
}

// ─── Main ──────────────────────────────────────────────────────────────────

static void print_usage() {
    fprintf(stderr,
        "tmux-tui-nav — Fast TUI navigation for tmux panes\n"
        "\n"
        "Usage:\n"
        "  tmux-tui-nav capture <pane_id>              Capture pane with ANSI\n"
        "  tmux-tui-nav state <pane_id>                Analyze pane state\n"
        "  tmux-tui-nav navigate <pane_id> <regex>     Navigate menu to item\n"
        "  tmux-tui-nav wait-for <pane_id> <regex> [timeout_ms]\n"
        "                                              Wait for content match\n"
        "  tmux-tui-nav send <pane_id> <key> [key...]  Send keystrokes\n"
        "  tmux-tui-nav type <pane_id> <text>          Type text char-by-char\n"
    );
}

int main(int argc, char** argv) {
    if (argc < 2) {
        print_usage();
        return 1;
    }

    std::string cmd = argv[1];

    if (cmd == "capture") return cmd_capture(argc, argv);
    if (cmd == "state") return cmd_state(argc, argv);
    if (cmd == "navigate") return cmd_navigate(argc, argv);
    if (cmd == "wait-for") return cmd_wait_for(argc, argv);
    if (cmd == "send") return cmd_send(argc, argv);
    if (cmd == "type") return cmd_type(argc, argv);

    fprintf(stderr, "error: unknown command '%s'\n", cmd.c_str());
    print_usage();
    return 1;
}
