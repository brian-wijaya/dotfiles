# Neovim Workflow Orientation Guide

Dense keybinding reference for your configuration. Every key listed should be pressed at least once.

**Legend:**
- `[CUSTOM]` = Defined in your dotfiles (overrides defaults)
- `[DEFAULT]` = Standard NvChad/Neovim/tmux binding
- `Prefix` = `Ctrl+Space` (your tmux prefix)
- `Leader` = `Space` (your Neovim leader key)

---

## STAGE 1: Terminal & Tmux

Your terminal auto-starts tmux when you open WezTerm.

### WezTerm Tab Management

| Keys | Source | Action |
|------|--------|--------|
| `Ctrl+Shift+T` | [CUSTOM] | New bash tab (bypasses tmux) |
| `Ctrl+Shift+N` | [CUSTOM] | New Neovim tab (bypasses tmux) |

### Tmux Session Management

| Keys | Source | Action |
|------|--------|--------|
| `Ctrl+Space` | [CUSTOM] | Tmux prefix (hold Ctrl, tap Space) |
| `Prefix d` | [DEFAULT] | Detach from session (exit without closing) |
| `Prefix s` | [DEFAULT] | List/switch sessions |
| `Prefix $` | [DEFAULT] | Rename current session |
| `Prefix (` | [DEFAULT] | Switch to previous session |
| `Prefix )` | [DEFAULT] | Switch to next session |

**Shell command:** `tmux attach -t main` - Reattach after detach

### Tmux Window Management

| Keys | Source | Action |
|------|--------|--------|
| `Prefix c` | [CUSTOM] | Create new window |
| `Prefix &` | [CUSTOM] | Kill current window |
| `Prefix 1` | [DEFAULT] | Switch to window 1 |
| `Prefix 2` | [DEFAULT] | Switch to window 2 |
| `Prefix n` | [DEFAULT] | Next window |
| `Prefix p` | [DEFAULT] | Previous window |
| `Prefix w` | [DEFAULT] | List windows (interactive picker) |
| `Prefix ,` | [DEFAULT] | Rename current window |

### Tmux Pane Management

| Keys | Source | Action |
|------|--------|--------|
| `Prefix \|` | [CUSTOM] | Split pane vertically (side by side) |
| `Prefix -` | [CUSTOM] | Split pane horizontally (top/bottom) |
| `Prefix h` | [CUSTOM] | Move to left pane |
| `Prefix j` | [CUSTOM] | Move to pane below |
| `Prefix k` | [CUSTOM] | Move to pane above |
| `Prefix l` | [CUSTOM] | Move to right pane |
| `Prefix x` | [CUSTOM] | Kill current pane |
| `Prefix <` | [CUSTOM] | Resize pane left (repeatable) |
| `Prefix >` | [CUSTOM] | Resize pane right (repeatable) |
| `Prefix +` | [CUSTOM] | Resize pane up (repeatable) |
| `Prefix =` | [CUSTOM] | Resize pane down (repeatable) |
| `Prefix z` | [DEFAULT] | Toggle pane zoom (fullscreen) |
| `Prefix q` | [DEFAULT] | Show pane numbers (then press number to jump) |
| `Prefix {` | [DEFAULT] | Swap pane with previous |
| `Prefix }` | [DEFAULT] | Swap pane with next |

### Tmux Copy Mode (vi-style)

| Keys | Source | Action |
|------|--------|--------|
| `Prefix [` | [DEFAULT] | Enter copy mode |
| `q` | [DEFAULT] | Exit copy mode |
| `h/j/k/l` | [CUSTOM] | Navigate in copy mode |
| `v` | [CUSTOM] | Begin selection |
| `y` | [CUSTOM] | Yank selection |
| `/` | [DEFAULT] | Search forward |
| `?` | [DEFAULT] | Search backward |
| `n` | [DEFAULT] | Next search result |
| `N` | [DEFAULT] | Previous search result |

---

## STAGE 2: File Management (NvimTree)

### Opening/Closing NvimTree

| Keys | Source | Action |
|------|--------|--------|
| `Ctrl+n` | [DEFAULT] | Toggle NvimTree open/close |
| `Leader e` | [DEFAULT] | Focus NvimTree (cursor jumps to tree) |

**Note:** `Leader` = tap `Space` once, then release and tap the next key.

### Navigating the Tree

| Keys | Source | Action |
|------|--------|--------|
| `j` | [DEFAULT] | Move cursor down |
| `k` | [DEFAULT] | Move cursor up |
| `Enter` | [DEFAULT] | Open file / expand folder |
| `o` | [DEFAULT] | Open file / expand folder (same as Enter) |
| `l` | [DEFAULT] | Open file / expand folder |
| `h` | [DEFAULT] | Close folder / go to parent |
| `Tab` | [DEFAULT] | Preview file (doesn't switch focus) |
| `Backspace` | [DEFAULT] | Close parent folder |
| `P` | [DEFAULT] | Go to parent directory |
| `-` | [DEFAULT] | Navigate up one directory |
| `Ctrl+]` | [DEFAULT] | cd into directory under cursor |

### File Operations

| Keys | Source | Action |
|------|--------|--------|
| `a` | [DEFAULT] | Create new file (add `/` at end for folder) |
| `r` | [DEFAULT] | Rename file/folder |
| `d` | [DEFAULT] | Delete file/folder |
| `x` | [DEFAULT] | Cut file/folder |
| `c` | [DEFAULT] | Copy file/folder |
| `p` | [DEFAULT] | Paste file/folder |
| `y` | [DEFAULT] | Copy filename to clipboard |
| `Y` | [DEFAULT] | Copy relative path to clipboard |
| `gy` | [DEFAULT] | Copy absolute path to clipboard |
| `R` | [DEFAULT] | Refresh tree |
| `H` | [DEFAULT] | Toggle hidden files (dotfiles) |
| `I` | [DEFAULT] | Toggle gitignored files |

### Marks and Bulk Operations

| Keys | Source | Action |
|------|--------|--------|
| `m` | [DEFAULT] | Toggle mark on file |
| `bd` | [DEFAULT] | Delete all marked files |
| `bmv` | [DEFAULT] | Move all marked files |

### Split Opening

| Keys | Source | Action |
|------|--------|--------|
| `v` | [DEFAULT] | Open in vertical split |
| `s` | [DEFAULT] | Open in horizontal split |
| `t` | [DEFAULT] | Open in new tab |

### Switching Focus

| Keys | Source | Action |
|------|--------|--------|
| `Ctrl+h` | [DEFAULT] | Move focus to left window (tree to editor) |
| `Ctrl+l` | [DEFAULT] | Move focus to right window (editor to tree) |
| `Ctrl+j` | [DEFAULT] | Move focus to window below |
| `Ctrl+k` | [DEFAULT] | Move focus to window above |

---

## STAGE 3: FZF & Shell Integration

### Shell FZF Commands (in bash/tmux pane)

| Keys | Source | Action |
|------|--------|--------|
| `Ctrl+r` | [DEFAULT] | Fuzzy search command history |
| `Ctrl+t` | [DEFAULT] | Fuzzy find file and insert path |
| `Alt+c` | [DEFAULT] | Fuzzy cd into directory |

### Zoxide (Smart Directory Jump)

**Shell commands:**
- `z <partial-name>` - Jump to frecent directory
- `zi` - Interactive directory picker with fzf
- `z -` - Jump to previous directory

---

## STAGE 4: Buffers, Windows & Tabs

### Buffer Navigation

| Keys | Source | Action |
|------|--------|--------|
| `Tab` | [DEFAULT] | Next buffer (in tabufline) |
| `Shift+Tab` | [DEFAULT] | Previous buffer |
| `Leader x` | [DEFAULT] | Close current buffer |
| `Leader b` | [DEFAULT] | New buffer |

### Window/Split Management

| Keys | Source | Action |
|------|--------|--------|
| `Ctrl+h` | [DEFAULT] | Move to left window |
| `Ctrl+j` | [DEFAULT] | Move to window below |
| `Ctrl+k` | [DEFAULT] | Move to window above |
| `Ctrl+l` | [DEFAULT] | Move to right window |
| `:vsp` | [DEFAULT] | Create vertical split (command) |
| `:sp` | [DEFAULT] | Create horizontal split (command) |
| `Ctrl+w v` | [DEFAULT] | Create vertical split |
| `Ctrl+w s` | [DEFAULT] | Create horizontal split |
| `Ctrl+w q` | [DEFAULT] | Close current window |
| `Ctrl+w o` | [DEFAULT] | Close all windows except current |
| `Ctrl+w =` | [DEFAULT] | Make all windows equal size |
| `Ctrl+w >` | [DEFAULT] | Increase window width |
| `Ctrl+w <` | [DEFAULT] | Decrease window width |
| `Ctrl+w +` | [DEFAULT] | Increase window height |
| `Ctrl+w -` | [DEFAULT] | Decrease window height |

---

## STAGE 5: Telescope

### File Finding

| Keys | Source | Action |
|------|--------|--------|
| `Leader f f` | [DEFAULT] | Find files in project |
| `Leader f a` | [DEFAULT] | Find all files (including hidden) |
| `Leader f o` | [DEFAULT] | Find recently opened files (oldfiles) |
| `Leader f b` | [DEFAULT] | Find in open buffers |
| `Leader f z` | [DEFAULT] | Find in current buffer (fuzzy) |

### Text Search

| Keys | Source | Action |
|------|--------|--------|
| `Leader f w` | [DEFAULT] | Live grep (search text in project) |
| `Leader f W` | [DEFAULT] | Grep word under cursor |

### Telescope Pickers

| Keys | Source | Action |
|------|--------|--------|
| `Leader f h` | [DEFAULT] | Help tags |
| `Leader g t` | [DEFAULT] | Git status |
| `Leader g c` | [DEFAULT] | Git commits |
| `Leader p t` | [DEFAULT] | Pick hidden terminal |
| `Leader m a` | [DEFAULT] | Find marks |
| `Leader c m` | [DEFAULT] | Git commits |

### Theme & UI

| Keys | Source | Action |
|------|--------|--------|
| `Leader t h` | [DEFAULT] | Theme picker (preview themes!) |

### Inside Telescope Picker

| Keys | Source | Action |
|------|--------|--------|
| `Ctrl+n` | [DEFAULT] | Move to next item |
| `Ctrl+p` | [DEFAULT] | Move to previous item |
| `j` / `k` | [DEFAULT] | Navigate (in normal mode) |
| `Enter` | [DEFAULT] | Select item |
| `Ctrl+x` | [DEFAULT] | Open in horizontal split |
| `Ctrl+v` | [DEFAULT] | Open in vertical split |
| `Ctrl+t` | [DEFAULT] | Open in new tab |
| `Ctrl+u` | [DEFAULT] | Scroll preview up |
| `Ctrl+d` | [DEFAULT] | Scroll preview down |
| `Esc` | [DEFAULT] | Close picker (in insert mode) |
| `q` | [DEFAULT] | Close picker (in normal mode) |

---

## STAGE 6: LSP (Language Server)

### Code Navigation

| Keys | Source | Action |
|------|--------|--------|
| `gd` | [DEFAULT] | Go to definition |
| `gD` | [DEFAULT] | Go to declaration |
| `gi` | [DEFAULT] | Go to implementation |
| `gr` | [DEFAULT] | Go to references |
| `K` | [DEFAULT] | Hover documentation |
| `Ctrl+k` | [DEFAULT] | Signature help (in insert mode) |

### Code Actions

| Keys | Source | Action |
|------|--------|--------|
| `Leader c a` | [DEFAULT] | Code actions |
| `Leader r a` | [DEFAULT] | LSP rename symbol |
| `Leader f m` | [DEFAULT] | Format file |

### Diagnostics

| Keys | Source | Action |
|------|--------|--------|
| `[d` | [DEFAULT] | Previous diagnostic |
| `]d` | [DEFAULT] | Next diagnostic |
| `Leader d` | [DEFAULT] | Floating diagnostic |
| `Leader q` | [DEFAULT] | Diagnostic loclist |

---

## STAGE 7: Modal Editing Essentials

### Your Custom Mappings

| Keys | Source | Action |
|------|--------|--------|
| `;` | [CUSTOM] | Enter command mode (instead of `:`) |
| `jk` | [CUSTOM] | Exit insert mode (instead of `Esc`) |

### Modes

| Keys | Source | Action |
|------|--------|--------|
| `i` | [DEFAULT] | Insert mode (before cursor) |
| `a` | [DEFAULT] | Insert mode (after cursor) |
| `I` | [DEFAULT] | Insert at line beginning |
| `A` | [DEFAULT] | Insert at line end |
| `o` | [DEFAULT] | Insert on new line below |
| `O` | [DEFAULT] | Insert on new line above |
| `v` | [DEFAULT] | Visual mode (character) |
| `V` | [DEFAULT] | Visual line mode |
| `Ctrl+v` | [DEFAULT] | Visual block mode |
| `Esc` | [DEFAULT] | Return to normal mode |
| `jk` | [CUSTOM] | Return to normal mode (from insert) |

### Movement

| Keys | Source | Action |
|------|--------|--------|
| `h` | [DEFAULT] | Left |
| `j` | [DEFAULT] | Down |
| `k` | [DEFAULT] | Up |
| `l` | [DEFAULT] | Right |
| `w` | [DEFAULT] | Next word start |
| `W` | [DEFAULT] | Next WORD start (whitespace delimited) |
| `b` | [DEFAULT] | Previous word start |
| `B` | [DEFAULT] | Previous WORD start |
| `e` | [DEFAULT] | Next word end |
| `E` | [DEFAULT] | Next WORD end |
| `0` | [DEFAULT] | Line beginning |
| `^` | [DEFAULT] | First non-whitespace |
| `$` | [DEFAULT] | Line end |
| `gg` | [DEFAULT] | File beginning |
| `G` | [DEFAULT] | File end |
| `{` | [DEFAULT] | Previous paragraph |
| `}` | [DEFAULT] | Next paragraph |
| `%` | [DEFAULT] | Matching bracket |
| `f{char}` | [DEFAULT] | Jump to next {char} |
| `F{char}` | [DEFAULT] | Jump to previous {char} |
| `t{char}` | [DEFAULT] | Jump to before next {char} |
| `T{char}` | [DEFAULT] | Jump to after previous {char} |
| `;` | [DEFAULT] | Repeat f/t forward (overridden in your config) |
| `,` | [DEFAULT] | Repeat f/t backward |
| `Ctrl+d` | [DEFAULT] | Half page down |
| `Ctrl+u` | [DEFAULT] | Half page up |
| `Ctrl+f` | [DEFAULT] | Full page down |
| `Ctrl+b` | [DEFAULT] | Full page up |
| `zz` | [DEFAULT] | Center cursor line |
| `zt` | [DEFAULT] | Cursor line to top |
| `zb` | [DEFAULT] | Cursor line to bottom |

### Editing

| Keys | Source | Action |
|------|--------|--------|
| `x` | [DEFAULT] | Delete character |
| `X` | [DEFAULT] | Delete character before cursor |
| `r{char}` | [DEFAULT] | Replace character with {char} |
| `s` | [DEFAULT] | Delete character and insert |
| `S` | [DEFAULT] | Delete line and insert |
| `dd` | [DEFAULT] | Delete line |
| `D` | [DEFAULT] | Delete to line end |
| `yy` | [DEFAULT] | Yank (copy) line |
| `Y` | [DEFAULT] | Yank to line end |
| `p` | [DEFAULT] | Paste after |
| `P` | [DEFAULT] | Paste before |
| `u` | [DEFAULT] | Undo |
| `Ctrl+r` | [DEFAULT] | Redo |
| `.` | [DEFAULT] | Repeat last change |
| `~` | [DEFAULT] | Toggle case |
| `>>` | [DEFAULT] | Indent line |
| `<<` | [DEFAULT] | Unindent line |
| `J` | [DEFAULT] | Join lines |

### Operators (combine with motion)

| Keys | Source | Action |
|------|--------|--------|
| `d{motion}` | [DEFAULT] | Delete |
| `c{motion}` | [DEFAULT] | Change (delete and insert) |
| `y{motion}` | [DEFAULT] | Yank |
| `>>{motion}` | [DEFAULT] | Indent |
| `<<{motion}` | [DEFAULT] | Unindent |

**Examples:**
- `dw` = delete word
- `ciw` = change inner word
- `ci"` = change inside quotes
- `ct)` = change to closing paren
- `d$` = delete to end of line
- `y2j` = yank current and 2 lines below

### Text Objects

| Keys | Source | Action |
|------|--------|--------|
| `iw` | [DEFAULT] | Inner word |
| `aw` | [DEFAULT] | A word (includes space) |
| `i"` | [DEFAULT] | Inside double quotes |
| `a"` | [DEFAULT] | Around double quotes |
| `i'` | [DEFAULT] | Inside single quotes |
| `i(` or `ib` | [DEFAULT] | Inside parentheses |
| `a(` or `ab` | [DEFAULT] | Around parentheses |
| `i{` or `iB` | [DEFAULT] | Inside braces |
| `i[` | [DEFAULT] | Inside brackets |
| `it` | [DEFAULT] | Inside HTML tag |
| `ip` | [DEFAULT] | Inner paragraph |

### Search

| Keys | Source | Action |
|------|--------|--------|
| `/{pattern}` | [DEFAULT] | Search forward |
| `?{pattern}` | [DEFAULT] | Search backward |
| `n` | [DEFAULT] | Next search result |
| `N` | [DEFAULT] | Previous search result |
| `*` | [DEFAULT] | Search word under cursor forward |
| `#` | [DEFAULT] | Search word under cursor backward |
| `Leader n` | [DEFAULT] | Clear search highlight |

### Marks

| Keys | Source | Action |
|------|--------|--------|
| `m{a-z}` | [DEFAULT] | Set mark |
| `'{a-z}` | [DEFAULT] | Jump to mark line |
| `` `{a-z} `` | [DEFAULT] | Jump to mark position |
| `''` | [DEFAULT] | Jump to last position |

---

## STAGE 8: Utilities & Terminal

### Built-in Terminal

| Keys | Source | Action |
|------|--------|--------|
| `Leader h` | [DEFAULT] | Horizontal terminal |
| `Leader v` | [DEFAULT] | Vertical terminal |
| `Alt+h` | [DEFAULT] | Toggle horizontal terminal |
| `Alt+v` | [DEFAULT] | Toggle vertical terminal |
| `Alt+i` | [DEFAULT] | Toggle floating terminal |
| `Ctrl+x` | [DEFAULT] | Exit terminal mode (to normal) |

### Cheatsheet & Help

| Keys | Source | Action |
|------|--------|--------|
| `Leader c h` | [DEFAULT] | Open NvChad cheatsheet |
| `:Telescope keymaps` | [DEFAULT] | Searchable keybindings |
| `:h {topic}` | [DEFAULT] | Neovim help |

### Line Numbers

| Keys | Source | Action |
|------|--------|--------|
| `Leader n` | [DEFAULT] | Toggle line numbers |
| `Leader r n` | [DEFAULT] | Toggle relative line numbers |

### WhichKey (built-in helper)

Press `Leader` (Space) and wait 500ms - a popup shows available keybindings.

---

## Quick Reference: Most Important Keys

### "I need to..."

| Task | Keys |
|------|------|
| Open file tree | `Ctrl+n` |
| Find file by name | `Leader f f` |
| Search text in project | `Leader f w` |
| Go to definition | `gd` |
| Switch buffer | `Tab` / `Shift+Tab` |
| Close buffer | `Leader x` |
| Save file | `:w` or `; w Enter` |
| Quit | `:q` or `; q Enter` |
| Split vertical | `Ctrl+w v` |
| Move between windows | `Ctrl+h/j/k/l` |
| Change theme | `Leader t h` |
| Open cheatsheet | `Leader c h` |

### "In tmux I need to..."

| Task | Keys |
|------|------|
| New pane right | `Prefix \|` |
| New pane below | `Prefix -` |
| Move between panes | `Prefix h/j/k/l` |
| New window | `Prefix c` |
| Switch window | `Prefix 1/2/3...` |
| Close pane | `Prefix x` |
| Detach session | `Prefix d` |
| Reattach session | `tmux attach -t main` (shell) |

---

## Practice Drills

### Drill 1: Tmux Navigation
1. `Prefix |` - split right
2. `Prefix -` - split below (now 3 panes)
3. `Prefix h` - go left
4. `Prefix l` - go right
5. `Prefix j` - go down
6. `Prefix z` - zoom current pane
7. `Prefix z` - unzoom
8. `Prefix x` - kill pane
9. `Prefix x` - kill another

### Drill 2: File Operations
1. `Ctrl+n` - open tree
2. `j j j` - move down
3. `a` - create file, type `test.txt`, Enter
4. `Enter` - open file
5. `i` - insert mode
6. Type something
7. `jk` - back to normal mode
8. `;w Enter` - save
9. `Ctrl+n` - back to tree
10. Navigate to test.txt, press `d` - delete
11. `y` - confirm

### Drill 3: Telescope Workflow
1. `Leader f f` - find files
2. Type partial filename
3. `Ctrl+n` / `Ctrl+p` - navigate results
4. `Enter` - open
5. `Leader f w` - grep project
6. Type search term
7. `Enter` - jump to match

### Drill 4: Buffer Workflow
1. `Leader f f` - open file 1
2. `Leader f f` - open file 2
3. `Leader f f` - open file 3
4. `Tab` - cycle forward
5. `Shift+Tab` - cycle back
6. `Leader x` - close current
7. `Tab` - to next

### Drill 5: Modal Editing
1. `gg` - go to top
2. `G` - go to bottom
3. `Ctrl+u` - half page up
4. `/function` - search for "function"
5. `n` - next match
6. `N` - previous match
7. `ciw` - change word under cursor
8. Type new word
9. `jk` - back to normal
10. `.` - repeat on another word

---

*Generated from your dotfiles configuration. When in doubt: `Leader c h`*
