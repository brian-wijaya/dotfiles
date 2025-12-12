# Neovim Workflow Orientation Guide — Exhaustive Edition

Every keybinding you should know, organized by tool and source.

**Legend — Where each mapping comes from:**
- `[.wezterm.lua]` = Your WezTerm config (in .config/wezterm/)
- `[tmux.conf]` = Your tmux config (in .config/tmux/)
- `[mappings.lua]` = Your Neovim mappings
- `[TMUX]` = Tmux default
- `[NVCHAD]` = NvChad default mapping
- `[NVIMTREE]` = NvimTree plugin default
- `[TELESCOPE]` = Telescope plugin default
- `[NEOVIM]` = Vanilla Neovim/Vim default
- `[FZF]` = FZF shell integration default
- `[ZOXIDE]` = Zoxide default
- `[flash.lua]` = flash.nvim plugin config
- `[harpoon.lua]` = harpoon plugin config
- `[lazygit.lua]` = lazygit.nvim plugin config
- `[trouble.lua]` = trouble.nvim plugin config
- `[todo-comments.lua]` = todo-comments.nvim plugin config
- `[refactoring.lua]` = refactoring.nvim plugin config
- `[oil.lua]` = oil.nvim plugin config
- `[grug-far.lua]` = grug-far.nvim plugin config
- `[gitlinker.lua]` = gitlinker.nvim plugin config
- `[ufo.lua]` = nvim-ufo plugin config
- `[neocodeium.lua]` = neocodeium plugin config
- `[vim-repeat]` = tpope/vim-repeat plugin (makes commands repeatable)
- `[utils/repeatable.lua]` = custom repeatable command utility
- `[init.lua]` = plugins/init.lua (inline plugin configs)

**Prefix** = `Ctrl+Space` (your tmux prefix, defined in tmux.conf)
**Leader** = `Space` (NvChad default)

---

## STAGE 1: Terminal & Tmux

Your terminal auto-starts tmux when you open WezTerm (configured in .bashrc).

### WezTerm Tab Management

| Keys | Source | Action |
|------|--------|--------|
| `Ctrl+Shift+T` | [.wezterm.lua] | New bash tab (bypasses tmux) |
| `Ctrl+Shift+N` | [.wezterm.lua] | New Neovim tab (bypasses tmux) |

### Tmux — Your Custom Bindings (tmux.conf)

| Keys | Source | Action |
|------|--------|--------|
| `Ctrl+Space` | [tmux.conf] | Tmux prefix (replaces default `Ctrl+b`) |
| `Prefix \|` | [tmux.conf] | Split pane vertically (replaces `%`) |
| `Prefix -` | [tmux.conf] | Split pane horizontally (replaces `"`) |
| `Prefix h` | [tmux.conf] | Select pane left (vim-style backup) |
| `Prefix j` | [tmux.conf] | Select pane down (vim-style backup) |
| `Prefix k` | [tmux.conf] | Select pane up (vim-style backup) |
| `Prefix l` | [tmux.conf] | Select pane right (vim-style backup) |
| `Prefix <` | [tmux.conf] | Resize pane left 5 units (repeatable) |
| `Prefix >` | [tmux.conf] | Resize pane right 5 units (repeatable) |
| `Prefix +` | [tmux.conf] | Resize pane up 2 units (repeatable) |
| `Prefix =` | [tmux.conf] | Resize pane down 2 units (repeatable) |
| `Prefix c` | [tmux.conf] | New window (rebind) |
| `Prefix x` | [tmux.conf] | Kill pane (rebind) |
| `Prefix &` | [tmux.conf] | Kill window (rebind) |
| `Prefix r` | [tmux.conf] | Reload tmux config |
| `Prefix w` | [tmux.conf] | Open workflow guide in new pane |
| `v` (copy-mode) | [tmux.conf] | Begin selection |
| `y` (copy-mode) | [tmux.conf] | Yank selection |
| `Prefix ?` | [tmux-which-key] | Show tmux keybindings menu (which-key) |

### Seamless Navigation (vim-tmux-navigator)

These keys work the same in both Neovim and tmux panes — press once to navigate regardless of where you are.

| Keys | Source | Action |
|------|--------|--------|
| `Ctrl+h` | [tmux.conf] + [mappings.lua] | Navigate left (works across vim/tmux) |
| `Ctrl+j` | [tmux.conf] + [mappings.lua] | Navigate down (works across vim/tmux) |
| `Ctrl+k` | [tmux.conf] + [mappings.lua] | Navigate up (works across vim/tmux) |
| `Ctrl+l` | [tmux.conf] + [mappings.lua] | Navigate right (works across vim/tmux) |

**How it works:** When you press `Ctrl+h`, tmux checks if the current pane is running Neovim. If yes, it sends the key to Neovim. If no, it handles the navigation itself. This creates seamless movement across your entire terminal environment.

### Tmux — All Default Bindings

#### Session Management

| Keys | Source | Action |
|------|--------|--------|
| `Prefix d` | [TMUX] | Detach client |
| `Prefix s` | [TMUX] | Choose session interactively |
| `Prefix $` | [TMUX] | Rename current session |
| `Prefix (` | [TMUX] | Switch to previous session |
| `Prefix )` | [TMUX] | Switch to next session |
| `Prefix L` | [TMUX] | Switch to last session |
| `Prefix D` | [TMUX] | Choose client to detach |

#### Window Management

| Keys | Source | Action |
|------|--------|--------|
| `Prefix c` | [TMUX] | Create new window |
| `Prefix &` | [TMUX] | Kill current window (with confirm) |
| `Prefix ,` | [TMUX] | Rename current window |
| `Prefix .` | [TMUX] | Move window (prompt for index) |
| `Prefix 0-9` | [TMUX] | Select window 0-9 |
| `Prefix '` | [TMUX] | Select window by index (prompt) |
| `Prefix n` | [TMUX] | Next window |
| `Prefix p` | [TMUX] | Previous window |
| `Prefix l` | [TMUX] | Last (previously selected) window |
| `Prefix w` | [TMUX] | Choose window interactively |
| `Prefix f` | [TMUX] | Find window by name |
| `Prefix i` | [TMUX] | Display window info |

#### Pane Management

| Keys | Source | Action |
|------|--------|--------|
| `Prefix "` | [TMUX] | Split horizontally (top/bottom) |
| `Prefix %` | [TMUX] | Split vertically (left/right) |
| `Prefix x` | [TMUX] | Kill current pane (with confirm) |
| `Prefix !` | [TMUX] | Break pane into its own window |
| `Prefix q` | [TMUX] | Display pane numbers (press number to jump) |
| `Prefix o` | [TMUX] | Select next pane |
| `Prefix ;` | [TMUX] | Select last (previously active) pane |
| `Prefix {` | [TMUX] | Swap pane with previous |
| `Prefix }` | [TMUX] | Swap pane with next |
| `Prefix z` | [TMUX] | Toggle pane zoom (fullscreen) |
| `Prefix Space` | [TMUX] | Cycle through pane layouts |
| `Prefix Arrow` | [TMUX] | Select pane in direction |
| `Prefix C-Arrow` | [TMUX] | Resize pane in direction |
| `Prefix M-Arrow` | [TMUX] | Resize pane in direction (larger) |
| `Prefix C-o` | [TMUX] | Rotate panes forward |
| `Prefix M-o` | [TMUX] | Rotate panes backward |

#### Layouts

| Keys | Source | Action |
|------|--------|--------|
| `Prefix M-1` | [TMUX] | Even-horizontal layout |
| `Prefix M-2` | [TMUX] | Even-vertical layout |
| `Prefix M-3` | [TMUX] | Main-horizontal layout |
| `Prefix M-4` | [TMUX] | Main-vertical layout |
| `Prefix M-5` | [TMUX] | Tiled layout |

#### Copy Mode & Buffers

| Keys | Source | Action |
|------|--------|--------|
| `Prefix [` | [TMUX] | Enter copy mode |
| `Prefix ]` | [TMUX] | Paste most recent buffer |
| `Prefix #` | [TMUX] | List all paste buffers |
| `Prefix =` | [TMUX] | Choose buffer to paste interactively |
| `Prefix -` | [TMUX] | Delete most recent buffer |
| `Prefix PgUp` | [TMUX] | Enter copy mode and scroll up |

#### Copy Mode Navigation (vi-style, enabled in tmux.conf)

| Keys | Source | Action |
|------|--------|--------|
| `h/j/k/l` | [tmux.conf] | Move cursor |
| `w/b/e` | [TMUX] | Word motions |
| `0/$` | [TMUX] | Line start/end |
| `g/G` | [TMUX] | Top/bottom of buffer |
| `Ctrl+u/d` | [TMUX] | Half-page up/down |
| `Ctrl+b/f` | [TMUX] | Full page up/down |
| `/` | [TMUX] | Search forward |
| `?` | [TMUX] | Search backward |
| `n/N` | [TMUX] | Next/previous search result |
| `v` | [tmux.conf] | Begin selection |
| `y` | [tmux.conf] | Yank selection |
| `q` | [TMUX] | Exit copy mode |

#### Miscellaneous

| Keys | Source | Action |
|------|--------|--------|
| `Prefix :` | [TMUX] | Command prompt |
| `Prefix ?` | [TMUX] | List all key bindings |
| `Prefix t` | [TMUX] | Show clock |
| `Prefix ~` | [TMUX] | Show messages |
| `Prefix r` | [TMUX] | Refresh client |
| `Prefix C-z` | [TMUX] | Suspend tmux client |

**Shell command:** `tmux attach -t main` — Reattach to session after detach

---

## STAGE 2: File Management (NvimTree) — Complete

### Opening/Closing

| Keys | Source | Action |
|------|--------|--------|
| `Ctrl+n` | [NVCHAD] | Toggle NvimTree |
| `Leader e` | [NVCHAD] | Focus NvimTree |
| `q` | [NVIMTREE] | Close NvimTree |
| `g?` | [NVIMTREE] | Show help (all keybindings) |

### Navigation

| Keys | Source | Action |
|------|--------|--------|
| `j` | [NVIMTREE] | Move cursor down |
| `k` | [NVIMTREE] | Move cursor up |
| `Enter` | [NVIMTREE] | Open file / expand folder |
| `o` | [NVIMTREE] | Open file / expand folder |
| `l` | [NVIMTREE] | Open file / expand folder |
| `h` | [NVIMTREE] | Close folder / go to parent |
| `Tab` | [NVIMTREE] | Preview file (keeps cursor in tree) |
| `Backspace` | [NVIMTREE] | Close parent folder |
| `P` | [NVIMTREE] | Go to parent directory |
| `-` | [NVIMTREE] | Navigate up one directory |
| `>` | [NVIMTREE] | Navigate into directory |
| `<` | [NVIMTREE] | Navigate to parent directory |
| `Ctrl+]` | [NVIMTREE] | CD into directory under cursor |
| `J` | [NVIMTREE] | Move to last sibling |
| `K` | [NVIMTREE] | Move to first sibling |

### Opening Files

| Keys | Source | Action |
|------|--------|--------|
| `<CR>` / `o` | [NVIMTREE] | Open file |
| `v` | [NVIMTREE] | Open in vertical split |
| `s` | [NVIMTREE] | Open in horizontal split |
| `Ctrl+v` | [NVIMTREE] | Open in vertical split |
| `Ctrl+x` | [NVIMTREE] | Open in horizontal split |
| `Ctrl+t` | [NVIMTREE] | Open in new tab |
| `t` | [NVIMTREE] | Open in new tab |
| `O` | [NVIMTREE] | Open with system application |

### File Operations

| Keys | Source | Action |
|------|--------|--------|
| `a` | [NVIMTREE] | Create file/folder (add `/` for folder) |
| `r` | [NVIMTREE] | Rename |
| `Ctrl+r` | [NVIMTREE] | Rename (omitting current name) |
| `d` | [NVIMTREE] | Delete |
| `D` | [NVIMTREE] | Trash (if trash enabled) |
| `x` | [NVIMTREE] | Cut |
| `c` | [NVIMTREE] | Copy |
| `p` | [NVIMTREE] | Paste |
| `y` | [NVIMTREE] | Copy filename |
| `Y` | [NVIMTREE] | Copy relative path |
| `gy` | [NVIMTREE] | Copy absolute path |

### Marks & Bulk Operations

| Keys | Source | Action |
|------|--------|--------|
| `m` | [NVIMTREE] | Toggle mark |
| `bd` | [NVIMTREE] | Delete bookmarked |
| `bt` | [NVIMTREE] | Trash bookmarked |
| `bmv` | [NVIMTREE] | Move bookmarked |

### Filtering & Display

| Keys | Source | Action |
|------|--------|--------|
| `H` | [NVIMTREE] | Toggle hidden files (dotfiles) |
| `I` | [NVIMTREE] | Toggle gitignored files |
| `U` | [NVIMTREE] | Toggle custom filter |
| `C` | [NVIMTREE] | Toggle git clean filter |
| `B` | [NVIMTREE] | Toggle no-buffer filter |
| `f` | [NVIMTREE] | Filter: live filter |
| `F` | [NVIMTREE] | Filter: clear |
| `E` | [NVIMTREE] | Expand all |
| `W` | [NVIMTREE] | Collapse all |
| `R` | [NVIMTREE] | Refresh |
| `S` | [NVIMTREE] | Search |

### Info & Git

| Keys | Source | Action |
|------|--------|--------|
| `Ctrl+k` | [NVIMTREE] | Show file info |
| `[c` | [NVIMTREE] | Previous git item |
| `]c` | [NVIMTREE] | Next git item |
| `[e` | [NVIMTREE] | Previous diagnostic |
| `]e` | [NVIMTREE] | Next diagnostic |

### Focus Switching (vim-tmux-navigator)

| Keys | Source | Action |
|------|--------|--------|
| `Ctrl+h` | [mappings.lua] | Move focus left (seamless vim/tmux) |
| `Ctrl+l` | [mappings.lua] | Move focus right (seamless vim/tmux) |
| `Ctrl+j` | [mappings.lua] | Move focus down (seamless vim/tmux) |
| `Ctrl+k` | [mappings.lua] | Move focus up (seamless vim/tmux) |

**Note:** These override NvChad defaults to enable seamless navigation between Neovim windows and tmux panes.

---

## STAGE 3: FZF & Shell Integration

### Shell FZF Commands (in bash/tmux pane)

| Keys | Source | Action |
|------|--------|--------|
| `Ctrl+r` | [FZF] | Fuzzy search command history |
| `Ctrl+t` | [FZF] | Fuzzy find file, insert path at cursor |
| `Alt+c` | [FZF] | Fuzzy find directory and cd into it |

### Zoxide (Smart Directory Jump)

| Command | Source | Action |
|---------|--------|--------|
| `z <partial>` | [ZOXIDE] | Jump to frecent directory matching partial |
| `z -` | [ZOXIDE] | Jump to previous directory |
| `zi` | [ZOXIDE] | Interactive directory picker with fzf |
| `z ..` | [ZOXIDE] | Go up one directory |
| `zq <path>` | [ZOXIDE] | Add path to database |
| `zr <path>` | [ZOXIDE] | Remove path from database |

---

## STAGE 4: Buffers, Windows & Tabs — Complete

### Buffer Navigation

| Keys | Source | Action |
|------|--------|--------|
| `Tab` | [NVCHAD] | Next buffer |
| `Shift+Tab` | [NVCHAD] | Previous buffer |
| `Leader x` | [NVCHAD] | Close current buffer |
| `Leader b` | [NVCHAD] | New buffer |
| `:ls` | [NEOVIM] | List all buffers |
| `:ls!` | [NEOVIM] | List all buffers (including unlisted) |
| `:b <num>` | [NEOVIM] | Go to buffer number |
| `:b <name>` | [NEOVIM] | Go to buffer by partial name |
| `:bn` | [NEOVIM] | Next buffer |
| `:bp` | [NEOVIM] | Previous buffer |
| `:bf` | [NEOVIM] | First buffer |
| `:bl` | [NEOVIM] | Last buffer |
| `:bd` | [NEOVIM] | Delete (close) buffer |
| `:bd!` | [NEOVIM] | Force delete buffer |
| `:bw` | [NEOVIM] | Wipe buffer |
| `Ctrl+^` | [NEOVIM] | Switch to alternate buffer |
| `Ctrl+6` | [NEOVIM] | Switch to alternate buffer |

### Window/Split Management

| Keys | Source | Action |
|------|--------|--------|
| `Ctrl+h` | [NVCHAD] | Move to left window |
| `Ctrl+j` | [NVCHAD] | Move to window below |
| `Ctrl+k` | [NVCHAD] | Move to window above |
| `Ctrl+l` | [NVCHAD] | Move to right window |
| `Ctrl+w v` | [NEOVIM] | Split vertical |
| `Ctrl+w s` | [NEOVIM] | Split horizontal |
| `Ctrl+w n` | [NEOVIM] | New window with empty buffer |
| `Ctrl+w q` | [NEOVIM] | Close current window |
| `Ctrl+w c` | [NEOVIM] | Close current window |
| `Ctrl+w o` | [NEOVIM] | Close all other windows |
| `Ctrl+w w` | [NEOVIM] | Cycle through windows |
| `Ctrl+w W` | [NEOVIM] | Cycle through windows (reverse) |
| `Ctrl+w p` | [NEOVIM] | Go to previous window |
| `Ctrl+w h/j/k/l` | [NEOVIM] | Go to window in direction |
| `Ctrl+w H/J/K/L` | [NEOVIM] | Move window to far direction |
| `Ctrl+w r` | [NEOVIM] | Rotate windows down/right |
| `Ctrl+w R` | [NEOVIM] | Rotate windows up/left |
| `Ctrl+w x` | [NEOVIM] | Exchange with next window |
| `Ctrl+w T` | [NEOVIM] | Move window to new tab |
| `Ctrl+w =` | [NEOVIM] | Make all windows equal size |
| `Ctrl+w _` | [NEOVIM] | Maximize height |
| `Ctrl+w \|` | [NEOVIM] | Maximize width |
| `Ctrl+w +` | [NEOVIM] | Increase height |
| `Ctrl+w -` | [NEOVIM] | Decrease height |
| `Ctrl+w >` | [NEOVIM] | Increase width |
| `Ctrl+w <` | [NEOVIM] | Decrease width |
| `:vsp <file>` | [NEOVIM] | Vertical split with file |
| `:sp <file>` | [NEOVIM] | Horizontal split with file |
| `:new` | [NEOVIM] | New horizontal split |
| `:vnew` | [NEOVIM] | New vertical split |
| `:only` | [NEOVIM] | Close all other windows |

### Tab Management

| Keys | Source | Action |
|------|--------|--------|
| `:tabnew` | [NEOVIM] | New tab |
| `:tabnew <file>` | [NEOVIM] | New tab with file |
| `:tabc` | [NEOVIM] | Close current tab |
| `:tabo` | [NEOVIM] | Close all other tabs |
| `gt` | [NEOVIM] | Next tab |
| `gT` | [NEOVIM] | Previous tab |
| `{n}gt` | [NEOVIM] | Go to tab n |
| `:tabm <n>` | [NEOVIM] | Move tab to position n |
| `:tabs` | [NEOVIM] | List all tabs |
| `:tabdo <cmd>` | [NEOVIM] | Execute command in all tabs |

---

## STAGE 5: Telescope — Complete

### File Finding

| Keys | Source | Action |
|------|--------|--------|
| `Leader f f` | [NVCHAD] | Find files |
| `Leader f a` | [NVCHAD] | Find all files (including hidden) |
| `Leader f o` | [NVCHAD] | Find recently opened (oldfiles) |
| `Leader f b` | [NVCHAD] | Find open buffers |
| `Leader f z` | [NVCHAD] | Fuzzy search current buffer |

### Text Search

| Keys | Source | Action |
|------|--------|--------|
| `Leader f w` | [NVCHAD] | Live grep (search text) |
| `Leader f W` | [NVCHAD] | Grep word under cursor |

### Git Integration

| Keys | Source | Action |
|------|--------|--------|
| `Leader g t` | [NVCHAD] | Git status |
| `Leader g c` | [NVCHAD] | Git commits |
| `Leader c m` | [NVCHAD] | Git commits (duplicate) |

### Help & Misc

| Keys | Source | Action |
|------|--------|--------|
| `Leader f h` | [NVCHAD] | Help tags |
| `Leader m a` | [NVCHAD] | Find marks |
| `Leader p t` | [NVCHAD] | Pick hidden terminal |
| `Leader t h` | [NVCHAD] | Theme picker |

### WhichKey Integration

| Keys | Source | Action |
|------|--------|--------|
| `Leader w K` | [NVCHAD] | Display all keymaps |
| `Leader w k` | [NVCHAD] | Query specific keymap |

### Inside Telescope Picker — Insert Mode

| Keys | Source | Action |
|------|--------|--------|
| `Ctrl+n` | [TELESCOPE] | Move to next item |
| `Ctrl+p` | [TELESCOPE] | Move to previous item |
| `Ctrl+j` | [TELESCOPE] | Move to next item |
| `Ctrl+k` | [TELESCOPE] | Move to previous item |
| `Down` | [TELESCOPE] | Move to next item |
| `Up` | [TELESCOPE] | Move to previous item |
| `Enter` | [TELESCOPE] | Select/confirm |
| `Ctrl+x` | [TELESCOPE] | Open in horizontal split |
| `Ctrl+v` | [TELESCOPE] | Open in vertical split |
| `Ctrl+t` | [TELESCOPE] | Open in new tab |
| `Ctrl+u` | [TELESCOPE] | Scroll preview up |
| `Ctrl+d` | [TELESCOPE] | Scroll preview down |
| `Ctrl+f` | [TELESCOPE] | Scroll preview down (page) |
| `Ctrl+b` | [TELESCOPE] | Scroll preview up (page) |
| `Ctrl+/` | [TELESCOPE] | Show mappings help |
| `Tab` | [TELESCOPE] | Toggle selection + move next |
| `Shift+Tab` | [TELESCOPE] | Toggle selection + move prev |
| `Ctrl+q` | [TELESCOPE] | Send to quickfix list |
| `Ctrl+l` | [TELESCOPE] | Complete tag |
| `Ctrl+c` | [TELESCOPE] | Close picker |
| `Esc` | [TELESCOPE] | Close picker |

### Inside Telescope Picker — Normal Mode

| Keys | Source | Action |
|------|--------|--------|
| `j` | [TELESCOPE] | Move to next item |
| `k` | [TELESCOPE] | Move to previous item |
| `gg` | [TELESCOPE] | Go to first item |
| `G` | [TELESCOPE] | Go to last item |
| `Enter` | [TELESCOPE] | Select/confirm |
| `q` | [TELESCOPE] | Close picker |
| `Esc` | [TELESCOPE] | Close picker |
| `?` | [TELESCOPE] | Show mappings help |
| `H` | [TELESCOPE] | Go to top of list |
| `M` | [TELESCOPE] | Go to middle of list |
| `L` | [TELESCOPE] | Go to bottom of list |

---

## STAGE 6: NvChad Mappings — Complete

### General

| Keys | Mode | Source | Action |
|------|------|--------|--------|
| `Esc` | n | [NVCHAD] | Clear search highlights |
| `Ctrl+s` | n | [NVCHAD] | Save file |
| `Ctrl+c` | n | [NVCHAD] | Copy entire file to clipboard |
| `Leader n` | n | [NVCHAD] | Toggle line numbers |
| `Leader r n` | n | [NVCHAD] | Toggle relative numbers |
| `Leader c h` | n | [NVCHAD] | Open NvCheatsheet |

### Insert Mode Navigation

| Keys | Mode | Source | Action |
|------|------|--------|--------|
| `Ctrl+b` | i | [NVCHAD] | Move to beginning of line |
| `Ctrl+e` | i | [NVCHAD] | Move to end of line |
| `Ctrl+h` | i | [NVCHAD] | Move left |
| `Ctrl+l` | i | [NVCHAD] | Move right |
| `Ctrl+j` | i | [NVCHAD] | Move down |
| `Ctrl+k` | i | [NVCHAD] | Move up |

### Comments

| Keys | Mode | Source | Action |
|------|------|--------|--------|
| `Leader /` | n | [NVCHAD] | Toggle line comment |
| `Leader /` | v | [NVCHAD] | Toggle block comment |
| `gcc` | n | [NVCHAD] | Toggle line comment |
| `gc` | v | [NVCHAD] | Toggle selection comment |

### Formatting

| Keys | Mode | Source | Action |
|------|------|--------|--------|
| `Leader f m` | n,v | [NVCHAD] | Format file/selection |

### LSP & Diagnostics

| Keys | Mode | Source | Action |
|------|------|--------|--------|
| `Leader d s` | n | [NVCHAD] | Diagnostic loclist |

### Terminal

| Keys | Mode | Source | Action |
|------|------|--------|--------|
| `Ctrl+x` | t | [NVCHAD] | Exit terminal mode |
| `Leader h` | n | [NVCHAD] | Horizontal terminal |
| `Leader v` | n | [NVCHAD] | Vertical terminal |
| `Alt+v` | n,t | [NVCHAD] | Toggle vertical terminal |
| `Alt+h` | n,t | [NVCHAD] | Toggle horizontal terminal |
| `Alt+i` | n,t | [NVCHAD] | Toggle floating terminal |

---

## STAGE 7: LSP (Language Server) — Complete

These activate when an LSP server attaches to a buffer.

### Navigation

| Keys | Source | Action |
|------|--------|--------|
| `gd` | [mappings.lua] | Go to definition (with navigation support) |
| `]d` | [mappings.lua] | Next definition/reference/implementation |
| `[d` | [mappings.lua] | Previous definition/reference/implementation |
| `gD` | [NVCHAD lspconfig] | Go to declaration |
| `gi` | [mappings.lua] | Go to implementation (with navigation support) |
| `gr` | [mappings.lua] | Go to references (with navigation support) |
| `K` | [ufo.lua] | Hover documentation (or peek fold) |
| `]w` | [mappings.lua] | Next word with definition |
| `[w` | [mappings.lua] | Previous word with definition |
| `Ctrl+k` | [NVCHAD lspconfig] | Signature help (insert mode) |
| `gO` | [NEOVIM] | Document symbols |

### Code Actions

| Keys | Source | Action |
|------|--------|--------|
| `Leader c a` | [NVCHAD] | Code actions |
| `Leader r a` | [NVCHAD] | Rename symbol |
| `Leader f m` | [NVCHAD] | Format |

### Diagnostics

| Keys | Source | Action |
|------|--------|--------|
| `[d` | [NVCHAD lspconfig] | Previous diagnostic |
| `]d` | [NVCHAD lspconfig] | Next diagnostic |
| `Leader d` | [NVCHAD] | Floating diagnostic |
| `Leader q` | [NVCHAD] | Diagnostic loclist |
| `Leader d s` | [NVCHAD] | Diagnostic loclist |

### LSP Commands

| Command | Source | Action |
|---------|--------|--------|
| `:LspInfo` | [NEOVIM] | Show attached LSP servers |
| `:LspStart` | [NEOVIM] | Start LSP server |
| `:LspStop` | [NEOVIM] | Stop LSP server |
| `:LspRestart` | [NEOVIM] | Restart LSP server |

---

## STAGE 8: Modal Editing — Exhaustive

### Your Custom Mappings (mappings.lua)

| Keys | Source | Action |
|------|--------|--------|
| `jk` | [mappings.lua] | Exit insert mode (alternative to Esc) |
| `Ctrl+h/j/k/l` | [mappings.lua] | Navigate windows (vim-tmux-navigator) |
| `Leader ac` | [mappings.lua] | Launch Claude in terminal buffer |
| `Leader gw` | [mappings.lua] | Open workflow guide (absolute path) |
| `:Claude` | [mappings.lua] | User command to launch Claude |

**Note:** `;` retains its default Neovim behavior (repeat f/t forward). Use `,` for repeating backward.

### Mode Switching

| Keys | Source | Action |
|------|--------|--------|
| `i` | [NEOVIM] | Insert before cursor |
| `a` | [NEOVIM] | Insert after cursor |
| `I` | [NEOVIM] | Insert at line start |
| `A` | [NEOVIM] | Insert at line end |
| `o` | [NEOVIM] | Insert on new line below |
| `O` | [NEOVIM] | Insert on new line above |
| `gi` | [NEOVIM] | Insert at last insert position |
| `gI` | [NEOVIM] | Insert at column 1 |
| `v` | [NEOVIM] | Visual character mode |
| `V` | [NEOVIM] | Visual line mode |
| `Ctrl+v` | [NEOVIM] | Visual block mode |
| `gv` | [NEOVIM] | Reselect last visual selection |
| `R` | [NEOVIM] | Replace mode |
| `Esc` | [NEOVIM] | Return to normal mode |
| `jk` | [mappings.lua] | Return to normal mode |
| `Ctrl+[` | [NEOVIM] | Return to normal mode |
| `Ctrl+c` | [NEOVIM] | Return to normal mode (interrupts) |

### Left-Right Motion

| Keys | Source | Action |
|------|--------|--------|
| `h` | [NEOVIM] | Left |
| `l` | [NEOVIM] | Right |
| `0` | [NEOVIM] | Line start (column 0) |
| `^` | [NEOVIM] | First non-whitespace |
| `$` | [NEOVIM] | Line end |
| `g0` | [NEOVIM] | Screen line start |
| `g^` | [NEOVIM] | Screen line first non-whitespace |
| `g$` | [NEOVIM] | Screen line end |
| `gm` | [NEOVIM] | Middle of screen line |
| `gM` | [NEOVIM] | Middle of line |
| `\|` | [NEOVIM] | Go to column (with count) |
| `f{char}` | [NEOVIM] | Find char forward |
| `F{char}` | [NEOVIM] | Find char backward |
| `t{char}` | [NEOVIM] | Till char forward |
| `T{char}` | [NEOVIM] | Till char backward |
| `;` | [NEOVIM] | Repeat last f/F/t/T in same direction |
| `,` | [NEOVIM] | Repeat last f/F/t/T in opposite direction |

**Important:** `;` and `,` only work after `f`, `F`, `t`, or `T` commands. They do NOT repeat other commands like `%`, scrolling, or navigation. For repeating other commands, use `.` (dot).

### Up-Down Motion

| Keys | Source | Action |
|------|--------|--------|
| `j` | [NEOVIM] | Down |
| `k` | [NEOVIM] | Up |
| `gj` | [NEOVIM] | Down (display line) |
| `gk` | [NEOVIM] | Up (display line) |
| `-` | [NEOVIM] | Up, to first non-blank |
| `+` | [NEOVIM] | Down, to first non-blank |
| `_` | [NEOVIM] | Down n-1 lines, first non-blank |
| `G` | [NEOVIM] | Go to line (default: last) |
| `gg` | [NEOVIM] | Go to line (default: first) |
| `{n}G` | [NEOVIM] | Go to line n |
| `:{n}` | [NEOVIM] | Go to line n |
| `{n}%` | [NEOVIM] | Go to n% of file |

### Word Motion

| Keys | Source | Action |
|------|--------|--------|
| `w` | [NEOVIM] | Next word start |
| `W` | [NEOVIM] | Next WORD start |
| `e` | [NEOVIM] | Next word end |
| `E` | [NEOVIM] | Next WORD end |
| `b` | [NEOVIM] | Previous word start |
| `B` | [NEOVIM] | Previous WORD start |
| `ge` | [NEOVIM] | Previous word end |
| `gE` | [NEOVIM] | Previous WORD end |

### Text Object Motion

| Keys | Source | Action |
|------|--------|--------|
| `(` | [NEOVIM] | Previous sentence |
| `)` | [NEOVIM] | Next sentence |
| `{` | [NEOVIM] | Previous paragraph |
| `}` | [NEOVIM] | Next paragraph |
| `[[` | [NEOVIM] | Previous section start |
| `]]` | [NEOVIM] | Next section start |
| `[]` | [NEOVIM] | Previous section end |
| `][` | [NEOVIM] | Next section end |

### Pattern Search

| Keys | Source | Action |
|------|--------|--------|
| `/{pattern}` | [NEOVIM] | Search forward |
| `?{pattern}` | [NEOVIM] | Search backward |
| `n` | [NEOVIM] | Repeat search (same direction) |
| `N` | [NEOVIM] | Repeat search (opposite direction) |
| `*` | [NEOVIM] | Search word under cursor forward |
| `#` | [NEOVIM] | Search word under cursor backward |
| `g*` | [NEOVIM] | Like * but partial word match |
| `g#` | [NEOVIM] | Like # but partial word match |
| `gd` | [NEOVIM] | Go to local definition |
| `gD` | [NEOVIM] | Go to global definition |
| `Leader n` | [NVCHAD] | Clear search highlight |
| `:noh` | [NEOVIM] | Clear search highlight |

### Scrolling

| Keys | Source | Action |
|------|--------|--------|
| `Ctrl+e` | [NEOVIM] | Scroll down one line |
| `Ctrl+y` | [NEOVIM] | Scroll up one line |
| `Ctrl+d` | [NEOVIM] | Scroll down half page |
| `Ctrl+u` | [NEOVIM] | Scroll up half page |
| `Ctrl+f` | [NEOVIM] | Scroll down full page |
| `Ctrl+b` | [NEOVIM] | Scroll up full page |

### Percentage Jumps (Repeatable)

| Keys | Source | Action |
|------|--------|--------|
| `{n}%` | [mappings.lua] | Jump to n% of file (repeatable with `.`) |
| `%` | [mappings.lua] | Jump to matching bracket (default behavior) |

**Usage:** Type any number followed by `%` (e.g., `10%`, `25%`, `75%`) to jump to that percentage of the file. These jumps are now **repeatable with `.`** thanks to the repeatable command system.

**Examples:**
- `10%` → jumps to 10% of file, then `.` repeats it
- `50%` → jumps to 50% of file, then `.` repeats it
- `%` (no number) → jumps to matching bracket (unchanged behavior)

### Screen Position

| Keys | Source | Action |
|------|--------|--------|
| `zz` | [NEOVIM] | Center cursor line |
| `zt` | [NEOVIM] | Cursor line to top |
| `zb` | [NEOVIM] | Cursor line to bottom |
| `z<CR>` | [NEOVIM] | Cursor line to top, cursor to first non-blank |
| `z.` | [NEOVIM] | Center, cursor to first non-blank |
| `z-` | [NEOVIM] | Bottom, cursor to first non-blank |
| `zh` | [NEOVIM] | Scroll right (no wrap) |
| `zl` | [NEOVIM] | Scroll left (no wrap) |
| `zH` | [NEOVIM] | Scroll half screen right |
| `zL` | [NEOVIM] | Scroll half screen left |
| `zs` | [NEOVIM] | Scroll horizontally, cursor at start |
| `ze` | [NEOVIM] | Scroll horizontally, cursor at end |

### Percentage Jumps (Custom)

| Keys | Source | Action |
|------|--------|--------|
| `Leader j` | [mappings.lua] | Prompt for percentage (0-100) and jump |
| `Leader j1` | [mappings.lua] | Quick jump to 10% |
| `Leader j2` | [mappings.lua] | Quick jump to 20% |
| `Leader j3` | [mappings.lua] | Quick jump to 30% |
| `Leader j4` | [mappings.lua] | Quick jump to 40% |
| `Leader j5` | [mappings.lua] | Quick jump to 50% |
| `Leader j6` | [mappings.lua] | Quick jump to 60% |
| `Leader j7` | [mappings.lua] | Quick jump to 70% |
| `Leader j8` | [mappings.lua] | Quick jump to 80% |
| `Leader j9` | [mappings.lua] | Quick jump to 90% |
| `Leader j0` | [mappings.lua] | Quick jump to 100% |

**Usage:** Press `Leader j` (Space + j) and type any percentage (e.g., `25` for 25%, `75` for 75%). For quick access to common percentages, use `Leader j1` through `Leader j0`.

### Screen Position

| Keys | Source | Action |
|------|--------|--------|
| `H` | [NEOVIM] | Go to top of screen |
| `M` | [NEOVIM] | Go to middle of screen |
| `L` | [NEOVIM] | Go to bottom of screen |

### Marks

| Keys | Source | Action |
|------|--------|--------|
| `m{a-z}` | [NEOVIM] | Set local mark |
| `m{A-Z}` | [NEOVIM] | Set global mark (cross-file) |
| `'{a-z}` | [NEOVIM] | Jump to mark line |
| `` `{a-z} `` | [NEOVIM] | Jump to mark position |
| `''` | [NEOVIM] | Jump to position before last jump |
| ` `` ` | [NEOVIM] | Jump to exact position before last jump |
| `'.` | [NEOVIM] | Jump to line of last change |
| `` `. `` | [NEOVIM] | Jump to position of last change |
| `'^` | [NEOVIM] | Jump to line where insert stopped |
| `` `^ `` | [NEOVIM] | Jump to position where insert stopped |
| `'[` | [NEOVIM] | Start of last yanked/changed text |
| `']` | [NEOVIM] | End of last yanked/changed text |
| `'<` | [NEOVIM] | Start of last visual selection |
| `'>` | [NEOVIM] | End of last visual selection |
| `:marks` | [NEOVIM] | List all marks |
| `:delmarks {marks}` | [NEOVIM] | Delete specified marks |
| `:delmarks!` | [NEOVIM] | Delete all lowercase marks |

### Jumps

| Keys | Source | Action |
|------|--------|--------|
| `Ctrl+o` | [NEOVIM] | Go to older jump position |
| `Ctrl+i` | [NEOVIM] | Go to newer jump position |
| `:jumps` | [NEOVIM] | List jump locations |
| `:clearjumps` | [NEOVIM] | Clear jump list |

**Note:** Navigation commands like `%`, `gg`, `G`, `10%` are added to the jumplist. Use `Ctrl+o` to go back and `Ctrl+i` to go forward through your jump history. The `.` command does NOT repeat these - you must type the command again or use the jumplist.

### Changes

| Keys | Source | Action |
|------|--------|--------|
| `g;` | [NEOVIM] | Go to older change position |
| `g,` | [NEOVIM] | Go to newer change position |
| `:changes` | [NEOVIM] | List change positions |

### Operators

| Keys | Source | Action |
|------|--------|--------|
| `d{motion}` | [NEOVIM] | Delete |
| `c{motion}` | [NEOVIM] | Change (delete and insert) |
| `y{motion}` | [NEOVIM] | Yank (copy) |
| `>{motion}` | [NEOVIM] | Indent right |
| `<{motion}` | [NEOVIM] | Indent left |
| `={motion}` | [NEOVIM] | Auto-indent |
| `gU{motion}` | [NEOVIM] | Uppercase |
| `gu{motion}` | [NEOVIM] | Lowercase |
| `g~{motion}` | [NEOVIM] | Toggle case |
| `gq{motion}` | [NEOVIM] | Format text |
| `gw{motion}` | [NEOVIM] | Format text (cursor stays) |
| `!{motion}` | [NEOVIM] | Filter through shell command |

### Operator Shortcuts

| Keys | Source | Action |
|------|--------|--------|
| `dd` | [NEOVIM] | Delete line |
| `cc` | [NEOVIM] | Change line |
| `yy` | [NEOVIM] | Yank line |
| `>>` | [NEOVIM] | Indent line |
| `<<` | [NEOVIM] | Unindent line |
| `==` | [NEOVIM] | Auto-indent line |
| `gUU` | [NEOVIM] | Uppercase line |
| `guu` | [NEOVIM] | Lowercase line |
| `g~~` | [NEOVIM] | Toggle case line |
| `D` | [NEOVIM] | Delete to end of line (d$) |
| `C` | [NEOVIM] | Change to end of line (c$) |
| `Y` | [NEOVIM] | Yank to end of line (y$) |
| `x` | [NEOVIM] | Delete character (dl) |
| `X` | [NEOVIM] | Delete character before (dh) |
| `s` | [NEOVIM] | Substitute character (cl) |
| `S` | [NEOVIM] | Substitute line (cc) |
| `r{char}` | [NEOVIM] | Replace character |
| `R` | [NEOVIM] | Replace mode |
| `J` | [NEOVIM] | Join lines (with space) |
| `gJ` | [NEOVIM] | Join lines (no space) |

### Text Objects

Use with operators: `d`, `c`, `y`, `v`, etc.

| Keys | Source | Action |
|------|--------|--------|
| `iw` | [NEOVIM] | Inner word |
| `aw` | [NEOVIM] | A word (includes surrounding space) |
| `iW` | [NEOVIM] | Inner WORD |
| `aW` | [NEOVIM] | A WORD |
| `is` | [NEOVIM] | Inner sentence |
| `as` | [NEOVIM] | A sentence |
| `ip` | [NEOVIM] | Inner paragraph |
| `ap` | [NEOVIM] | A paragraph |
| `i"` | [NEOVIM] | Inside double quotes |
| `a"` | [NEOVIM] | Around double quotes |
| `i'` | [NEOVIM] | Inside single quotes |
| `a'` | [NEOVIM] | Around single quotes |
| `` i` `` | [NEOVIM] | Inside backticks |
| `` a` `` | [NEOVIM] | Around backticks |
| `i(` or `ib` | [NEOVIM] | Inside parentheses |
| `a(` or `ab` | [NEOVIM] | Around parentheses |
| `i[` | [NEOVIM] | Inside brackets |
| `a[` | [NEOVIM] | Around brackets |
| `i{` or `iB` | [NEOVIM] | Inside braces |
| `a{` or `aB` | [NEOVIM] | Around braces |
| `i<` | [NEOVIM] | Inside angle brackets |
| `a<` | [NEOVIM] | Around angle brackets |
| `it` | [NEOVIM] | Inside tag |
| `at` | [NEOVIM] | Around tag |

**Examples:**
- `ciw` = change inner word
- `da"` = delete around quotes
- `yip` = yank inner paragraph
- `vi(` = visual select inside parens
- `ct)` = change to closing paren

### Treesitter Text Objects (AST-Aware)

Smart text objects based on code structure (requires treesitter-textobjects plugin).

| Keys | Source | Action |
|------|--------|--------|
| `af` | [treesitter-textobjects] | Around function (outer) |
| `if` | [treesitter-textobjects] | Inner function |
| `ac` | [treesitter-textobjects] | Around class (outer) |
| `ic` | [treesitter-textobjects] | Inner class |
| `aa` | [treesitter-textobjects] | Around parameter |
| `ia` | [treesitter-textobjects] | Inner parameter |
| `ab` | [treesitter-textobjects] | Around block |
| `ib` | [treesitter-textobjects] | Inner block |
| `aC` | [treesitter-textobjects] | Around call |
| `iC` | [treesitter-textobjects] | Inner call |
| `as` | [treesitter-textobjects] | Around statement |
| `a/` | [treesitter-textobjects] | Around comment |

**Navigation with Treesitter:**
| Keys | Source | Action |
|------|--------|--------|
| `]f` | [treesitter-textobjects] | Next function start |
| `[f` | [treesitter-textobjects] | Previous function start |
| `]F` | [treesitter-textobjects] | Next function end |
| `[F` | [treesitter-textobjects] | Previous function end |
| `]c` | [treesitter-textobjects] | Next class start |
| `[c` | [treesitter-textobjects] | Previous class start |
| `]a` | [treesitter-textobjects] | Next parameter start |
| `[a` | [treesitter-textobjects] | Previous parameter start |
| `]b` | [treesitter-textobjects] | Next block start |
| `[b` | [treesitter-textobjects] | Previous block start |

**Parameter Swapping:**
| Keys | Source | Action |
|------|--------|--------|
| `<leader>a` | [treesitter-textobjects] | Swap with next parameter |
| `<leader>A` | [treesitter-textobjects] | Swap with previous parameter |

**Examples:**
- `daf` = delete entire function
- `cif` = change function body
- `yac` = yank entire class
- `]f` = jump to next function
- `vif` = visually select function body

**Note:** These work based on the actual code structure (AST), not just syntax. More accurate than regex-based text objects.

### Registers

| Register | Source | Description |
|----------|--------|-------------|
| `""` | [NEOVIM] | Unnamed (default yank/delete) |
| `"0` | [NEOVIM] | Last yank |
| `"1-"9` | [NEOVIM] | Last 9 deletes |
| `"a-"z` | [NEOVIM] | Named registers |
| `"A-"Z` | [NEOVIM] | Append to named register |
| `"-` | [NEOVIM] | Small delete (less than one line) |
| `"+` | [NEOVIM] | System clipboard |
| `"*` | [NEOVIM] | Selection clipboard (X11 primary) |
| `".` | [NEOVIM] | Last inserted text (read-only) |
| `"%` | [NEOVIM] | Current filename (read-only) |
| `"#` | [NEOVIM] | Alternate filename (read-only) |
| `":` | [NEOVIM] | Last command (read-only) |
| `"/` | [NEOVIM] | Last search pattern |
| `"=` | [NEOVIM] | Expression register |
| `"_` | [NEOVIM] | Black hole (discard) |

**Usage:**
- `"ayy` = yank line to register a
- `"ap` = paste from register a
- `"+y` = yank to system clipboard
- `"+p` = paste from system clipboard
- `"_dd` = delete line without saving to register
- `:reg` = list all registers
- `Ctrl+r {reg}` = insert register (insert mode)

### Macros

| Keys | Source | Action |
|------|--------|--------|
| `q{a-z}` | [NEOVIM] | Start recording macro to register |
| `q` | [NEOVIM] | Stop recording |
| `@{a-z}` | [NEOVIM] | Execute macro |
| `@@` | [NEOVIM] | Repeat last macro |
| `{n}@{a-z}` | [NEOVIM] | Execute macro n times |
| `:@{a-z}` | [NEOVIM] | Execute register as Ex commands |

### Undo/Redo

| Keys | Source | Action |
|------|--------|--------|
| `u` | [NEOVIM] | Undo |
| `U` | [NEOVIM] | Undo all changes on line |
| `Ctrl+r` | [NEOVIM] | Redo |
| `.` | [NEOVIM] + [vim-repeat] | Repeat last change or repeatable command |

**Important Notes:**
- `.` repeats the last **change** (like `dd`, `ciw`, `r`, `x`, etc.)
- `.` also repeats **repeatable navigation commands** (like `10%`, `gg`, `G`) thanks to `vim-repeat` plugin
- For repeating character searches (`f`/`F`/`t`/`T`), use `;` (same direction) or `,` (opposite direction)
- Custom repeatable commands are configured in `utils/repeatable.lua` for consistency
| `g-` | [NEOVIM] | Go to older text state |
| `g+` | [NEOVIM] | Go to newer text state |
| `:earlier {time}` | [NEOVIM] | Go to earlier state (e.g., 10m) |
| `:later {time}` | [NEOVIM] | Go to later state |
| `:undolist` | [NEOVIM] | List undo history |

### Folding

| Keys | Source | Action |
|------|--------|--------|
| `zo` | [NEOVIM] | Open fold under cursor |
| `zO` | [NEOVIM] | Open all folds under cursor |
| `zc` | [NEOVIM] | Close fold under cursor |
| `zC` | [NEOVIM] | Close all folds under cursor |
| `za` | [NEOVIM] | Toggle fold |
| `zA` | [NEOVIM] | Toggle all folds under cursor |
| `zv` | [NEOVIM] | Open folds to reveal cursor |
| `zx` | [NEOVIM] | Update folds |
| `zX` | [NEOVIM] | Undo manually opened/closed folds |
| `zm` | [NEOVIM] | Fold more (increase foldlevel) |
| `zM` | [NEOVIM] | Close all folds |
| `zr` | [NEOVIM] | Fold less (decrease foldlevel) |
| `zR` | [NEOVIM] | Open all folds |
| `zn` | [NEOVIM] | Disable folding |
| `zN` | [NEOVIM] | Enable folding |
| `zi` | [NEOVIM] | Toggle foldenable |
| `zj` | [NEOVIM] | Move to next fold |
| `zk` | [NEOVIM] | Move to previous fold |
| `[z` | [NEOVIM] | Move to start of open fold |
| `]z` | [NEOVIM] | Move to end of open fold |
| `zf{motion}` | [NEOVIM] | Create fold |
| `zd` | [NEOVIM] | Delete fold under cursor |
| `zD` | [NEOVIM] | Delete all folds under cursor |
| `zE` | [NEOVIM] | Eliminate all folds |

### g Commands (Miscellaneous)

| Keys | Source | Action |
|------|--------|--------|
| `ga` | [NEOVIM] | Show ASCII value of character |
| `g8` | [NEOVIM] | Show UTF-8 bytes of character |
| `gf` | [NEOVIM] | Edit file under cursor |
| `gF` | [NEOVIM] | Edit file under cursor, go to line |
| `gx` | [NEOVIM] | Open URL/file under cursor |
| `gv` | [NEOVIM] | Reselect last visual area |
| `gi` | [NEOVIM] | Go to last insert position and insert |
| `gI` | [NEOVIM] | Insert at column 1 |
| `gn` | [NEOVIM] | Search forward and select match |
| `gN` | [NEOVIM] | Search backward and select match |
| `gp` | [NEOVIM] | Paste and move cursor after |
| `gP` | [NEOVIM] | Paste before and move cursor after |
| `g;` | [NEOVIM] | Go to older change |
| `g,` | [NEOVIM] | Go to newer change |
| `g&` | [NEOVIM] | Repeat last substitute on all lines |
| `gq{motion}` | [NEOVIM] | Format lines |
| `gw{motion}` | [NEOVIM] | Format lines (cursor stays) |
| `guu` | [NEOVIM] | Lowercase line |
| `gUU` | [NEOVIM] | Uppercase line |
| `g~~` | [NEOVIM] | Toggle case line |
| `gJ` | [NEOVIM] | Join without space |
| `g?{motion}` | [NEOVIM] | ROT13 encode |
| `g<` | [NEOVIM] | Show last command output |
| `gQ` | [NEOVIM] | Enter Ex mode |
| `g Ctrl+g` | [NEOVIM] | Show cursor position info |
| `gm` | [NEOVIM] | Go to middle of screen line |
| `go` | [NEOVIM] | Go to byte in buffer |

### Visual Mode

| Keys | Source | Action |
|------|--------|--------|
| `v` | [NEOVIM] | Start visual character mode |
| `V` | [NEOVIM] | Start visual line mode |
| `Ctrl+v` | [NEOVIM] | Start visual block mode |
| `gv` | [NEOVIM] | Reselect last selection |
| `o` | [NEOVIM] | Move to other end of selection |
| `O` | [NEOVIM] | Move to other corner (block mode) |
| `>` | [NEOVIM] | Indent selection |
| `<` | [NEOVIM] | Unindent selection |
| `=` | [NEOVIM] | Auto-indent selection |
| `~` | [NEOVIM] | Toggle case |
| `u` | [NEOVIM] | Lowercase |
| `U` | [NEOVIM] | Uppercase |
| `I` | [NEOVIM] | Insert at start (block mode) |
| `A` | [NEOVIM] | Append at end (block mode) |
| `c` | [NEOVIM] | Change selection |
| `C` | [NEOVIM] | Change to end of line |
| `r{char}` | [NEOVIM] | Replace all with char |
| `J` | [NEOVIM] | Join lines |
| `gJ` | [NEOVIM] | Join lines without space |
| `:` | [NEOVIM] | Enter command for selection |

### Insert Mode

| Keys | Source | Action |
|------|--------|--------|
| `Ctrl+h` | [NEOVIM] | Delete character before cursor |
| `Ctrl+w` | [NEOVIM] | Delete word before cursor |
| `Ctrl+u` | [NEOVIM] | Delete to start of line |
| `Ctrl+t` | [NEOVIM] | Indent current line |
| `Ctrl+d` | [NEOVIM] | Unindent current line |
| `Ctrl+j` | [NEOVIM] | Begin new line |
| `Ctrl+m` | [NEOVIM] | Begin new line |
| `Ctrl+r {reg}` | [NEOVIM] | Insert register contents |
| `Ctrl+r =` | [NEOVIM] | Insert expression result |
| `Ctrl+a` | [NEOVIM] | Insert previously inserted text |
| `Ctrl+@` | [NEOVIM] | Insert previously inserted text and exit |
| `Ctrl+o` | [NEOVIM] | Execute one normal mode command |
| `Ctrl+n` | [NEOVIM] | Next completion match |
| `Ctrl+p` | [NEOVIM] | Previous completion match |
| `Ctrl+x Ctrl+l` | [NEOVIM] | Line completion |
| `Ctrl+x Ctrl+n` | [NEOVIM] | Keyword completion (next) |
| `Ctrl+x Ctrl+p` | [NEOVIM] | Keyword completion (prev) |
| `Ctrl+x Ctrl+f` | [NEOVIM] | Filename completion |
| `Ctrl+x Ctrl+]` | [NEOVIM] | Tag completion |
| `Ctrl+x Ctrl+o` | [NEOVIM] | Omni completion |
| `Ctrl+v {char}` | [NEOVIM] | Insert character literally |
| `Ctrl+k {digraph}` | [NEOVIM] | Insert digraph |

### Command-Line Mode

| Keys | Source | Action |
|------|--------|--------|
| `:` | [NEOVIM] | Enter command-line mode |
| `/` | [NEOVIM] | Search forward |
| `?` | [NEOVIM] | Search backward |
| `Ctrl+r {reg}` | [NEOVIM] | Insert register |
| `Ctrl+r Ctrl+w` | [NEOVIM] | Insert word under cursor |
| `Ctrl+r Ctrl+a` | [NEOVIM] | Insert WORD under cursor |
| `Ctrl+r Ctrl+l` | [NEOVIM] | Insert line under cursor |
| `Ctrl+f` | [NEOVIM] | Open command-line window |
| `Up` / `Ctrl+p` | [NEOVIM] | Previous command |
| `Down` / `Ctrl+n` | [NEOVIM] | Next command |
| `Tab` | [NEOVIM] | Complete |
| `Ctrl+d` | [NEOVIM] | List completions |
| `Ctrl+l` | [NEOVIM] | Complete and show all matches |

### Ex Commands (Common)

| Command | Source | Action |
|---------|--------|--------|
| `:w` | [NEOVIM] | Write (save) |
| `:w {file}` | [NEOVIM] | Write to file |
| `:wa` | [NEOVIM] | Write all buffers |
| `:q` | [NEOVIM] | Quit |
| `:q!` | [NEOVIM] | Quit without saving |
| `:wq` | [NEOVIM] | Write and quit |
| `:x` | [NEOVIM] | Write if changed and quit |
| `ZZ` | [NEOVIM] | Write if changed and quit |
| `ZQ` | [NEOVIM] | Quit without saving |
| `:e {file}` | [NEOVIM] | Edit file |
| `:e!` | [NEOVIM] | Reload file |
| `:r {file}` | [NEOVIM] | Read file contents below cursor |
| `:r !{cmd}` | [NEOVIM] | Read command output |
| `:{range}d` | [NEOVIM] | Delete lines |
| `:{range}y` | [NEOVIM] | Yank lines |
| `:{range}m {line}` | [NEOVIM] | Move lines |
| `:{range}co {line}` | [NEOVIM] | Copy lines |
| `:{range}s/pat/rep/` | [NEOVIM] | Substitute |
| `:{range}s/pat/rep/g` | [NEOVIM] | Substitute all on line |
| `:{range}s/pat/rep/gc` | [NEOVIM] | Substitute with confirm |
| `:%s/pat/rep/g` | [NEOVIM] | Substitute in whole file |
| `:g/pat/cmd` | [NEOVIM] | Execute cmd on matching lines |
| `:v/pat/cmd` | [NEOVIM] | Execute cmd on non-matching lines |
| `:sort` | [NEOVIM] | Sort lines |
| `:sort!` | [NEOVIM] | Sort reverse |
| `:sort u` | [NEOVIM] | Sort and remove duplicates |
| `:!{cmd}` | [NEOVIM] | Execute shell command |
| `:{range}!{cmd}` | [NEOVIM] | Filter lines through command |
| `:h {topic}` | [NEOVIM] | Help |
| `:set {option}` | [NEOVIM] | Set option |
| `:set {option}?` | [NEOVIM] | Show option value |
| `:set {option}&` | [NEOVIM] | Reset option to default |

---

## STAGE 9: Utilities & Terminal — Complete

### Built-in Terminal

| Keys | Source | Action |
|------|--------|--------|
| `Leader h` | [NVCHAD] | Horizontal terminal |
| `Leader v` | [NVCHAD] | Vertical terminal |
| `Leader ac` | [mappings.lua] | Launch Claude in terminal buffer |
| `Alt+h` | [NVCHAD] | Toggle horizontal terminal |
| `Alt+v` | [NVCHAD] | Toggle vertical terminal |
| `Alt+i` | [NVCHAD] | Toggle floating terminal |
| `Ctrl+x` | [NVCHAD] | Exit terminal mode |
| `:terminal` | [NEOVIM] | Open terminal in current window |
| `:term {cmd}` | [NEOVIM] | Open terminal with command |
| `:Claude` | [mappings.lua] | Launch Claude CLI in terminal |

**Shell Alias:** `cld` — Claude CLI with required flags (configured in .bashrc)

### Cheatsheet & Help

| Keys | Source | Action |
|------|--------|--------|
| `Leader c h` | [NVCHAD] | Open NvChad cheatsheet |
| `Leader gw` | [mappings.lua] | Open workflow guide (this file) |
| `Leader` (wait) | [which-key.lua] | Show all keybindings in large panel |
| `:Telescope keymaps` | [TELESCOPE] | Searchable keybindings |
| `:h {topic}` | [NEOVIM] | Neovim help |
| `:h index` | [NEOVIM] | Index of all commands |
| `:h quickref` | [NEOVIM] | Quick reference |
| `K` | [NEOVIM] | Help for word under cursor (in help files) |

### Line Numbers

| Keys | Source | Action |
|------|--------|--------|
| `Leader n` | [NVCHAD] | Toggle line numbers |
| `Leader r n` | [NVCHAD] | Toggle relative numbers |
| `:set nu` | [NEOVIM] | Enable line numbers |
| `:set nonu` | [NEOVIM] | Disable line numbers |
| `:set rnu` | [NEOVIM] | Enable relative numbers |
| `:set nornu` | [NEOVIM] | Disable relative numbers |

### WhichKey

Press `Leader` (Space) and wait 300ms — large panel shows all available keybindings at once [which-key.lua].

**Features:**
- Large panel mode (50x25) — see all mappings without scrolling
- Full visibility — no paging or collapsing
- Rehearsal mode — perfect for learning keybindings
- Shows all leader key combinations

**Custom Mappings:**
- `Leader ac` — Launch Claude in terminal buffer
- `Leader gw` — Open workflow guide (absolute path)

---

## STAGE 10: Additional Plugins — Complete

### Flash.nvim — Enhanced Navigation

Better than hop/easymotion for jumping to any visible location.

| Keys | Mode | Source | Action |
|------|------|--------|--------|
| `s` | n,x,o | [flash.lua] | Flash jump (type chars, jump to label) |
| `S` | n,x,o | [flash.lua] | Flash Treesitter (select syntax nodes) |
| `r` | o | [flash.lua] | Remote Flash (operator pending) |
| `R` | o,x | [flash.lua] | Treesitter Search |
| `Ctrl+s` | c | [flash.lua] | Toggle Flash Search (in command-line) |

**Usage:** Press `s`, type a few chars, then press the label to jump. `S` lets you select treesitter nodes visually.

### Harpoon — Fast File Navigation (ThePrimeagen)

Mark frequently used files and jump to them instantly with numbered shortcuts.

| Keys | Source | Action |
|------|--------|--------|
| `Leader hm` | [harpoon.lua] | Mark current file (add to harpoon list) |
| `Leader hh` | [harpoon.lua] | Toggle harpoon quick menu |
| `Leader h1` | [harpoon.lua] | Jump to harpoon file 1 |
| `Leader h2` | [harpoon.lua] | Jump to harpoon file 2 |
| `Leader h3` | [harpoon.lua] | Jump to harpoon file 3 |
| `Leader h4` | [harpoon.lua] | Jump to harpoon file 4 |
| `Leader h5` | [harpoon.lua] | Jump to harpoon file 5 |
| `Leader h6-h9` | [harpoon.lua] | Jump to harpoon files 6-9 |
| `Leader hn` | [harpoon.lua] | Navigate to next harpoon file |
| `Leader hp` | [harpoon.lua] | Navigate to previous harpoon file |

**Usage:** Mark your most frequently accessed files with `Leader hm`, then use `Leader h1-h9` for instant access.

### LazyGit — Full Git UI

| Keys | Source | Action |
|------|--------|--------|
| `Leader gg` | [lazygit.lua] | Open LazyGit (full git workflow UI) |

**Commands:** `:LazyGit`, `:LazyGitConfig`, `:LazyGitCurrentFile`, `:LazyGitFilter`

### Trouble.nvim — Better Diagnostics/Quickfix

| Keys | Source | Action |
|------|--------|--------|
| `Leader xx` | [trouble.lua] | Toggle diagnostics (all files) |
| `Leader xX` | [trouble.lua] | Toggle diagnostics (current buffer only) |
| `Leader cs` | [trouble.lua] | Toggle symbols outline |
| `Leader cl` | [trouble.lua] | Toggle LSP definitions/references/etc |
| `Leader xL` | [trouble.lua] | Toggle location list |
| `Leader xQ` | [trouble.lua] | Toggle quickfix list |

### TODO Comments — Navigate/Search TODOs

| Keys | Source | Action |
|------|--------|--------|
| `]t` | [todo-comments.lua] | Jump to next TODO comment |
| `[t` | [todo-comments.lua] | Jump to previous TODO comment |
| `Leader TA` | [todo-comments.lua] | Search all TODOs (Telescope) |
| `Leader TT` | [todo-comments.lua] | Search TODO/FIX only (Telescope) |

**Recognized keywords:** TODO, FIX, FIXME, HACK, WARN, PERF, NOTE, TEST

### Refactoring.nvim — Code Refactoring (ThePrimeagen)

| Keys | Mode | Source | Action |
|------|------|--------|--------|
| `Leader re` | n,v | [refactoring.lua] | Open refactor menu |

**Available refactors:** Extract Function, Extract Variable, Inline Variable, Inline Function, Extract Block, and more.

### Oil.nvim — File Explorer (Buffer-Style)

| Keys | Source | Action |
|------|--------|--------|
| `-` | [oil.lua] | Open parent directory in oil |

**Note:** Oil opens directories as editable buffers. Use normal vim motions to navigate, rename by editing text, delete with `dd`, etc. Save with `:w` to apply changes.

### Grug-Far.nvim — Search and Replace with Preview

| Keys | Mode | Source | Action |
|------|------|--------|--------|
| `Leader sr` | n,v | [grug-far.lua] | Open search and replace UI |

**Features:** Live preview of replacements, regex support, file type filtering.

### Gitlinker.nvim — Generate Git Links

| Keys | Source | Action |
|------|--------|--------|
| `Leader gy` | [gitlinker.lua] | Copy GitHub/GitLab link to clipboard |

### File Path Operations

| Keys | Source | Action |
|------|--------|--------|
| `Leader yf` | [mappings.lua] | Yank file absolute path |
| `Leader yr` | [mappings.lua] | Yank file relative path |

**Usage:** Quickly copy file paths to clipboard for sharing, documentation, or scripts.

### UFO.nvim — Better Folding (Treesitter)

| Keys | Source | Action |
|------|--------|--------|
| `zR` | [ufo.lua] | Open all folds |
| `zM` | [ufo.lua] | Close all folds |
| `K` | [ufo.lua] | Peek fold preview OR LSP hover |

**Note:** `K` first tries to show folded content preview. If no fold under cursor, shows LSP hover docs instead.

### dial.nvim — Enhanced Increment/Decrement

| Keys | Source | Action |
|------|--------|--------|
| `Ctrl+a` | [init.lua] | Increment (numbers, dates, booleans, semver) |
| `Ctrl+x` | [init.lua] | Decrement (numbers, dates, booleans, semver) |

**Supports:** Integers, hex, dates (YYYY/MM/DD), booleans (true/false), semver (1.2.3), let/const cycling.

### git.nvim — Git Blame/Browse

| Keys | Source | Action |
|------|--------|--------|
| `Leader gb` | [init.lua] | Open git blame window |
| `Leader go` | [init.lua] | Open file/folder in git repository (browser) |

### close-buffers.nvim — Buffer Cleanup

| Keys | Source | Action |
|------|--------|--------|
| `Leader th` | [init.lua] | Close all hidden buffers |
| `Leader tu` | [init.lua] | Close all nameless buffers |

### mini.bracketed — Bracket Navigation

| Keys | Source | Action |
|------|--------|--------|
| `]n` / `[n` | [init.lua] | Next/previous treesitter node |

**Note:** Other bracket pairs (files, windows, quickfix, yank) are disabled in config.

### vim-repeat — Enhanced Repeat System

| Keys | Source | Action |
|------|--------|--------|
| `.` | [vim-repeat] | Repeat last change or repeatable command |

**What it does:** Extends Neovim's `.` command to repeat more types of commands, including custom repeatable navigation commands.

**Custom Repeatable Commands:**
- `{n}%` (percentage jumps) — now repeatable with `.`
- More commands can be made repeatable using `utils/repeatable.lua`

**Usage:** After typing `10%`, press `.` to repeat the jump. The repeatable system is extensible — new commands can be added to `utils/repeatable.lua` for consistency.

### NeoCodeium — AI Code Completion (Free)

Free AI-powered code completion powered by Windsurf (formerly Codeium). Provides intelligent suggestions as you type.

| Keys | Mode | Source | Action |
|------|------|--------|--------|
| `Alt+f` | i | [neocodeium.lua] | Accept full completion |
| `Alt+w` | i | [neocodeium.lua] | Accept word only |
| `Alt+a` | i | [neocodeium.lua] | Accept line only |
| `Alt+e` | i | [neocodeium.lua] | Cycle to next suggestion or trigger completion |
| `Alt+r` | i | [neocodeium.lua] | Cycle to previous suggestion |
| `Alt+c` | i | [neocodeium.lua] | Clear current completion |

**Commands:**
- `:NeoCodeium auth` — Authenticate and save API token (required on first use)
- `:NeoCodeium toggle` — Toggle completions on/off
- `:NeoCodeium chat` — Open Windsurf Chat in browser
- `:NeoCodeium restart` — Restart the server

**Setup:** After installing, run `:NeoCodeium auth` to authenticate with Windsurf. The plugin will automatically start providing completions.

**Note:** Uses Alt key bindings to avoid conflicts with Tab completion. Completions appear as virtual text.

---

## Comprehensive Navigation Workflow

### Daily Project Exploration Routine

**1. Initial Project Setup (First 5 minutes)**
```
1. <leader>ff          → Find file (Telescope)
2. Navigate to main entry point
3. <leader>hm          → Mark file (Harpoon #1)
4. <leader>ff          → Find related files
5. <leader>hm          → Mark 4 more files (Harpoon #2-5)
```

**2. Understanding Code Flow**
```
1. Place cursor on function/class name
2. gd                   → Go to definition
3. ]d                   → Next definition (if multiple) // not doing anything?
4. [d                   → Previous definition
5. gr                   → Find all references
6. ]d                   → Navigate through references
7. gi                   → Find implementations
8. ]d                   → Navigate implementations
9. <C-o>                → Jump back (vim jump list)
```

**3. Quick File Switching (Harpoon)**
```
<leader>h1  → Jump to file #1 // how to designate files to a number?
<leader>h2  → Jump to file #2
<leader>h3  → Jump to file #3
<leader>h4  → Jump to file #4
<leader>h5  → Jump to file #5
<leader>hh  → See all marked files
<leader>hn  → Next in harpoon list
<leader>hp  → Previous in harpoon list
```

**4. Learning Definitions (Hover & Navigate)**
```
1. K                    → Hover (show definition/tooltip)
2. ]w                   → Next word with definition
3. [w                   → Previous word with definition
4. gd                   → Jump to definition
5. <C-o>                → Jump back
```

**5. Reference Navigation**
```
1. gr                   → Find all references
2. ]d                   → Next reference
3. [d                   → Previous reference
4. <leader>yf           → Yank file path (if you want to note it)
```

**6. Implementation Navigation**
```
1. gi                   → Find implementations
2. ]d                   → Next implementation
3. [d                   → Previous implementation
```

**7. Smart Text Object Selection**
```
vaf          → Select entire function
vif          → Select function body
vac          → Select entire class
vic          → Select class body
daf          → Delete entire function
cif          → Change function body
]f           → Jump to next function
[f           → Jump to previous function
```

**8. File Operations**
```
-            → Open Oil (editable file tree)
<leader>yf   → Yank absolute file path
<leader>yr   → Yank relative file path
<leader>gy   → Yank GitHub link (gitlinker)
```

### Complete Navigation Key Reference

| Task | Keys | Source |
|------|------|--------|
| **File Navigation** | | |
| Find files | `<leader>ff` | [NVCHAD] |
| Live grep | `<leader>fw` | [NVCHAD] |
| Harpoon mark | `<leader>hm` | [harpoon.lua] |
| Harpoon jump | `<leader>h1-9` | [harpoon.lua] |
| Oil explorer | `-` | [oil.lua] |
| **LSP Navigation** | | |
| Go to definition | `gd` | [mappings.lua] |
| Next/prev result | `]d` / `[d` | [mappings.lua] |
| Find references | `gr` | [mappings.lua] |
| Find implementations | `gi` | [mappings.lua] |
| Hover documentation | `K` | [ufo.lua] |
| Next word with def | `]w` | [mappings.lua] |
| Prev word with def | `[w` | [mappings.lua] |
| **Text Objects** | | |
| Function (outer) | `af` / `if` | [treesitter-textobjects] |
| Class (outer) | `ac` / `ic` | [treesitter-textobjects] |
| Parameter | `aa` / `ia` | [treesitter-textobjects] |
| Block | `ab` / `ib` | [treesitter-textobjects] |
| Call | `aC` / `iC` | [treesitter-textobjects] |
| Next function | `]f` / `[f` | [treesitter-textobjects] |
| **File Operations** | | |
| Yank absolute path | `<leader>yf` | [mappings.lua] |
| Yank relative path | `<leader>yr` | [mappings.lua] |
| Yank git link | `<leader>gy` | [gitlinker.lua] |

---

## Quick Reference: Most Important Keys

### "I need to..."

| Task | Keys | Source |
|------|------|--------|
| Open file tree | `Ctrl+n` | [NVCHAD] |
| Find file by name | `Leader f f` | [NVCHAD] |
| Search text in project | `Leader f w` | [NVCHAD] |
| Go to definition | `gd` then `]d`/`[d` | [mappings.lua] |
| Navigate LSP results | `]d` / `[d` | [mappings.lua] |
| Next word with def | `]w` / `[w` | [mappings.lua] |
| Switch buffer | `Tab` / `Shift+Tab` | [NVCHAD] |
| Close buffer | `Leader x` | [NVCHAD] |
| Save file | `:w Enter` or `Ctrl+s` | [NEOVIM] / [NVCHAD] |
| Quit | `:q Enter` | [NEOVIM] |
| Split vertical | `Ctrl+w v` | [NEOVIM] |
| Move between windows | `Ctrl+h/j/k/l` | [mappings.lua] (vim-tmux-navigator) |
| Change theme | `Leader t h` | [NVCHAD] |
| Open cheatsheet | `Leader c h` | [NVCHAD] |
| Open workflow guide | `Leader gw` | [mappings.lua] |
| Show all keybindings | `Leader` (wait) | [which-key.lua] |
| Launch Claude | `Leader ac` | [mappings.lua] |
| Jump to percentage | `{n}%` then `.` | [mappings.lua] |
| Toggle comment | `Leader /` or `gcc` | [NVCHAD] |
| Accept AI completion | `Alt+f` | [neocodeium.lua] |
| Accept AI word | `Alt+w` | [neocodeium.lua] |
| Accept AI line | `Alt+a` | [neocodeium.lua] |
| Toggle AI completion | `:NeoCodeium toggle` | [neocodeium.lua] |
| System clipboard yank | `"+y` | [NEOVIM] |
| System clipboard paste | `"+p` | [NEOVIM] |

### "In tmux I need to..."

| Task | Keys | Source |
|------|------|--------|
| New pane right | `Prefix \|` | [tmux.conf] |
| New pane below | `Prefix -` | [tmux.conf] |
| Move between panes | `Ctrl+h/j/k/l` | [tmux.conf] (seamless vim/tmux) |
| Move between panes (backup) | `Prefix h/j/k/l` | [tmux.conf] |
| New window | `Prefix c` | [tmux.conf] |
| Switch window | `Prefix 1/2/3...` | [TMUX] |
| Close pane | `Prefix x` | [tmux.conf] |
| Zoom pane | `Prefix z` | [TMUX] |
| Detach session | `Prefix d` | [TMUX] |
| Reload config | `Prefix r` | [tmux.conf] |
| Open workflow guide | `Prefix w` | [tmux.conf] |
| List all bindings | `Prefix ?` | [TMUX] |
| Show keybindings menu | `Prefix ?` | [tmux-which-key] |
| Reattach session | `tmux attach -t main` | shell |

---

## Practice Drills

### Drill 1: Tmux Navigation
1. `Prefix |` [tmux.conf] — split right
2. `Prefix -` [tmux.conf] — split below
3. `Ctrl+h` [tmux.conf] — go left (seamless)
4. `Ctrl+l` [tmux.conf] — go right (seamless)
5. `Ctrl+j` [tmux.conf] — go down (seamless)
6. `Prefix z` [TMUX] — zoom pane
7. `Prefix z` [TMUX] — unzoom
8. `Prefix x` [tmux.conf] — kill pane

### Drill 2: File Operations
1. `Ctrl+n` [NVCHAD] — open tree
2. `j j j` [NVIMTREE] — move down
3. `a` [NVIMTREE] — create `test.txt`
4. `Enter` [NVIMTREE] — open file
5. `i` [NEOVIM] — insert mode
6. Type something
7. `jk` [mappings.lua] — normal mode
8. `Ctrl+s` [NVCHAD] — save
9. `Ctrl+n` [NVCHAD] — back to tree
10. `d` [NVIMTREE] — delete
11. `y` [NVIMTREE] — confirm

### Drill 3: Telescope Workflow
1. `Leader f f` [NVCHAD] — find files
2. Type partial filename
3. `Ctrl+n` / `Ctrl+p` [TELESCOPE] — navigate
4. `Enter` [TELESCOPE] — open
5. `Leader f w` [NVCHAD] — grep project
6. Type search term
7. `Enter` [TELESCOPE] — jump to match

### Drill 4: Buffer Workflow
1. `Leader f f` [NVCHAD] — open file 1
2. `Leader f f` [NVCHAD] — open file 2
3. `Leader f f` [NVCHAD] — open file 3
4. `Tab` [NVCHAD] — cycle forward
5. `Shift+Tab` [NVCHAD] — cycle back
6. `Leader x` [NVCHAD] — close current

### Drill 5: Modal Editing
1. `gg` [NEOVIM] — go to top
2. `G` [NEOVIM] — go to bottom
3. `Ctrl+u` [NEOVIM] — half page up
4. `/function` [NEOVIM] — search
5. `n` [NEOVIM] — next match
6. `ciw` [NEOVIM] — change word
7. Type new word
8. `jk` [mappings.lua] — normal mode
9. `.` [NEOVIM] — repeat change

### Drill 6: Registers & Clipboard
1. `"ayy` [NEOVIM] — yank line to register a
2. Move to another location
3. `"ap` [NEOVIM] — paste from register a
4. `"+yy` [NEOVIM] — yank to system clipboard
5. Open another app, paste to verify
6. Copy text from another app
7. `"+p` [NEOVIM] — paste from clipboard
8. `:reg` [NEOVIM] — view all registers

### Drill 7: Macros
1. `qa` [NEOVIM] — start recording to register a
2. Make some edits
3. `q` [NEOVIM] — stop recording
4. Move to another location
5. `@a` [NEOVIM] — execute macro
6. `@@` [NEOVIM] — repeat macro
7. `10@a` [NEOVIM] — execute macro 10 times

### Drill 8: Folding (UFO Enhanced)
1. Open a file with functions
2. `zM` [ufo.lua] — close all folds
3. `zo` [NEOVIM] — open fold under cursor
4. `zc` [NEOVIM] — close fold
5. `za` [NEOVIM] — toggle fold
6. `zR` [ufo.lua] — open all folds
7. `K` [ufo.lua] — peek fold content (or LSP hover)
8. `zj` [NEOVIM] — next fold
9. `zk` [NEOVIM] — previous fold

### Drill 9: Flash Navigation
1. `s` [flash.lua] — start flash
2. Type 2-3 chars of target
3. Press the label letter to jump
4. `S` [flash.lua] — flash treesitter (select nodes)
5. Use `o` to expand/contract selection
6. `y` [NEOVIM] — yank selection

### Drill 10: Harpoon Workflow
1. Open a file you use often
2. `Leader hm` [harpoon.lua] — mark it
3. Open another frequently used file
4. `Leader hm` [harpoon.lua] — mark it
5. `Leader h1` [harpoon.lua] — jump to first marked
6. `Leader h2` [harpoon.lua] — jump to second marked
7. `Leader hh` [harpoon.lua] — view/edit harpoon list
8. `Leader hn` / `Leader hp` — cycle through marked files

### Drill 11: Diagnostics with Trouble
1. Open a file with LSP errors
2. `Leader xx` [trouble.lua] — open diagnostics panel
3. `j/k` — navigate errors
4. `Enter` — jump to error
5. `Leader xX` [trouble.lua] — buffer diagnostics only
6. `Leader cs` [trouble.lua] — toggle symbols outline

### Drill 12: Search and Replace
1. `Leader sr` [grug-far.lua] — open search/replace
2. Type search pattern
3. Type replacement
4. Preview changes in real-time
5. Confirm to apply

---

*Exhaustive guide generated from your dotfiles. Built-in help: `Leader c h` [NVCHAD], `Prefix ?` [TMUX], `g?` [NVIMTREE], `:h` [NEOVIM]*
