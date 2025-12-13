# Cursor IDE + Neovim Workflow Guide — Complete Edition

A comprehensive guide to using Neovim within Cursor IDE, optimized for modern development workflows.

**Legend — Where each mapping comes from:**
- `[CURSOR]` = Cursor IDE native feature
- `[CURSOR-KB]` = Cursor keybindings.json configuration
- `[mappings.lua]` = Your Neovim mappings
- `[NVCHAD]` = NvChad default mapping
- `[NVIMTREE]` = NvimTree plugin default
- `[TELESCOPE]` = Telescope plugin default
- `[NEOVIM]` = Vanilla Neovim/Vim default
- `[vscode-neovim]` = vscode-neovim extension default
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
- `[vim-repeat]` = tpope/vim-repeat plugin
- `[utils/repeatable.lua]` = custom repeatable command utility
- `[init.lua]` = plugins/init.lua (inline plugin configs)

**Leader** = `Space` (NvChad default)
**Cursor Command Palette** = `Cmd+Shift+P` (Mac) / `Ctrl+Shift+P` (Windows/Linux)

---

## STAGE 1: Cursor IDE Fundamentals

Cursor is a full-featured IDE built on VS Code, with embedded Neovim via the vscode-neovim extension. Unlike the WezTerm + tmux workflow, Cursor provides integrated panels, terminals, and AI features.

### Cursor Window Management

| Keys | Source | Action |
|------|--------|--------|
| `Cmd+W` / `Ctrl+W` | [CURSOR] | Close current editor tab |
| `Cmd+K W` / `Ctrl+K W` | [CURSOR] | Close all editor tabs |
| `Cmd+\` / `Ctrl+\` | [CURSOR] | Split editor (side by side) |
| `Cmd+K Cmd+\` / `Ctrl+K Ctrl+\` | [CURSOR] | Split editor (three columns) |
| `Cmd+1/2/3` / `Ctrl+1/2/3` | [CURSOR] | Focus editor group 1/2/3 |
| `Cmd+K Cmd+Left/Right` | [CURSOR] | Move editor to next/previous group |
| `Cmd+Option+Left/Right` | [CURSOR] | Navigate between editor groups |

### Cursor Panel Management

| Keys | Source | Action |
|------|--------|--------|
| `Cmd+J` / `Ctrl+J` | [CURSOR] | Toggle bottom panel (terminal/output) |
| `Cmd+\`` / `Ctrl+\`` | [CURSOR] | Toggle integrated terminal |
| `Cmd+Shift+\`` / `Ctrl+Shift+\`` | [CURSOR] | Create new terminal |
| `Cmd+K Cmd+\`` | [CURSOR] | Split terminal |
| `Cmd+Option+Up/Down` | [CURSOR] | Resize panel up/down |

### Cursor Sidebar & Explorer

| Keys | Source | Action |
|------|--------|--------|
| `Cmd+B` / `Ctrl+B` | [CURSOR] | Toggle sidebar |
| `Cmd+Shift+E` / `Ctrl+Shift+E` | [CURSOR] | Focus file explorer |
| `Cmd+Shift+F` / `Ctrl+Shift+F` | [CURSOR] | Focus search |
| `Cmd+Shift+G` / `Ctrl+Shift+G` | [CURSOR] | Focus source control |
| `Cmd+Shift+D` / `Ctrl+Shift+D` | [CURSOR] | Focus debug |
| `Cmd+Shift+X` / `Ctrl+Shift+X` | [CURSOR] | Focus extensions |

### Cursor Command Palette & Quick Open

| Keys | Source | Action |
|------|--------|--------|
| `Cmd+P` / `Ctrl+P` | [CURSOR] | Quick Open (find file by name) |
| `Cmd+Shift+P` / `Ctrl+Shift+P` | [CURSOR] | Command Palette |
| `Cmd+T` / `Ctrl+T` | [CURSOR] | Go to Symbol in Workspace |
| `Cmd+Shift+O` / `Ctrl+Shift+O` | [CURSOR] | Go to Symbol in File |
| `Cmd+P` then `@` | [CURSOR] | Go to Symbol in current file |
| `Cmd+P` then `#` | [CURSOR] | Search by symbol name |
| `Cmd+P` then `>` | [CURSOR] | Run command |

### Cursor AI Features

| Keys | Source | Action |
|------|--------|--------|
| `Cmd+L` / `Ctrl+L` | [CURSOR] | Open Cursor Chat |
| `Cmd+K` / `Ctrl+K` | [CURSOR] | Inline edit (AI code generation) |
| `Tab` | [CURSOR] | Accept AI suggestion |
| `Esc` | [CURSOR] | Dismiss AI suggestion |
| `Cmd+Shift+L` / `Ctrl+Shift+L` | [CURSOR] | Open Composer (multi-file editing) |

### Cursor Terminal Integration

| Keys | Source | Action |
|------|--------|--------|
| `Cmd+\`` / `Ctrl+\`` | [CURSOR] | Toggle terminal |
| `Cmd+Shift+\`` / `Ctrl+Shift+\`` | [CURSOR] | New terminal |
| `Cmd+K Cmd+\`` | [CURSOR] | Split terminal |
| `Cmd+Option+Up/Down` | [CURSOR] | Resize terminal panel |
| `Cmd+Up/Down` | [CURSOR] | Scroll terminal up/down |
| `Cmd+PageUp/PageDown` | [CURSOR] | Switch terminal tabs |

**Note:** Cursor's integrated terminal runs in the bottom panel. You can have multiple terminal tabs, but they're managed by Cursor, not tmux.

---

## STAGE 2: Neovim in Cursor (vscode-neovim)

The vscode-neovim extension embeds Neovim as the editor backend. Your full Neovim configuration loads, but some keybindings may be intercepted by Cursor.

### vscode-neovim Mode Detection

| Variable | Value | Meaning |
|----------|-------|---------|
| `g:vscode` | `1` | Running in vscode-neovim (Cursor/VSCode) |

### vscode-neovim Default Keybindings (May Need Override)

| Keys | Source | Action | Override Needed? |
|------|--------|--------|------------------|
| `gd` / `Ctrl+]` | [vscode-neovim] | Go to definition (Cursor native) | **YES** - Remove in keybindings.json |
| `gf` | [vscode-neovim] | Go to declaration (Cursor native) | Optional |
| `gH` | [vscode-neovim] | Find references (Cursor native) | Optional |
| `gO` | [vscode-neovim] | Go to symbol in workspace | Optional |
| `K` | [vscode-neovim] | Show hover (Cursor native) | Optional |
| `gh` | [vscode-neovim] | Show hover (alternative) | Optional |

**Important:** To use your custom `gd` handler, you must remove vscode-neovim's binding in Cursor's `keybindings.json`:

```json
[
  {
    "key": "gd",
    "command": "-editor.action.revealDefinition",
    "when": "editorTextFocus && neovim.init && !inQuickOpen"
  },
  {
    "key": "ctrl+]",
    "command": "-editor.action.revealDefinition",
    "when": "editorTextFocus && neovim.init && !inQuickOpen"
  }
]
```

### Neovim Window Navigation in Cursor

| Keys | Source | Action |
|------|--------|--------|
| `Ctrl+h` | [mappings.lua] | Navigate left (Neovim split) |
| `Ctrl+j` | [mappings.lua] | Navigate down (Neovim split) |
| `Ctrl+k` | [mappings.lua] | Navigate up (Neovim split) |
| `Ctrl+l` | [mappings.lua] | Navigate right (Neovim split) |
| `Ctrl+w v` | [NEOVIM] | Split vertical (Neovim split) |
| `Ctrl+w s` | [NEOVIM] | Split horizontal (Neovim split) |
| `Ctrl+w q` | [NEOVIM] | Close current Neovim window |
| `Ctrl+w o` | [NEOVIM] | Close all other Neovim windows |

**Note:** In Cursor, `Ctrl+h/j/k/l` only navigates within Neovim splits, not between Cursor editor groups. Use `Cmd+1/2/3` to switch between Cursor editor groups.

---

## STAGE 3: File Management

### Cursor File Explorer

| Keys | Source | Action |
|------|--------|--------|
| `Cmd+Shift+E` / `Ctrl+Shift+E` | [CURSOR] | Focus file explorer |
| `Cmd+K Cmd+B` / `Ctrl+K Ctrl+B` | [CURSOR] | Toggle file explorer |
| `Right Arrow` | [CURSOR] | Expand folder / open file |
| `Left Arrow` | [CURSOR] | Collapse folder |
| `Enter` | [CURSOR] | Open file |
| `Cmd+Enter` / `Ctrl+Enter` | [CURSOR] | Open file to side |
| `F2` | [CURSOR] | Rename file |
| `Delete` | [CURSOR] | Delete file (move to trash) |
| `Cmd+Down` / `Ctrl+Down` | [CURSOR] | Open file without focusing editor |
| `Cmd+Click` / `Ctrl+Click` | [CURSOR] | Open file in new editor group |

### Neovim File Tree (NvimTree)

| Keys | Source | Action |
|------|--------|--------|
| `Ctrl+n` | [NVCHAD] | Toggle NvimTree |
| `Leader e` | [NVCHAD] | Focus NvimTree |
| `q` | [NVIMTREE] | Close NvimTree |
| `g?` | [NVIMTREE] | Show help (all keybindings) |

**Note:** You can use either Cursor's native explorer or NvimTree. NvimTree provides more vim-like navigation, while Cursor's explorer integrates better with Cursor's features.

### Oil.nvim (Editable File Tree)

| Keys | Source | Action |
|------|--------|--------|
| `-` | [oil.lua] | Open parent directory in oil |
| `:w` | [oil.lua] | Save changes (rename/delete) |

**Note:** Oil opens directories as editable buffers. Edit filenames directly, delete with `dd`, save with `:w`.

---

## STAGE 4: File Finding & Navigation

### Cursor Quick Open

| Keys | Source | Action |
|------|--------|--------|
| `Cmd+P` / `Ctrl+P` | [CURSOR] | Quick Open (fuzzy find files) |
| `Cmd+P` then `@` | [CURSOR] | Go to symbol in current file |
| `Cmd+P` then `#` | [CURSOR] | Search by symbol name |
| `Cmd+T` / `Ctrl+T` | [CURSOR] | Go to Symbol in Workspace |
| `Cmd+Shift+O` / `Ctrl+Shift+O` | [CURSOR] | Go to Symbol in File |

### Neovim Telescope

| Keys | Source | Action |
|------|--------|--------|
| `Leader f f` | [NVCHAD] | Find files |
| `Leader f a` | [NVCHAD] | Find all files (including hidden) |
| `Leader f o` | [NVCHAD] | Find recently opened (oldfiles) |
| `Leader f b` | [NVCHAD] | Find open buffers |
| `Leader f w` | [NVCHAD] | Live grep (search text) |
| `Leader f W` | [NVCHAD] | Grep word under cursor |

### Harpoon — Fast File Navigation

| Keys | Source | Action |
|------|--------|--------|
| `Leader hm` | [harpoon.lua] | Mark current file (add to harpoon list) |
| `Leader hh` | [harpoon.lua] | Toggle harpoon quick menu |
| `Leader h1-h9` | [harpoon.lua] | Jump to harpoon file 1-9 |
| `Leader hn` | [harpoon.lua] | Navigate to next harpoon file |
| `Leader hp` | [harpoon.lua] | Navigate to previous harpoon file |

**Usage:** Mark your most frequently accessed files with `Leader hm`, then use `Leader h1-h9` for instant access.

---

## STAGE 5: Code Navigation & LSP

### LSP Navigation (Custom Enhanced)

| Keys | Source | Action |
|------|--------|--------|
| `gd` | [mappings.lua] | Go to definition (with navigation support) |
| `]d` | [mappings.lua] | Next definition/reference/implementation |
| `[d` | [mappings.lua] | Previous definition/reference/implementation |
| `gr` | [mappings.lua] | Find references (with navigation support) |
| `gi` | [mappings.lua] | Go to implementation (with navigation support) |
| `K` | [ufo.lua] | Hover documentation (or peek fold) |
| `]w` | [mappings.lua] | Next word with definition |
| `[w` | [mappings.lua] | Previous word with definition |
| `gD` | [NVCHAD lspconfig] | Go to declaration |
| `Ctrl+k` | [NVCHAD lspconfig] | Signature help (insert mode) |

**Important:** For `gd` to use your custom handler, you must remove vscode-neovim's binding in `keybindings.json` (see Stage 2).

### Cursor Native Code Navigation

| Keys | Source | Action |
|------|--------|--------|
| `F12` | [CURSOR] | Go to definition (Cursor native) |
| `Alt+F12` | [CURSOR] | Peek definition |
| `Shift+F12` | [CURSOR] | Find all references |
| `Ctrl+F12` | [CURSOR] | Go to implementation |
| `F2` | [CURSOR] | Rename symbol |
| `Shift+Alt+F` | [CURSOR] | Format document |
| `Ctrl+.` / `Cmd+.` | [CURSOR] | Quick fix / code actions |

**Note:** You can use either Neovim's LSP navigation or Cursor's native features. They work independently.

### LSP Diagnostics & Code Actions

| Keys | Source | Action |
|------|--------|--------|
| `Leader c a` | [NVCHAD] | Code actions |
| `Leader r a` | [NVCHAD] | Rename symbol (inc-rename) |
| `Leader f m` | [NVCHAD] | Format |
| `[d` | [NVCHAD lspconfig] | Previous diagnostic |
| `]d` | [NVCHAD lspconfig] | Next diagnostic |
| `Leader d` | [NVCHAD] | Floating diagnostic |
| `Leader q` | [NVCHAD] | Diagnostic loclist |
| `Leader d s` | [NVCHAD] | Diagnostic loclist |

### Trouble.nvim — Better Diagnostics

| Keys | Source | Action |
|------|--------|--------|
| `Leader xx` | [trouble.lua] | Toggle diagnostics (all files) |
| `Leader xX` | [trouble.lua] | Toggle diagnostics (current buffer only) |
| `Leader cs` | [trouble.lua] | Toggle symbols outline |
| `Leader cl` | [trouble.lua] | Toggle LSP definitions/references/etc |
| `Leader xL` | [trouble.lua] | Toggle location list |
| `Leader xQ` | [trouble.lua] | Toggle quickfix list |

---

## STAGE 6: Buffers, Windows & Tabs

### Cursor Editor Tabs

| Keys | Source | Action |
|------|--------|--------|
| `Cmd+W` / `Ctrl+W` | [CURSOR] | Close current tab |
| `Cmd+K W` / `Ctrl+K W` | [CURSOR] | Close all tabs |
| `Cmd+K Cmd+Shift+W` | [CURSOR] | Close other tabs |
| `Cmd+Option+Left/Right` | [CURSOR] | Navigate between tabs |
| `Cmd+1/2/3/...` | [CURSOR] | Go to tab 1/2/3/... |
| `Cmd+P` / `Ctrl+P` | [CURSOR] | Quick switch between open files |

### Neovim Buffers

| Keys | Source | Action |
|------|--------|--------|
| `Tab` | [NVCHAD] | Next buffer |
| `Shift+Tab` | [NVCHAD] | Previous buffer |
| `Leader x` | [NVCHAD] | Close current buffer |
| `Leader b` | [NVCHAD] | New buffer |
| `:ls` | [NEOVIM] | List all buffers |
| `:b <num>` | [NEOVIM] | Go to buffer number |
| `:b <name>` | [NEOVIM] | Go to buffer by partial name |
| `Ctrl+^` | [NEOVIM] | Switch to alternate buffer |

**Note:** In Cursor, editor tabs are managed by Cursor, while Neovim buffers are managed by Neovim. They can overlap but are separate concepts.

### Neovim Window/Split Management

| Keys | Source | Action |
|------|--------|--------|
| `Ctrl+h` | [mappings.lua] | Move to left window |
| `Ctrl+j` | [mappings.lua] | Move to window below |
| `Ctrl+k` | [mappings.lua] | Move to window above |
| `Ctrl+l` | [mappings.lua] | Move to right window |
| `Ctrl+w v` | [NEOVIM] | Split vertical |
| `Ctrl+w s` | [NEOVIM] | Split horizontal |
| `Ctrl+w q` | [NEOVIM] | Close current window |
| `Ctrl+w o` | [NEOVIM] | Close all other windows |
| `Ctrl+w =` | [NEOVIM] | Make all windows equal size |

**Note:** Neovim splits exist within a single Cursor editor tab. Use Cursor's editor groups (`Cmd+\`) to create side-by-side editors.

---

## STAGE 7: Search & Replace

### Cursor Search

| Keys | Source | Action |
|------|--------|--------|
| `Cmd+F` / `Ctrl+F` | [CURSOR] | Find in file |
| `Cmd+Shift+F` / `Ctrl+Shift+F` | [CURSOR] | Find in workspace |
| `Cmd+H` / `Ctrl+H` | [CURSOR] | Replace in file |
| `Cmd+Shift+H` / `Ctrl+Shift+H` | [CURSOR] | Replace in workspace |
| `F3` / `Shift+F3` | [CURSOR] | Find next/previous |
| `Enter` | [CURSOR] | Find next (in search box) |
| `Shift+Enter` | [CURSOR] | Find previous |

### Neovim Search

| Keys | Source | Action |
|------|--------|--------|
| `/` | [NEOVIM] | Search forward |
| `?` | [NEOVIM] | Search backward |
| `n` | [NEOVIM] | Repeat search (same direction) |
| `N` | [NEOVIM] | Repeat search (opposite direction) |
| `*` | [NEOVIM] | Search word under cursor forward |
| `#` | [NEOVIM] | Search word under cursor backward |
| `Leader n` | [NVCHAD] | Clear search highlight |

### Telescope Live Grep

| Keys | Source | Action |
|------|--------|--------|
| `Leader f w` | [NVCHAD] | Live grep (search text in project) |
| `Leader f W` | [NVCHAD] | Grep word under cursor |

### Grug-Far.nvim — Search and Replace with Preview

| Keys | Mode | Source | Action |
|------|------|--------|--------|
| `Leader sr` | n,v | [grug-far.lua] | Open search and replace UI |

**Features:** Live preview of replacements, regex support, file type filtering.

---

## STAGE 8: Git Integration

### Cursor Source Control

| Keys | Source | Action |
|------|--------|--------|
| `Cmd+Shift+G` / `Ctrl+Shift+G` | [CURSOR] | Focus source control |
| `Ctrl+Shift+G G` | [CURSOR] | Open source control |
| `+` | [CURSOR] | Stage file |
| `-` | [CURSOR] | Unstage file |
| `U` | [CURSOR] | Discard changes |
| `Cmd+Enter` / `Ctrl+Enter` | [CURSOR] | Commit |

### Neovim Git Tools

| Keys | Source | Action |
|------|--------|--------|
| `Leader gg` | [lazygit.lua] | Open LazyGit (full git workflow UI) |
| `Leader gb` | [init.lua] | Open git blame window |
| `Leader go` | [init.lua] | Open file/folder in git repository (browser) |
| `Leader gy` | [gitlinker.lua] | Copy GitHub/GitLab link to clipboard |
| `Leader g t` | [NVCHAD] | Git status (Telescope) |
| `Leader g c` | [NVCHAD] | Git commits (Telescope) |

**Commands:** `:LazyGit`, `:LazyGitConfig`, `:LazyGitCurrentFile`, `:LazyGitFilter`

---

## STAGE 9: Terminal Workflow

### Cursor Integrated Terminal

| Keys | Source | Action |
|------|--------|--------|
| `Cmd+\`` / `Ctrl+\`` | [CURSOR] | Toggle terminal |
| `Cmd+Shift+\`` / `Ctrl+Shift+\`` | [CURSOR] | New terminal |
| `Cmd+K Cmd+\`` | [CURSOR] | Split terminal |
| `Cmd+Option+Up/Down` | [CURSOR] | Resize terminal panel |
| `Cmd+Up/Down` | [CURSOR] | Scroll terminal up/down |
| `Cmd+PageUp/PageDown` | [CURSOR] | Switch terminal tabs |

### Neovim Terminal Buffers

| Keys | Source | Action |
|------|--------|--------|
| `Leader h` | [NVCHAD] | Horizontal terminal |
| `Leader v` | [NVCHAD] | Vertical terminal |
| `Leader ac` | [mappings.lua] | Launch Claude in terminal buffer |
| `Alt+h` | [NVCHAD] | Toggle horizontal terminal |
| `Alt+v` | [NVCHAD] | Toggle vertical terminal |
| `Alt+i` | [NVCHAD] | Toggle floating terminal |
| `Ctrl+x` | [NVCHAD] | Exit terminal mode |
| `:Claude` | [mappings.lua] | Launch Claude CLI in terminal |

**Shell Alias:** `cld` — Claude CLI with required flags (configured in .bashrc)

**Note:** In Cursor, you can use either Cursor's integrated terminal or Neovim's terminal buffers. Cursor's terminal is better for long-running processes, while Neovim's terminal integrates with your vim workflow.

---

## STAGE 10: Modal Editing — Complete

### Your Custom Mappings (mappings.lua)

| Keys | Source | Action |
|------|--------|--------|
| `jk` | [mappings.lua] | Exit insert mode (alternative to Esc) |
| `Ctrl+h/j/k/l` | [mappings.lua] | Navigate windows (Neovim splits) |
| `Leader ac` | [mappings.lua] | Launch Claude in terminal buffer |
| `Leader gw` | [mappings.lua] | Open workflow guide (absolute path) |
| `:Claude` | [mappings.lua] | User command to launch Claude |

### Mode Switching

| Keys | Source | Action |
|------|--------|--------|
| `i` | [NEOVIM] | Insert before cursor |
| `a` | [NEOVIM] | Insert after cursor |
| `I` | [NEOVIM] | Insert at line start |
| `A` | [NEOVIM] | Insert at line end |
| `o` | [NEOVIM] | Insert on new line below |
| `O` | [NEOVIM] | Insert on new line above |
| `v` | [NEOVIM] | Visual character mode |
| `V` | [NEOVIM] | Visual line mode |
| `Ctrl+v` | [NEOVIM] | Visual block mode |
| `Esc` | [NEOVIM] | Return to normal mode |
| `jk` | [mappings.lua] | Return to normal mode |

### Basic Motion

| Keys | Source | Action |
|------|--------|--------|
| `h/j/k/l` | [NEOVIM] | Left/Down/Up/Right |
| `w` | [NEOVIM] | Next word start |
| `b` | [NEOVIM] | Previous word start |
| `e` | [NEOVIM] | Next word end |
| `0` | [NEOVIM] | Line start |
| `^` | [NEOVIM] | First non-whitespace |
| `$` | [NEOVIM] | Line end |
| `gg` | [NEOVIM] | Go to first line |
| `G` | [NEOVIM] | Go to last line |
| `{n}G` | [NEOVIM] | Go to line n |
| `{n}%` | [mappings.lua] | Jump to n% of file (repeatable with `.`) |

### Operators

| Keys | Source | Action |
|------|--------|--------|
| `d{motion}` | [NEOVIM] | Delete |
| `c{motion}` | [NEOVIM] | Change (delete and insert) |
| `y{motion}` | [NEOVIM] | Yank (copy) |
| `>{motion}` | [NEOVIM] | Indent right |
| `<{motion}` | [NEOVIM] | Indent left |
| `={motion}` | [NEOVIM] | Auto-indent |

### Text Objects

| Keys | Source | Action |
|------|--------|--------|
| `iw` | [NEOVIM] | Inner word |
| `aw` | [NEOVIM] | A word (includes surrounding space) |
| `ip` | [NEOVIM] | Inner paragraph |
| `ap` | [NEOVIM] | A paragraph |
| `i"` / `a"` | [NEOVIM] | Inside/around double quotes |
| `i(` / `a(` | [NEOVIM] | Inside/around parentheses |
| `i[` / `a[` | [NEOVIM] | Inside/around brackets |
| `i{` / `a{` | [NEOVIM] | Inside/around braces |

### Treesitter Text Objects (AST-Aware)

| Keys | Source | Action |
|------|--------|--------|
| `af` | [treesitter-textobjects] | Around function (outer) |
| `if` | [treesitter-textobjects] | Inner function |
| `ac` | [treesitter-textobjects] | Around class (outer) |
| `ic` | [treesitter-textobjects] | Inner class |
| `aa` | [treesitter-textobjects] | Around parameter |
| `ia` | [treesitter-textobjects] | Inner parameter |
| `]f` | [treesitter-textobjects] | Next function start |
| `[f` | [treesitter-textobjects] | Previous function start |

---

## STAGE 11: Additional Plugins

### Flash.nvim — Enhanced Navigation

| Keys | Mode | Source | Action |
|------|------|--------|--------|
| `s` | n,x,o | [flash.lua] | Flash jump (type chars, jump to label) |
| `S` | n,x,o | [flash.lua] | Flash Treesitter (select syntax nodes) |

### TODO Comments

| Keys | Source | Action |
|------|--------|--------|
| `]t` | [todo-comments.lua] | Jump to next TODO comment |
| `[t` | [todo-comments.lua] | Jump to previous TODO comment |
| `Leader TA` | [todo-comments.lua] | Search all TODOs (Telescope) |
| `Leader TT` | [todo-comments.lua] | Search TODO/FIX only (Telescope) |

### Refactoring.nvim

| Keys | Mode | Source | Action |
|------|------|--------|--------|
| `Leader re` | n,v | [refactoring.lua] | Open refactor menu |

### UFO.nvim — Better Folding

| Keys | Source | Action |
|------|--------|--------|
| `zR` | [ufo.lua] | Open all folds |
| `zM` | [ufo.lua] | Close all folds |
| `K` | [ufo.lua] | Peek fold preview OR LSP hover |

### NeoCodeium — AI Code Completion

| Keys | Mode | Source | Action |
|------|------|--------|--------|
| `Alt+f` | i | [neocodeium.lua] | Accept full completion |
| `Alt+w` | i | [neocodeium.lua] | Accept word only |
| `Alt+a` | i | [neocodeium.lua] | Accept line only |
| `Alt+e` | i | [neocodeium.lua] | Cycle to next suggestion |
| `Alt+r` | i | [neocodeium.lua] | Cycle to previous suggestion |
| `Alt+c` | i | [neocodeium.lua] | Clear current completion |

**Commands:** `:NeoCodeium auth`, `:NeoCodeium toggle`, `:NeoCodeium chat`

**Note:** NeoCodeium provides free AI completions. Cursor also has built-in AI (Cmd+K), so you have two AI systems available.

---

## STAGE 12: WhichKey & Help

### WhichKey

| Keys | Source | Action |
|------|--------|--------|
| `Leader` (wait) | [which-key.lua] | Show all keybindings in large panel |

**Features:**
- Large panel mode — see all mappings without scrolling
- Full visibility — no paging or collapsing
- Rehearsal mode — perfect for learning keybindings
- Shows all leader key combinations

### Cursor Help

| Keys | Source | Action |
|------|--------|--------|
| `Cmd+Shift+P` / `Ctrl+Shift+P` | [CURSOR] | Command Palette |
| `F1` | [CURSOR] | Show all commands |
| `Cmd+K Cmd+S` / `Ctrl+K Ctrl+S` | [CURSOR] | Keyboard shortcuts editor |

### Neovim Help

| Keys | Source | Action |
|------|--------|--------|
| `Leader c h` | [NVCHAD] | Open NvChad cheatsheet |
| `Leader gw` | [mappings.lua] | Open workflow guide |
| `:h {topic}` | [NEOVIM] | Neovim help |
| `:h index` | [NEOVIM] | Index of all commands |
| `:h quickref` | [NEOVIM] | Quick reference |

---

## Comprehensive Navigation Workflow

### Daily Project Exploration Routine

**1. Initial Project Setup (First 5 minutes)**
```
1. Cmd+P (Cursor) or Leader ff (Telescope) → Find file
2. Navigate to main entry point
3. Leader hm → Mark file (Harpoon #1)
4. Find related files
5. Leader hm → Mark 4 more files (Harpoon #2-5)
```

**2. Understanding Code Flow**
```
1. Place cursor on function/class name
2. gd → Go to definition (your custom handler)
3. ]d → Next definition (if multiple)
4. [d → Previous definition
5. gr → Find all references
6. ]d → Navigate through references
7. gi → Find implementations
8. ]d → Navigate implementations
9. Ctrl+o → Jump back (vim jump list)
```

**3. Quick File Switching (Harpoon)**
```
Leader h1 → Jump to file #1
Leader h2 → Jump to file #2
Leader h3 → Jump to file #3
Leader h4 → Jump to file #4
Leader h5 → Jump to file #5
Leader hh → See all marked files
Leader hn → Next in harpoon list
Leader hp → Previous in harpoon list
```

**4. Learning Definitions (Hover & Navigate)**
```
1. K → Hover (show definition/tooltip)
2. ]w → Next word with definition
3. [w → Previous word with definition
4. gd → Jump to definition
5. Ctrl+o → Jump back
```

**5. Reference Navigation**
```
1. gr → Find all references
2. ]d → Next reference
3. [d → Previous reference
4. Leader yf → Yank file path (if you want to note it)
```

---

## Quick Reference: Most Important Keys

### "I need to..."

| Task | Keys | Source |
|------|------|--------|
| Open file tree | `Ctrl+n` (NvimTree) or `Cmd+Shift+E` (Cursor) | [NVCHAD] / [CURSOR] |
| Find file by name | `Cmd+P` (Cursor) or `Leader f f` (Telescope) | [CURSOR] / [NVCHAD] |
| Search text in project | `Cmd+Shift+F` (Cursor) or `Leader f w` (Telescope) | [CURSOR] / [NVCHAD] |
| Go to definition | `gd` then `]d`/`[d` | [mappings.lua] |
| Navigate LSP results | `]d` / `[d` | [mappings.lua] |
| Switch buffer | `Tab` / `Shift+Tab` | [NVCHAD] |
| Switch editor tab | `Cmd+Option+Left/Right` | [CURSOR] |
| Close buffer | `Leader x` | [NVCHAD] |
| Close tab | `Cmd+W` | [CURSOR] |
| Save file | `:w Enter` or `Ctrl+s` | [NEOVIM] / [NVCHAD] |
| Split editor | `Cmd+\` (Cursor) or `Ctrl+w v` (Neovim) | [CURSOR] / [NEOVIM] |
| Move between windows | `Ctrl+h/j/k/l` | [mappings.lua] |
| Toggle terminal | `Cmd+\`` | [CURSOR] |
| Open AI chat | `Cmd+L` | [CURSOR] |
| Inline AI edit | `Cmd+K` | [CURSOR] |
| Show all keybindings | `Leader` (wait) | [which-key.lua] |
| Launch Claude | `Leader ac` | [mappings.lua] |
| Jump to percentage | `{n}%` then `.` | [mappings.lua] |
| Toggle comment | `Leader /` or `gcc` | [NVCHAD] |
| Accept AI completion | `Alt+f` (NeoCodeium) or `Tab` (Cursor) | [neocodeium.lua] / [CURSOR] |

---

## Cursor vs WezTerm Workflow Comparison

### WezTerm + tmux + Neovim Workflow
- **Terminal-first:** Everything runs in terminal panes
- **tmux management:** Session persistence, pane splitting, window management
- **Seamless navigation:** vim-tmux-navigator works across all panes
- **Terminal tabs:** Independent bash/Neovim tabs via WezTerm
- **Best for:** Terminal-heavy workflows, server management, CLI tools

### Cursor + Neovim Workflow
- **IDE-first:** Full IDE features (debugger, extensions, AI)
- **Integrated panels:** Terminal, explorer, search all built-in
- **AI features:** Built-in chat, inline editing, composer
- **No tmux needed:** Cursor handles tabs/panels
- **Best for:** Modern development, AI-assisted coding, GUI features

### Key Differences

| Feature | WezTerm + tmux | Cursor |
|---------|----------------|--------|
| Terminal | tmux panes | Integrated terminal panel |
| File explorer | NvimTree or terminal | Cursor explorer + NvimTree |
| Tabs | tmux windows | Cursor editor tabs |
| AI | Claude CLI | Cursor AI + Claude CLI |
| Debugging | Terminal-based | Built-in debugger |
| Extensions | Neovim plugins only | VS Code extensions + Neovim plugins |
| Navigation | vim-tmux-navigator | Cursor editor groups + Neovim splits |

---

## Practice Drills

### Drill 1: Cursor Editor Management
1. `Cmd+\` [CURSOR] — split editor
2. `Cmd+1` [CURSOR] — focus left editor
3. `Cmd+2` [CURSOR] — focus right editor
4. `Cmd+W` [CURSOR] — close current tab
5. `Cmd+P` [CURSOR] — quick open file
6. `Cmd+Option+Left` [CURSOR] — navigate tabs

### Drill 2: File Operations
1. `Cmd+Shift+E` [CURSOR] — open explorer
2. `Right Arrow` [CURSOR] — expand folder
3. `Enter` [CURSOR] — open file
4. `F2` [CURSOR] — rename file
5. `Cmd+P` [CURSOR] — quick open another file

### Drill 3: Code Navigation
1. Place cursor on function name
2. `gd` [mappings.lua] — go to definition
3. `]d` [mappings.lua] — next definition
4. `gr` [mappings.lua] — find references
5. `]d` [mappings.lua] — navigate references
6. `Ctrl+o` [NEOVIM] — jump back

### Drill 4: AI Features
1. `Cmd+L` [CURSOR] — open Cursor chat
2. `Cmd+K` [CURSOR] — inline edit
3. `Tab` [CURSOR] — accept suggestion
4. `Leader ac` [mappings.lua] — launch Claude CLI
5. `Alt+f` [neocodeium.lua] — accept NeoCodeium completion

### Drill 5: Terminal Workflow
1. `Cmd+\`` [CURSOR] — toggle terminal
2. `Cmd+Shift+\`` [CURSOR] — new terminal
3. `Cmd+K Cmd+\`` [CURSOR] — split terminal
4. `Cmd+PageUp` [CURSOR] — switch terminal tabs
5. `Leader h` [NVCHAD] — Neovim horizontal terminal

---

## Configuration Requirements

### Cursor keybindings.json

To enable your custom `gd` handler, add this to Cursor's `keybindings.json`:

```json
[
  {
    "key": "gd",
    "command": "-editor.action.revealDefinition",
    "when": "editorTextFocus && neovim.init && !inQuickOpen"
  },
  {
    "key": "ctrl+]",
    "command": "-editor.action.revealDefinition",
    "when": "editorTextFocus && neovim.init && !inQuickOpen"
  }
]
```

**How to access:** `Cmd+Shift+P` → "Preferences: Open Keyboard Shortcuts (JSON)"

### vscode-neovim Settings

In Cursor's `settings.json`:

```json
{
  "vscode-neovim.neovimExecutablePaths.linux": "/usr/bin/nvim",
  "vscode-neovim.neovimInitVimPaths.linux": "/home/bw/.config/nvim/init.lua"
}
```

---

*Complete guide for Cursor IDE + Neovim workflow. Built-in help: `Leader c h` [NVCHAD], `Cmd+Shift+P` [CURSOR], `g?` [NVIMTREE], `:h` [NEOVIM]*

