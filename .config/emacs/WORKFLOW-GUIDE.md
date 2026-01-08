# Doom Emacs Workflow Guide — Complete Edition

A comprehensive guide to using Doom Emacs, optimized for modern development workflows.

**Legend — Where each mapping comes from:**
- `[DOOM]` = Doom Emacs default keybinding
- `[EVIL]` = Evil mode (Vim) default
- `[EMACS]` = Vanilla Emacs default
- `[config.el]` = Your custom configuration
- `[VERTICO]` = Vertico completion system
- `[CONSULT]` = Consult package
- `[EMBARK]` = Embark package
- `[ORG]` = Org mode
- `[EGLOT]` = EGLOT LSP client
- `[HARPER]` = Harper grammar checker

**Leader** = `SPC` (Space) - Doom Emacs default
**Command Palette** = `M-x` (Alt+x) or `SPC :` (Space + colon)

---

## Quick Reference: Essential Commands

### File Operations

| Keys | Source | Action |
|------|--------|--------|
| `SPC f f` | [DOOM] | Find file in project |
| `SPC f r` | [DOOM] | Find recent files |
| `SPC f s` | [DOOM] | Save file |
| `SPC f S` | [DOOM] | Save all files |
| `SPC f u` | [DOOM] | Sudo edit file |
| `SPC f y` | [DOOM] | Copy file path |
| `SPC f w` | [config.el] | Open workflow guide |
| `C-s` | [config.el] | Save buffer |
| `C-x C-f` | [config.el] | Find file (in minibuffer) |

### Buffer Management

| Keys | Source | Action |
|------|--------|--------|
| `SPC b b` | [DOOM] | Switch buffer |
| `SPC b d` | [DOOM] | Kill buffer |
| `SPC b D` | [DOOM] | Kill buffer (force) |
| `SPC b n` | [DOOM] | Next buffer |
| `SPC b p` | [DOOM] | Previous buffer |
| `SPC b s` | [DOOM] | Save buffer |
| `SPC TAB` | [DOOM] | Switch to last buffer |
| `:b <name>` | [EVIL] | Switch to buffer by name |

### Window Navigation

| Keys | Source | Action |
|------|--------|--------|
| `C-h` | [config.el] | Move to left window |
| `C-j` | [config.el] | Move to window below |
| `C-k` | [config.el] | Move to window above |
| `C-l` | [config.el] | Move to right window |
| `SPC w h` | [DOOM] | Move to left window |
| `SPC w j` | [DOOM] | Move to window below |
| `SPC w k` | [DOOM] | Move to window above |
| `SPC w l` | [DOOM] | Move to right window |
| `SPC w v` | [DOOM] | Split window vertically |
| `SPC w s` | [DOOM] | Split window horizontally |
| `SPC w d` | [DOOM] | Delete window |
| `SPC w o` | [DOOM] | Close other windows |
| `S-<left>` | [config.el] | Resize window left/right |
| `S-<right>` | [config.el] | Resize window left/right |
| `S-<up>` | [config.el] | Resize window up/down |
| `S-<down>` | [config.el] | Resize window up/down |

### Search & Navigation

| Keys | Source | Action |
|------|--------|--------|
| `SPC s s` | [DOOM] | Search in current buffer |
| `SPC s S` | [DOOM] | Search in project |
| `SPC s p` | [DOOM] | Search in project (ripgrep) |
| `SPC s r` | [DOOM] | Search in project (ripgrep) |
| `SPC s b` | [DOOM] | Search in open buffers |
| `SPC s h` | [config.el] | Command history (consult) |
| `SPC s d` | [config.el] | Dictionary lookup |
| `SPC s D` | [config.el] | Thesaurus lookup |
| `/` | [EVIL] | Search forward |
| `?` | [EVIL] | Search backward |
| `n` | [EVIL] | Next search result |
| `N` | [EVIL] | Previous search result |
| `*` | [EVIL] | Search word under cursor forward |
| `#` | [EVIL] | Search word under cursor backward |

### Code Navigation & LSP

| Keys | Source | Action |
|------|--------|--------|
| `SPC c c` | [DOOM] | Compile |
| `SPC c C` | [DOOM] | Recompile |
| `SPC c k` | [DOOM] | Kill compilation |
| `gd` | [EVIL] | Go to definition |
| `gr` | [EVIL] | Find references |
| `K` | [EVIL] | Show documentation/hover |
| `SPC c d` | [DOOM] | Show diagnostics |
| `SPC c e` | [DOOM] | List errors |
| `SPC c l` | [DOOM] | List linters |
| `SPC c r` | [DOOM] | Rename symbol |
| `SPC c f` | [DOOM] | Format buffer |

### Completion & Minibuffer

| Keys | Source | Action |
|------|--------|--------|
| `C-j` | [VERTICO] | Next completion candidate |
| `C-k` | [VERTICO] | Previous completion candidate |
| `M-RET` | [VERTICO] | Exit with input |
| `M-p` | [VERTICO] | Previous history |
| `M-n` | [VERTICO] | Next history |
| `C-r` | [VERTICO] | Consult history |
| `DEL` | [VERTICO] | Delete directory char |
| `M-DEL` | [VERTICO] | Delete directory word |
| `C-x C-d` | [config.el] | Consult directory |
| `C-x C-j` | [config.el] | Jump to file in directory |

### Embark Actions

| Keys | Source | Action |
|------|--------|--------|
| `SPC k a` | [config.el] | Embark act |
| `SPC k d` | [config.el] | Embark dwim (do what I mean) |
| `SPC k c` | [config.el] | Embark collect |

### Project Management

| Keys | Source | Action |
|------|--------|--------|
| `SPC p p` | [DOOM] | Switch project |
| `SPC p f` | [DOOM] | Find file in project |
| `SPC p t` | [DOOM] | Toggle project treemap |
| `SPC p h` | [DOOM] | Find file in home directory |
| `SPC p s` | [DOOM] | Search in project |

### Git Integration

| Keys | Source | Action |
|------|--------|--------|
| `SPC g s` | [DOOM] | Git status |
| `SPC g g` | [DOOM] | Magit status |
| `SPC g c` | [DOOM] | Git commit |
| `SPC g l` | [DOOM] | Git log |
| `SPC g b` | [DOOM] | Git blame |
| `SPC g t` | [DOOM] | Git time machine |

### Org Mode

| Keys | Source | Action |
|------|--------|--------|
| `SPC o a` | [DOOM] | Open agenda |
| `SPC o c` | [DOOM] | Capture |
| `SPC o l` | [DOOM] | Open org link |
| `SPC o t` | [DOOM] | Toggle todo |
| `M-<left>` | [config.el] | Promote heading (org mode) |
| `M-<right>` | [config.el] | Demote heading (org mode) |
| `C-c e` | [config.el] | Set effort (org mode) |
| `C-c i` | [config.el] | Clock in (org mode) |
| `C-c o` | [config.el] | Clock out (org mode) |
| `C-c C-i` | [config.el] | Insert image (org mode) |

### Org Agenda

| Keys | Source | Action |
|------|--------|--------|
| `SPC o a a` | [DOOM] | Open agenda |
| `SPC o a t` | [DOOM] | Open todo list |
| `SPC o a s` | [DOOM] | Search agenda |
| `d` | [ORG] | Dashboard view (custom command) |

### Org Capture Templates

| Template | Source | Action |
|----------|--------|--------|
| `t` | [config.el] | Capture todo |
| `e` | [config.el] | Capture event |
| `d` | [config.el] | Capture deadline |
| `p` | [config.el] | Capture project |
| `i` | [config.el] | Capture idea |
| `b` | [config.el] | Capture bookmark |
| `n` | [config.el] | Capture note |

### Text Editing

| Keys | Source | Action |
|------|--------|--------|
| `i` | [EVIL] | Insert mode |
| `a` | [EVIL] | Insert after cursor |
| `I` | [EVIL] | Insert at line start |
| `A` | [EVIL] | Insert at line end |
| `o` | [EVIL] | New line below |
| `O` | [EVIL] | New line above |
| `v` | [EVIL] | Visual character mode |
| `V` | [EVIL] | Visual line mode |
| `C-v` | [EVIL] | Visual block mode |
| `Esc` | [EVIL] | Normal mode |
| `C-=` | [config.el] | Zoom in (text scale increase) |
| `C--` | [config.el] | Zoom out (text scale decrease) |

### Motion & Navigation

| Keys | Source | Action |
|------|--------|--------|
| `h/j/k/l` | [EVIL] | Left/Down/Up/Right |
| `w` | [EVIL] | Next word start |
| `b` | [EVIL] | Previous word start |
| `e` | [EVIL] | Next word end |
| `0` | [EVIL] | Line start |
| `^` | [EVIL] | First non-whitespace |
| `$` | [EVIL] | Line end |
| `gg` | [EVIL] | Go to first line |
| `G` | [EVIL] | Go to last line |
| `{n}G` | [EVIL] | Go to line n |
| `%` | [EVIL] | Jump to matching bracket |

### Operators

| Keys | Source | Action |
|------|--------|--------|
| `d{motion}` | [EVIL] | Delete |
| `c{motion}` | [EVIL] | Change (delete and insert) |
| `y{motion}` | [EVIL] | Yank (copy) |
| `>{motion}` | [EVIL] | Indent right |
| `<{motion}` | [EVIL] | Indent left |
| `={motion}` | [EVIL] | Auto-indent |
| `p` | [EVIL] | Paste after |
| `P` | [EVIL] | Paste before |

### Text Objects

| Keys | Source | Action |
|------|--------|--------|
| `iw` | [EVIL] | Inner word |
| `aw` | [EVIL] | A word (includes space) |
| `ip` | [EVIL] | Inner paragraph |
| `ap` | [EVIL] | A paragraph |
| `i"` / `a"` | [EVIL] | Inside/around double quotes |
| `i(` / `a(` | [EVIL] | Inside/around parentheses |
| `i[` / `a[` | [EVIL] | Inside/around brackets |
| `i{` / `a{` | [EVIL] | Inside/around braces |

### Terminal

| Keys | Source | Action |
|------|--------|--------|
| `SPC o t` | [DOOM] | Toggle terminal |
| `SPC o T` | [DOOM] | New terminal |
| `:terminal` | [EVIL] | Open terminal buffer |
| `C-x` | [DOOM] | Exit terminal mode |

### Help & Documentation

| Keys | Source | Action |
|------|--------|--------|
| `SPC h f` | [DOOM] | Describe function |
| `SPC h v` | [DOOM] | Describe variable |
| `SPC h k` | [DOOM] | Describe keybinding |
| `SPC h m` | [DOOM] | Describe mode |
| `SPC h h` | [DOOM] | Doom help |
| `SPC h r` | [DOOM] | Reload config |
| `K` | [EVIL] | Show documentation |
| `:h {topic}` | [EVIL] | Vim help |

### Vertico Repeat

| Keys | Source | Action |
|------|--------|--------|
| `SPC r v` | [config.el] | Repeat last completion |

### Consult Directory

| Keys | Source | Action |
|------|--------|--------|
| `SPC s d` | [config.el] | Recent directories |

---

## Workflow Sections

### 1. File Management

**Finding Files:**
- `SPC f f` - Find file in project (uses fd/find)
- `SPC f r` - Recent files
- `SPC p f` - Find file in project (alternative)
- `C-x C-f` - Find file (in minibuffer with Vertico)

**Saving Files:**
- `SPC f s` - Save current file
- `SPC f S` - Save all files
- `C-s` - Quick save (custom)

**File Operations:**
- `SPC f y` - Copy file path
- `SPC f u` - Sudo edit file
- `SPC f w` - Open workflow guide

### 2. Buffer Management

**Switching Buffers:**
- `SPC b b` - Switch buffer (with Vertico)
- `SPC TAB` - Switch to last buffer
- `SPC b n` - Next buffer
- `SPC b p` - Previous buffer

**Buffer Operations:**
- `SPC b d` - Kill buffer
- `SPC b D` - Kill buffer (force)
- `SPC b s` - Save buffer

### 3. Window Management

**Navigation:**
- `C-h/j/k/l` - Vim-style window navigation (custom)
- `SPC w h/j/k/l` - Doom window navigation

**Window Operations:**
- `SPC w v` - Split vertically
- `SPC w s` - Split horizontally
- `SPC w d` - Delete window
- `SPC w o` - Close other windows

**Resizing:**
- `S-<left>/<right>` - Resize width
- `S-<up>/<down>` - Resize height

### 4. Search & Find

**In-Buffer Search:**
- `/` - Search forward (Evil)
- `?` - Search backward (Evil)
- `SPC s s` - Search in buffer (Doom)

**Project Search:**
- `SPC s p` - Search in project (ripgrep)
- `SPC s r` - Search in project (ripgrep, alternative)
- `SPC s S` - Search in project (Doom)

**Buffer Search:**
- `SPC s b` - Search in open buffers

**History:**
- `SPC s h` - Command history (Consult)

### 5. Code Navigation

**LSP Navigation:**
- `gd` - Go to definition
- `gr` - Find references
- `K` - Show hover/documentation

**LSP Actions:**
- `SPC c r` - Rename symbol
- `SPC c f` - Format buffer
- `SPC c d` - Show diagnostics
- `SPC c e` - List errors

**Compilation:**
- `SPC c c` - Compile
- `SPC c C` - Recompile
- `SPC c k` - Kill compilation

### 6. Completion System (Vertico)

**Navigation:**
- `C-j` - Next candidate
- `C-k` - Previous candidate
- `M-RET` - Exit with input

**History:**
- `M-p` - Previous history
- `M-n` - Next history
- `C-r` - Consult history

**Directory Navigation:**
- `DEL` - Delete directory char
- `M-DEL` - Delete directory word
- `C-x C-d` - Consult directory
- `C-x C-j` - Jump to file in directory

### 7. Embark Actions

Embark provides contextual actions on any minibuffer completion:

- `SPC k a` - Embark act (show actions menu)
- `SPC k d` - Embark dwim (do what I mean - smart action)
- `SPC k c` - Embark collect (collect items for batch operations)

**Usage:** Start any completion (e.g., `SPC f f`), then press `SPC k a` to see available actions.

### 8. Org Mode Workflow

**Agenda:**
- `SPC o a` - Open agenda
- `SPC o a a` - Open agenda (alternative)
- `SPC o a t` - Open todo list
- `d` - Dashboard view (custom command)

**Capture:**
- `SPC o c` - Capture (opens capture menu)
- Templates: `t` (todo), `e` (event), `d` (deadline), `p` (project), `i` (idea), `b` (bookmark), `n` (note)

**Org Editing:**
- `M-<left>` - Promote heading
- `M-<right>` - Demote heading
- `C-c e` - Set effort
- `C-c i` - Clock in
- `C-c o` - Clock out
- `C-c C-i` - Insert image

**Auto-Clocking:**
- Automatically clocks in when task state changes to STRT
- Automatically clocks out when leaving STRT state

### 9. Git Workflow

**Magit (Full Git Interface):**
- `SPC g g` - Open Magit status
- `SPC g s` - Git status (alternative)
- `SPC g c` - Git commit
- `SPC g l` - Git log
- `SPC g b` - Git blame
- `SPC g t` - Git time machine

### 10. Project Management

**Project Operations:**
- `SPC p p` - Switch project
- `SPC p f` - Find file in project
- `SPC p t` - Toggle project treemap
- `SPC p h` - Find file in home directory
- `SPC p s` - Search in project

### 11. Text Editing

**Mode Switching:**
- `i` - Insert mode
- `a` - Insert after cursor
- `I` - Insert at line start
- `A` - Insert at line end
- `o` - New line below
- `O` - New line above
- `v` - Visual character mode
- `V` - Visual line mode
- `C-v` - Visual block mode
- `Esc` - Normal mode

**Text Operations:**
- `d{motion}` - Delete
- `c{motion}` - Change (delete and insert)
- `y{motion}` - Yank (copy)
- `p` - Paste after
- `P` - Paste before

**Indentation:**
- `>{motion}` - Indent right
- `<{motion}` - Indent left
- `={motion}` - Auto-indent

**Text Scale:**
- `C-=` - Zoom in
- `C--` - Zoom out

**Line Wrapping:**
- Visual line mode is enabled globally (wraps long lines visually)

### 12. Harper Grammar Checker

Harper provides grammar checking for text-based modes (org, markdown, text):

- Automatically enabled in `text-mode`, `org-mode`, and `markdown-mode`
- Uses EGLOT for LSP integration
- Shows grammar suggestions inline

**Configuration:**
- Harper LSP server (`harper-ls`) must be installed and in PATH
- Download from: https://writewithharper.com/docs/integrations/emacs

### 13. Dictionary & Thesaurus

**Lookup:**
- `SPC s d` - Dictionary lookup
- `SPC s D` - Thesaurus lookup

Uses Doom's lookup module with dictionary support.

---

## Quick Reference: Most Common Tasks

| Task | Keys | Source |
|------|------|--------|
| Find file | `SPC f f` | [DOOM] |
| Switch buffer | `SPC b b` | [DOOM] |
| Save file | `C-s` or `SPC f s` | [config.el] / [DOOM] |
| Search in project | `SPC s p` | [DOOM] |
| Go to definition | `gd` | [EVIL] |
| Find references | `gr` | [EVIL] |
| Show documentation | `K` | [EVIL] |
| Open agenda | `SPC o a` | [DOOM] |
| Capture | `SPC o c` | [DOOM] |
| Git status | `SPC g g` | [DOOM] |
| Switch project | `SPC p p` | [DOOM] |
| Window navigation | `C-h/j/k/l` | [config.el] |
| Zoom in/out | `C-=/C--` | [config.el] |
| Command history | `SPC s h` | [config.el] |
| Open workflow guide | `SPC f w` | [config.el] |

---

## Configuration Files

- **Main Config:** `~/.config/doom/config.el`
- **Init File:** `~/.config/doom/init.el`
- **Packages:** `~/.config/doom/packages.el`
- **Workflow Guide:** `~/.config/doom/WORKFLOW-GUIDE.md`

---

## Custom Features

### Window Navigation
Custom vim-style window navigation using `C-h/j/k/l` for seamless window switching.

### Vertico Completion
Modern minibuffer completion with:
- History navigation
- Directory navigation
- Quick actions

### Consult Integration
Enhanced search and navigation with:
- Command history
- Directory browsing
- Better previews

### Embark Actions
Contextual actions on any completion candidate.

### Org Mode Enhancements
- Custom agenda dashboard
- Auto-clocking on task state changes
- Custom capture templates
- Image insertion

### Performance Optimizations
- GC threshold: 256MB
- Deferred native compilation
- 8 parallel compilation jobs
- GCMH idle delay: 5 seconds

---

*Complete guide for Doom Emacs workflow. Built-in help: `SPC h h` [DOOM], `M-x` [EMACS], `:h` [EVIL]*
