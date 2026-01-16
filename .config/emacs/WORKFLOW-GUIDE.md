# Doom Emacs Workflow Guide — Default Keybindings Edition

A comprehensive guide to using Doom Emacs with default keybindings.

**Legend — Where each mapping comes from:**
- `[DOOM]` = Doom Emacs default keybinding
- `[EVIL]` = Evil mode (Vim) default
- `[EMACS]` = Vanilla Emacs default
- `[VERTICO]` = Vertico completion system
- `[CONSULT]` = Consult package
- `[ORG]` = Org mode
- `[config.el]` = Your custom configuration

**Leader** = `SPC` (Space) - Doom Emacs default
**Local Leader** = `SPC m` or `,` - Mode-specific commands
**Command Palette** = `M-x` (Alt+x) or `SPC :` (Space + colon)

---

## Quick Reference: Essential Commands

### File Operations

| Keys | Source | Action |
|------|--------|--------|
| `SPC f f` | [DOOM] | Find file |
| `SPC f r` | [DOOM] | Find recent files |
| `SPC f s` | [DOOM] | Save file |
| `SPC f S` | [DOOM] | Save all files |
| `SPC f u` | [DOOM] | Sudo edit file |
| `SPC f y` | [DOOM] | Copy file path |
| `SPC f D` | [DOOM] | Delete file |
| `SPC f R` | [DOOM] | Rename/move file |
| `SPC f w` | [config.el] | Open workflow guide |

### Buffer Management

| Keys | Source | Action |
|------|--------|--------|
| `SPC b b` | [DOOM] | Switch buffer |
| `SPC b B` | [DOOM] | Switch buffer (all) |
| `SPC b d` | [DOOM] | Kill buffer |
| `SPC b k` | [DOOM] | Kill buffer |
| `SPC b n` | [DOOM] | Next buffer |
| `SPC b p` | [DOOM] | Previous buffer |
| `SPC b s` | [DOOM] | Save buffer |
| `SPC b z` | [DOOM] | Bury buffer |
| `SPC TAB` | [DOOM] | Switch to last buffer |
| `SPC ,` | [DOOM] | Switch buffer (workspace) |

### Window Navigation

| Keys | Source | Action |
|------|--------|--------|
| `SPC w h` | [DOOM] | Move to left window |
| `SPC w j` | [DOOM] | Move to window below |
| `SPC w k` | [DOOM] | Move to window above |
| `SPC w l` | [DOOM] | Move to right window |
| `SPC w w` | [DOOM] | Cycle windows |
| `SPC w v` | [DOOM] | Split window vertically |
| `SPC w s` | [DOOM] | Split window horizontally |
| `SPC w d` | [DOOM] | Delete window |
| `SPC w m` | [DOOM] | Maximize window |
| `SPC w o` | [DOOM] | Close other windows |
| `SPC w =` | [DOOM] | Balance windows |
| `C-x o` | [EMACS] | Other window |
| `C-x 0` | [EMACS] | Delete window |
| `C-x 1` | [EMACS] | Delete other windows |
| `C-x 2` | [EMACS] | Split below |
| `C-x 3` | [EMACS] | Split right |

### Search & Navigation

| Keys | Source | Action |
|------|--------|--------|
| `SPC s s` | [DOOM] | Search in current buffer |
| `SPC s S` | [DOOM] | Search in buffer (thing at point) |
| `SPC s p` | [DOOM] | Search in project |
| `SPC s P` | [DOOM] | Search in project (thing at point) |
| `SPC s d` | [DOOM] | Search in directory |
| `SPC s b` | [DOOM] | Search in open buffers |
| `SPC s i` | [DOOM] | Jump to symbol |
| `SPC s j` | [DOOM] | Jump list |
| `SPC s l` | [DOOM] | Search in buffer (line) |
| `SPC s r` | [DOOM] | Search and replace |
| `/` | [EVIL] | Search forward |
| `?` | [EVIL] | Search backward |
| `n` | [EVIL] | Next search result |
| `N` | [EVIL] | Previous search result |
| `*` | [EVIL] | Search word under cursor forward |
| `#` | [EVIL] | Search word under cursor backward |

### Code Navigation & LSP

| Keys | Source | Action |
|------|--------|--------|
| `gd` | [DOOM] | Go to definition |
| `gD` | [DOOM] | Go to references |
| `gi` | [DOOM] | Go to implementation |
| `gt` | [DOOM] | Go to type definition |
| `K` | [DOOM] | Show documentation/hover |
| `SPC c a` | [DOOM] | Code actions |
| `SPC c d` | [DOOM] | Jump to definition |
| `SPC c D` | [DOOM] | Jump to references |
| `SPC c f` | [DOOM] | Format buffer |
| `SPC c r` | [DOOM] | Rename symbol |
| `SPC c x` | [DOOM] | List errors |
| `SPC c c` | [DOOM] | Compile |
| `SPC c C` | [DOOM] | Recompile |
| `SPC c k` | [DOOM] | Kill compilation |
| `] d` | [DOOM] | Next diagnostic |
| `[ d` | [DOOM] | Previous diagnostic |

### Completion & Minibuffer (Vertico)

| Keys | Source | Action |
|------|--------|--------|
| `C-n` | [VERTICO] | Next candidate |
| `C-p` | [VERTICO] | Previous candidate |
| `M-RET` | [VERTICO] | Exit with input |
| `C-SPC` | [VERTICO] | Preview candidate |
| `TAB` | [VERTICO] | Complete |
| `RET` | [VERTICO] | Submit |
| `C-g` | [EMACS] | Abort |

### Embark Actions (Default)

| Keys | Source | Action |
|------|--------|--------|
| `SPC a` | [DOOM] | Embark act |
| `C-.` | [DOOM] | Embark act (in minibuffer) |
| `C-;` | [DOOM] | Embark dwim |

### Project Management

| Keys | Source | Action |
|------|--------|--------|
| `SPC p p` | [DOOM] | Switch project |
| `SPC p f` | [DOOM] | Find file in project |
| `SPC p F` | [DOOM] | Find file in project (all) |
| `SPC p b` | [DOOM] | Switch buffer in project |
| `SPC p d` | [DOOM] | Find directory in project |
| `SPC p i` | [DOOM] | Invalidate project cache |
| `SPC p k` | [DOOM] | Kill project buffers |
| `SPC p r` | [DOOM] | Find recent file in project |
| `SPC p s` | [DOOM] | Search in project |
| `SPC p t` | [DOOM] | Open project terminal |

### Git Integration (Magit)

| Keys | Source | Action |
|------|--------|--------|
| `SPC g g` | [DOOM] | Magit status |
| `SPC g G` | [DOOM] | Magit status (here) |
| `SPC g b` | [DOOM] | Magit blame |
| `SPC g B` | [DOOM] | Magit branch |
| `SPC g c` | [DOOM] | Magit commit |
| `SPC g C` | [DOOM] | Magit clone |
| `SPC g d` | [DOOM] | Magit diff |
| `SPC g f` | [DOOM] | Magit fetch |
| `SPC g l` | [DOOM] | Magit log |
| `SPC g L` | [DOOM] | Magit log (buffer) |
| `SPC g r` | [DOOM] | Magit rebase |
| `SPC g s` | [DOOM] | Magit stage file |
| `SPC g S` | [DOOM] | Magit stage all |
| `SPC g t` | [DOOM] | Git time machine |
| `SPC g u` | [DOOM] | Magit unstage file |
| `] h` | [DOOM] | Next hunk |
| `[ h` | [DOOM] | Previous hunk |

### Org Mode

| Keys | Source | Action |
|------|--------|--------|
| `SPC m` | [DOOM] | Local leader (org commands) |
| `SPC n a` | [DOOM] | Open agenda |
| `SPC n n` | [DOOM] | Open notes |
| `SPC n c` | [DOOM] | Capture |
| `SPC n f` | [DOOM] | Find org file |
| `SPC n j` | [DOOM] | Goto journal |
| `SPC n r` | [DOOM] | Roam |
| `SPC X` | [DOOM] | Org capture |
| `SPC o A` | [DOOM] | Org agenda |

### Org Mode (In Buffer)

| Keys | Source | Action |
|------|--------|--------|
| `TAB` | [ORG] | Cycle visibility |
| `S-TAB` | [ORG] | Cycle global visibility |
| `M-RET` | [ORG] | Insert heading |
| `M-S-RET` | [ORG] | Insert TODO heading |
| `M-left` | [ORG] | Promote heading |
| `M-right` | [ORG] | Demote heading |
| `M-up` | [ORG] | Move subtree up |
| `M-down` | [ORG] | Move subtree down |
| `C-c C-t` | [ORG] | Toggle TODO state |
| `C-c C-s` | [ORG] | Schedule |
| `C-c C-d` | [ORG] | Set deadline |
| `C-c C-c` | [ORG] | Set tags |
| `C-c '` | [ORG] | Edit code block |
| `SPC m e` | [DOOM] | Export dispatch |
| `SPC m t` | [DOOM] | Toggle TODO |
| `SPC m T` | [DOOM] | Show TODO tree |
| `SPC m i` | [DOOM] | Insert (heading, item, etc.) |
| `SPC m l` | [DOOM] | Insert link |
| `SPC m L` | [DOOM] | Store link |

### Terminal (Vterm)

| Keys | Source | Action |
|------|--------|--------|
| `SPC o t` | [DOOM] | Toggle vterm popup |
| `SPC o T` | [DOOM] | Open vterm |
| `C-c C-t` | [DOOM] | Toggle vterm copy mode |
| `Escape` | [EVIL] | Enter normal mode (navigate/yank) |
| `i` | [EVIL] | Enter insert mode (type in terminal) |

### Help & Documentation

| Keys | Source | Action |
|------|--------|--------|
| `SPC h f` | [DOOM] | Describe function |
| `SPC h v` | [DOOM] | Describe variable |
| `SPC h k` | [DOOM] | Describe keybinding |
| `SPC h m` | [DOOM] | Describe mode |
| `SPC h p` | [DOOM] | Doom packages |
| `SPC h d h` | [DOOM] | Doom help |
| `SPC h d m` | [DOOM] | Doom modules |
| `SPC h r r` | [DOOM] | Reload Doom |
| `SPC h t` | [DOOM] | Load theme |
| `K` | [EVIL] | Show documentation |

### Text Editing (Evil/Vim)

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

### Motion & Navigation (Evil/Vim)

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
| `{` | [EVIL] | Previous paragraph |
| `}` | [EVIL] | Next paragraph |
| `C-u` | [EVIL] | Scroll up half page |
| `C-d` | [EVIL] | Scroll down half page |
| `C-b` | [EVIL] | Scroll up full page |
| `C-f` | [EVIL] | Scroll down full page |
| `zz` | [EVIL] | Center cursor line |

### Operators (Evil/Vim)

| Keys | Source | Action |
|------|--------|--------|
| `d{motion}` | [EVIL] | Delete |
| `c{motion}` | [EVIL] | Change (delete and insert) |
| `y{motion}` | [EVIL] | Yank (copy) |
| `>{motion}` | [EVIL] | Indent right |
| `<{motion}` | [EVIL] | Indent left |
| `={motion}` | [EVIL] | Auto-indent |
| `gq{motion}` | [EVIL] | Format/wrap text |
| `p` | [EVIL] | Paste after |
| `P` | [EVIL] | Paste before |
| `u` | [EVIL] | Undo |
| `C-r` | [EVIL] | Redo |
| `.` | [EVIL] | Repeat last command |

### Text Objects (Evil/Vim)

| Keys | Source | Action |
|------|--------|--------|
| `iw` | [EVIL] | Inner word |
| `aw` | [EVIL] | A word (includes space) |
| `ip` | [EVIL] | Inner paragraph |
| `ap` | [EVIL] | A paragraph |
| `is` | [EVIL] | Inner sentence |
| `as` | [EVIL] | A sentence |
| `i"` / `a"` | [EVIL] | Inside/around double quotes |
| `i'` / `a'` | [EVIL] | Inside/around single quotes |
| `i(` / `a(` | [EVIL] | Inside/around parentheses |
| `i[` / `a[` | [EVIL] | Inside/around brackets |
| `i{` / `a{` | [EVIL] | Inside/around braces |
| `it` / `at` | [EVIL] | Inside/around tags |

---

## Workflow Tips

### Vterm Copy Mode

In vterm, you can navigate and copy text:
1. Press `Escape` to enter normal mode (Evil)
2. Use vim motions (`h j k l`, `w`, `b`, etc.) to navigate
3. Press `v` to start visual selection
4. Press `y` to yank (copy)
5. Press `i` to return to insert mode (terminal input)

Alternative: `C-c C-t` toggles vterm copy mode directly.

### Text Scaling (Zoom)

| Keys | Source | Action |
|------|--------|--------|
| `C-x C-=` | [EMACS] | Increase text scale |
| `C-x C--` | [EMACS] | Decrease text scale |
| `C-x C-0` | [EMACS] | Reset text scale |
| `SPC t b` | [DOOM] | Toggle big mode |

### Workspaces

| Keys | Source | Action |
|------|--------|--------|
| `SPC TAB n` | [DOOM] | New workspace |
| `SPC TAB d` | [DOOM] | Delete workspace |
| `SPC TAB r` | [DOOM] | Rename workspace |
| `SPC TAB 1-9` | [DOOM] | Switch to workspace N |
| `SPC TAB [` | [DOOM] | Previous workspace |
| `SPC TAB ]` | [DOOM] | Next workspace |

### Toggles

| Keys | Source | Action |
|------|--------|--------|
| `SPC t l` | [DOOM] | Toggle line numbers |
| `SPC t L` | [DOOM] | Toggle relative line numbers |
| `SPC t w` | [DOOM] | Toggle word wrap |
| `SPC t i` | [DOOM] | Toggle indent guides |
| `SPC t f` | [DOOM] | Toggle fullscreen |
| `SPC t t` | [DOOM] | Toggle treemacs |
| `SPC t z` | [DOOM] | Toggle zen mode |

---

## Your Custom Configuration

### Enabled Custom Bindings

| Keys | Action |
|------|--------|
| `SPC f w` | Open this workflow guide |

### Org Capture Templates

| Key | Template |
|-----|----------|
| `t` | Todo |
| `e` | Event |
| `d` | Deadline |
| `p` | Project |
| `i` | Idea |
| `b` | Bookmark |
| `n` | Note |

### Auto-Clocking

- Tasks auto-clock-in when state changes to `STRT`
- Tasks auto-clock-out when leaving `STRT` state

---

## Configuration Files

- **Main Config:** `~/.config/doom/config.el`
- **Init File:** `~/.config/doom/init.el`
- **Packages:** `~/.config/doom/packages.el`
- **Workflow Guide:** `~/.config/doom/WORKFLOW-GUIDE.md`

---

## Getting Help

| Keys | Action |
|------|--------|
| `SPC h d h` | Doom help documentation |
| `SPC h d m` | Doom module documentation |
| `SPC h k` | Describe what a key does |
| `SPC h f` | Search for function |
| `SPC h v` | Search for variable |
| `M-x` | Command palette |
| `SPC :` | Execute command |

---

*Updated: 2026-01-16 — Using default Doom Emacs keybindings*
