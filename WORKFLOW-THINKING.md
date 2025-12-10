# Thinking About Your Workflow

A companion to WORKFLOW-GUIDE.md. This document is about *when* and *why*, not just *what*.

---

## Stage 1: Terminal & Tmux — The Ground You Stand On

The first mental shift is understanding that your terminal is no longer a program you open—it's the environment you inhabit. When you launch WezTerm, you're not "opening a terminal application"; you're entering a workspace that happens to persist whether you're looking at it or not.

Tmux is the layer that makes this possible. A session (`Prefix d` to detach, `tmux attach -t main` to return) survives disconnection. This matters less on a local machine than you might think, but the habit matters enormously: you stop treating terminal state as ephemeral. You leave things running. You walk away mid-task and come back. Your mental model shifts from "I'm running commands" to "I'm tending a living workspace."

The most underused tmux feature is probably `Prefix z`—zooming a pane to fullscreen and back. The pattern is: you have three panes open (maybe editor, server logs, and a shell for git), and you need to focus on one. Instead of closing the others or squinting, you zoom. When you're done, you unzoom, and the context is still there. This sounds trivial until you realize how often you've been unconsciously closing things and re-opening them because the screen felt cluttered.

Windows (`Prefix c` to create, `Prefix 1/2/3` to switch) are for context-switching between *projects* or *modes of work*. You might have window 1 for your main project, window 2 for documentation you're referencing, window 3 for a completely unrelated side task. Panes are for parallel views *within* a context. Don't confuse these. If you're constantly splitting panes for unrelated work, you want windows. If you're opening new windows to see two files side-by-side, you want panes.

Your custom `Prefix |` and `Prefix -` for splits are mnemonic: the pipe character is vertical (a vertical split creates side-by-side panes), the dash is horizontal. This is backwards from what you might expect if you think about the *divider line* rather than the *resulting pane arrangement*. Just remember: `|` makes panes that look like `| |`, and `-` makes panes that look like `—`.

A peculiar but useful habit: before starting any significant task, create a new window for it (`Prefix c`), name it (`Prefix ,`), and work there. When you're done, kill the window (`Prefix &`). This forces a clean start and prevents the "I have 47 panes and don't know what any of them are" problem.

---

## Stage 2: File Management — The Death of the Mouse Reflex

The hardest habit to break is reaching for a GUI file manager. NvimTree exists to intercept that reflex and redirect it somewhere more powerful.

When you press `Ctrl+n`, you're not "opening a sidebar"—you're entering a different *mode* of interaction with your project. In this mode, the atomic unit is the file, and your navigation keys (`j/k`, `Enter`, `h` to close/ascend) operate on that unit. The shift is: you don't *browse* to files, you *navigate* to them. Browsing is wandering. Navigation is directed movement.

The `a` key (create file) accepts a path, not just a name. If you type `components/Button/index.tsx`, it creates the intermediate directories. This is enormously powerful and underused. You don't need to create a folder, enter it, create another folder, enter it, then create a file. You declare the destination and it exists.

`H` (toggle hidden files) is worth pressing early in any new project to understand what's actually there. Dotfiles are often the most important files—configuration, git internals, environment settings. Keeping them hidden by default is a reasonable UI choice, but you should develop the habit of revealing them deliberately when you need to understand a project's full shape.

The copy-path operations (`y` for filename, `Y` for relative path, `gy` for absolute path) seem redundant until you need them. The common case: you're in a conversation (Slack, GitHub, an AI chat) and need to reference a file. You navigate to it, hit `gy`, and paste. No thinking about "where am I" or "what's the path from root." The tree knows. You just ask it.

Marks (`m` to toggle) are for batch operations, which are rarer than you'd expect. The main use case: you're reorganizing a project and need to move several files to a new location. Mark them, navigate to the destination, `bmv`. The more common pattern is single-file operations—and that's fine. Not everything needs to be a power move.

One subtle point: `v` and `s` (open in vertical/horizontal split) from NvimTree are about *initiating* a multi-file editing session. You're in the tree, you find a file, and instead of just opening it, you're saying "I want to see this *alongside* what I already have open." This is the moment where you transition from single-file focus to comparative work—looking at two implementations side by side, editing a test while viewing the source, etc.

---

## Stage 3: FZF & Shell Integration — Query Replaces Location

The conceptual shift here is from *hierarchical* to *flat* thinking. A filesystem is a tree: directories contain files contain text. But your *work* doesn't follow that structure. You want "that function I wrote last week" or "the config file with the port number" or "that command I ran to fix the build."

FZF collapses the tree into a searchable list. `Ctrl+r` (history search) is the gateway drug. Instead of up-arrowing through history or trying to remember exact syntax, you type fragments of what you remember and it surfaces matches. The habit to develop: when you catch yourself thinking "what was that command...", you should *immediately* hit `Ctrl+r` and start typing. Don't try to remember first. Let the tool remember.

`Ctrl+t` (insert file path) is for when you're composing a command and need a filename. You're typing `vim ` and then you pause—where's that file? Instead of finishing the command, opening a file manager, navigating, copying the path, coming back, and pasting... you hit `Ctrl+t`, type fragments, and the path inserts itself. The command never leaves your focus.

`Alt+c` (cd into directory) is the least intuitive but potentially most transformative. You're somewhere in your filesystem, and you need to be somewhere else. You don't `cd` into intermediate directories. You don't even need to know the full path. You hit `Alt+c`, type what you remember about the destination ("config" or "tests" or "backend"), and you're there. This is zoxide's job too (`z partial-name`), but `Alt+c` gives you the interactive picker when you're not sure what you're looking for.

The deeper pattern: *fuzziness is a feature, not a limitation*. You don't need to remember exactly. You don't need to be precise. You throw fragments at the system and it figures out what you meant. This is uncomfortable at first—it feels sloppy—but it's actually a more honest interface with human memory. We don't remember exact strings. We remember shapes, contexts, fragments. FZF meets us where we are.

---

## Stage 4: Buffers, Windows & Tabs — The Geometry of Attention

A buffer is a file loaded into memory. A window is a viewport onto a buffer. A tab is a collection of windows. These three concepts are the source of endless confusion, but the confusion dissolves when you think about *attention* rather than *files*.

You have ten files open (buffers), but you're looking at one (window), or maybe two side-by-side (windows), arranged in a particular layout (which could be saved as a tab). The buffers are *available*. The windows are *visible*. The tab is a *workspace configuration*.

`Tab` and `Shift+Tab` cycle through buffers. This is your most frequent navigation—much more frequent than the file tree. The tree is for *finding* files. Buffer cycling is for *returning* to files you've already found. The habit: open files liberally, close them sparingly. Trust that buffer cycling is fast. You don't need to "clean up" by closing files you're done with. They cost almost nothing to keep open, and you might need them again.

`Leader x` closes a buffer. Use it when you're *sure* you're done with a file—when it was a wrong turn, or when you've finished a task and want to reduce visual noise in the tabufline. But err on the side of leaving things open.

Splits (`Ctrl+w v` for vertical, `Ctrl+w s` for horizontal) are for *comparative* work. The question to ask yourself: "Do I need to see two things at the same time?" If yes, split. If you're just going back and forth, buffer cycling is better. Splits cost screen real estate. Don't split reflexively.

`Ctrl+h/j/k/l` for window navigation uses the same directional logic as everything else—vim movements, tmux panes, everything. This is the deep consistency that makes the whole system feel unified. Left is always `h`. Down is always `j`. Once this is in your fingers, you stop thinking about "tmux navigation" vs "window navigation" vs "text navigation." It's all just *directional*.

`Ctrl+w =` (equalize window sizes) is the reset button. You've been resizing panes, the layout is weird, you want to start fresh. One keystroke. Similarly, `Ctrl+w o` (close all other windows) is for when you've been in a multi-window workflow and want to return to focus. These are *mode transitions*—from fragmented attention to unified attention.

---

## Stage 5: Telescope — The Universal Query Interface

Telescope is the conceptual successor to FZF, but elevated into the editor itself. Where FZF operates on your shell (history, files, directories), Telescope operates on *everything Neovim knows about*: files, buffers, text content, LSP symbols, git state, help documentation, keymaps themselves.

`Leader f f` (find files) is the flagship. You stop thinking about where files are and start thinking about what they're called. Partial matches work. You type `btn` and it shows you `Button.tsx`, `button.css`, `submitBtn.js`. The ranking is smart—it prioritizes recent files, exact matches, path proximity. Trust it.

`Leader f w` (live grep) is for when you know *what's in* the file but not *which* file. "Where do we handle authentication?" You don't know the filename. You type `authenticat` and see every line in your project containing that substring, with previews. This replaces the entire pattern of "grep in terminal, look at results, open file, navigate to line." It's one fluid motion.

The inside-Telescope navigation (`Ctrl+n/p` to move between results, `Ctrl+v/x/t` to open in split/hsplit/tab) is worth learning thoroughly. You'll spend real time in that picker UI. It's not just "find and open"—it's "find, preview, decide how to open." The preview pane (right side) shows you the file *before* you commit to opening it. Use this. Scroll the preview (`Ctrl+u/d`). Make sure it's the right file before you press Enter.

`Leader f o` (old files / recent files) is for "I was just working on that, where is it?" The buffer list (`Leader f b`) only shows currently-open buffers. Old files shows everything you've touched recently, including things you've closed. This is your "undo" for closing files prematurely.

`Leader t h` (theme picker) seems frivolous but has a genuine use: visual fatigue. If you've been staring at code for hours and everything looks the same, switching themes forces your visual system to re-engage. It's a reset. Also, some themes are better for certain conditions (high ambient light, low light, colorblindness considerations). Having instant access to the picker means you can adapt rather than suffer.

The meta-move: `:Telescope keymaps`. This searches all your keybindings. When you forget something—when you know there's a mapping but can't remember it—this is your escape hatch. Type fragments of what you're looking for. It's the system documenting itself.

---

## Stage 6: LSP — The Code Understands Itself

Language Server Protocol is the infrastructure that makes your editor actually understand code rather than just displaying text. When configured (and you have `lspconfig` in your plugins), you get a second layer of intelligence underneath everything.

`gd` (go to definition) is the most important. Cursor on a function call? `gd` takes you to where it's defined. Cursor on a variable? `gd` takes you to its declaration. This is the end of "where is this thing defined?"—the question that previously required grepping, reading, inferring. You just ask.

`gr` (go to references) is the inverse. You're *at* a definition, and you want to know: where is this used? Every callsite, every import, every reference—listed and navigable. This is the end of "what will break if I change this?" You can see the blast radius.

`K` (hover documentation) is the lightweight version. You don't need to jump anywhere—you just want to know what this thing is. Function signature, type information, docstrings. Press `K`, read, move on. The habit: any time you're uncertain what something is or does, hit `K`. It costs nothing.

`Leader c a` (code actions) is context-sensitive magic. The LSP looks at your cursor position and offers relevant transformations: auto-import, extract variable, rename, fix linting error, organize imports. The suggestions vary by language and situation. The habit: when you see a diagnostic (error/warning), navigate to it and hit `Leader c a` to see if there's an automatic fix.

`Leader r a` (rename symbol) is project-wide refactoring. Not search-and-replace—*semantic* renaming. The LSP knows that `user` in line 5 and `user` in line 50 are the same variable, but `user` in line 100 is a different thing in a different scope. Rename catches all the right ones and none of the wrong ones.

The diagnostic navigation (`[d` and `]d` for previous/next) is how you work through errors. The compiler found five problems. You don't manually search for red squiggles. You hit `]d` to jump to the first, fix it, hit `]d` again for the second. Linear progression through issues. When `]d` stops moving, you're done.

A subtle point: LSP features are contextual. `gd` does nothing useful if your cursor is on whitespace. `Leader c a` only offers actions when there are actions to offer. The intelligence is *responsive*—it activates when relevant. You develop an intuition for "this is a moment for LSP" vs "this is just text editing."

---

## Stage 7: Modal Editing — The Slowest Stage That Enables Everything

Modal editing is the foundation that the entire system assumes. It's also the part that takes longest to internalize, which is why it's listed last in your priority order despite being technically first.

The core idea: you are always in a *mode*, and the same key does different things in different modes. In Normal mode, `j` moves down. In Insert mode, `j` types the letter "j." This seems like a complication, but it's actually a compression—it gives you an entire keyboard of commands without needing modifier keys.

Your custom `jk` (exit Insert mode) is a signal that you've already internalized the most important habit: *getting out of Insert mode quickly*. Insert mode is for typing text. The moment you're done typing, you should leave. `jk` is faster than reaching for Escape. The pattern: enter Insert mode (`i`, `a`, `o`, etc.), type what you need, hit `jk`, continue editing with Normal mode commands. Insert mode is a temporary visit, not a place to live.

Your custom `;` for command mode (instead of `:`) is about hand position. `:` requires Shift. `;` doesn't. Every saved keystroke compounds. You'll enter command mode hundreds of times a day.

The movement keys (`h/j/k/l`) are worth forcing yourself to use, even when they feel slower than arrow keys. They keep your hands on the home row. The payoff isn't immediate—it's in the *combinations*. `d3j` (delete three lines down) is only possible if `j` is a motion your fingers know. If you're reaching for arrows, you can't compose.

Motions combine with operators: `d` (delete), `c` (change), `y` (yank/copy). The grammar is: *operator + motion = action*. `dw` deletes a word. `cw` changes a word (deletes and enters Insert mode). `yw` yanks a word. Once you learn a new motion (`}` for next paragraph, `f)` for next closing paren), you automatically know how to delete/change/yank that motion. The vocabulary grows multiplicatively.

Text objects (`iw` for inner word, `i"` for inside quotes, `ip` for inner paragraph) are the advanced version. They're *regions* rather than *movements*. `ciw` changes the word the cursor is *in*—you don't need to be at the word's start. `ci"` changes everything inside the quotes, wherever you are between them. This is where modal editing becomes genuinely faster than any alternative: you express intent ("change this quoted string") directly, and the system figures out the boundaries.

The `.` command (repeat last change) is the hidden multiplier. You do something—`ciwhello<Esc>` to change a word to "hello"—and then you can repeat it anywhere with `.`. Navigate to another word, hit `.`, it's "hello" now too. This is why Vim users structure their edits as *repeatable changes* when possible.

The paradox: modal editing is both the most basic and most advanced skill. You need it to do anything, but mastering it takes years. Accept the long timeline. Use the basics now (`i`, `jk`, `dd`, `yy`, `p`, `/search`), and trust that fluency will come from accumulated hours. There's no shortcut.

---

## Stage 8: Utilities & Terminal — The Escape Hatches

Sometimes you need a shell without leaving Neovim. The built-in terminal (`Leader h` for horizontal, `Leader v` for vertical, `Alt+i` for floating) is for that. Run a quick command, see output, return to editing. This is *not* a replacement for tmux—it's a convenience for moments when context-switching to another pane is too disruptive.

The floating terminal (`Alt+i`) is particularly useful for one-off commands. It hovers over your editor, you do your thing, you dismiss it. Nothing changes in your window layout.

`Ctrl+x` to exit terminal mode is essential. When you're in the terminal, you're in Terminal-Insert mode—keys go to the shell. To get back to Normal mode (to navigate the terminal output, copy text, etc.), you need `Ctrl+x`. This is easy to forget and frustrating when forgotten.

`Leader c h` (cheatsheet) is the system documenting itself. When you forget, look it up. The cheatsheet shows you every NvChad mapping, organized by category. This is not a crutch—it's the intended interface for learning. Use it liberally.

`:Telescope keymaps` is the searchable version. You remember there's a mapping involving "git" but not the exact keys. Search for "git" in keymaps. The answer appears.

The line number toggles (`Leader n` for absolute, `Leader r n` for relative) are about context. Relative line numbers show you how far away things are: that function is `7j` away. This is useful for navigation and for `d7j`-style operations. Absolute numbers are useful when someone says "line 234." Switch based on what you're doing.

---

## The Meta-Pattern

Everything above is in service of a single idea: *removing friction between intention and action*.

You intend to open a file. The frictionless path is `Leader f f` + fuzzy search + Enter.
You intend to rename a variable everywhere. The frictionless path is cursor on variable + `Leader r a` + new name + Enter.
You intend to see two files side by side. The frictionless path is open first file + `Ctrl+w v` + `Leader f f` + second file.

The system becomes invisible when you internalize it. You stop thinking "I need to press Leader f f to open Telescope to find the file." You just *think of the file* and your fingers do the thing. This is the goal: the interface disappears.

It takes time. The keybindings feel arbitrary at first, then mnemonic, then automatic. The stages above are ordered by impact—you can do real work with just Stage 1-3 mastered, and stages 4-7 compound on that foundation. Don't try to learn everything at once. Get comfortable, then expand.

The final habit: when something feels slow, notice it. When you're reaching for the mouse, notice it. When you're manually doing something repetitive, notice it. Each moment of friction is a signal that there might be a faster path. Check the cheatsheet (`Leader c h`), search keymaps (`:Telescope keymaps`), or ask. The tool probably does what you need—you just don't know the key yet.

---

*You are not learning an editor. You are building a second set of hands.*
