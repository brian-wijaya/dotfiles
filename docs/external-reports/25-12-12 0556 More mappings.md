UNIFIED IMPLEMENTATION PROMPT
Objective

Establish a streamlined Neovim + tmux workflow that:

Eliminates repetitive typing

Centralizes workflow control inside Neovim

Adds safe, discoverable keybinding rehearsal via which-key

Preserves tmux as a stable outer layer

Uses short, ergonomic commands

Environment Assumptions

OS: WSL (Arch Linux)

User: bw

tmux prefix: C-Space (do not change)

Neovim is primary workspace

tmux plugin manager (TPM) is available or may be installed

Neovim plugin manager already exists (lazy.nvim, packer, etc. — choose what’s present)

Paths

Workflow guide file (authoritative reference):

/home/bw/dotfiles/WORKFLOW_GUIDE.md

PART 1 — Shell Setup
1. Claude alias

Add the following alias to the user’s shell config (~/.bashrc or ~/.zshrc):

alias cld='claude --dangerously-skip-permissions'


Reload shell config after change.

PART 2 — Neovim Core Commands & Mappings
2. Claude launcher inside Neovim

Create a Neovim user command that launches Claude inside a terminal buffer:

command! Claude terminal cld


Bind it in Normal mode:

nnoremap <leader>ac :Claude<CR>

3. Workflow guide quick access

Bind a Normal-mode mapping to open the workflow guide:

nnoremap <leader>gw :edit /home/bw/dotfiles/WORKFLOW_GUIDE.md<CR>


This must always open the same file (no cwd dependence).

4. Exit discipline (no prompts)

Ensure the following behavior is preserved and documented:

:qa! exits Neovim completely with no confirmation

Do not add quit confirmations or wrappers

Terminal escape remains default:

<C-\><C-n>

PART 3 — Neovim which-key (FULL PANEL MODE)
5. Install folke/which-key.nvim

Install and enable which-key.nvim.

6. which-key configuration requirements

Configure which-key so that:

It opens in a large panel

Width and height are maximized (or near-max)

No paging or scrolling is required

User can see all mappings for a prefix at once

Avoid <C-d> / <C-u> navigation

Intent:

This is for rehearsal and overview, not drilling down.

Implementation guidance:

Increase window.width and window.height

Prefer vertical layout or full-width

Disable automatic collapsing

Do not hide mappings by default

PART 4 — tmux which-key
7. Install tmux-which-key

Install the plugin:

alexwforsythe/tmux-which-key


Use TPM if available; install TPM if missing.

8. tmux binding descriptions (required)

Ensure tmux keybindings include human-readable descriptions so tmux-which-key can display meaningful labels.

Pattern to follow when defining tmux bindings:

bind w split-window -h \; display-message "Split window horizontally"


Descriptions must reflect actual behavior.

9. tmux-which-key activation

Bind tmux-which-key so that after pressing the prefix (C-Space):

A popup/menu appears

Shows the user’s actual tmux bindings

Prevents accidental execution

Closes only after a selection or Escape

Do not change the prefix key.

PART 5 — tmux Convenience Bootstrap
10. Open workflow guide in new tmux pane

Add a tmux binding that opens a new horizontal pane running Neovim with the workflow guide:

bind w split-window -h "nvim /home/bw/dotfiles/WORKFLOW_GUIDE.md"


This is a convenience shortcut, not the primary access method.

PART 6 — Validation Checklist

Confirm the following behaviors:

cld runs Claude with required flags

<leader>ac launches Claude inside Neovim terminal

<leader>gw opens the workflow guide instantly

Neovim which-key opens in a large, non-scroll panel

tmux-which-key shows user-defined bindings, not defaults

tmux prefix remains C-Space

:qa! exits cleanly with no prompts

Escape cleanly exits which-key popups in both tools

Constraints

Do not invent new workflows

Do not refactor unrelated configs

Do not change tmux prefix or Neovim leader

Prefer explicit mappings over clever automation

Visibility > minimalism