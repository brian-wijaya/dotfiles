# Architecture Decision Records

## ADR-001: Emacs Profile System

**Date**: 2026-01-23

**Status**: Accepted

**Context**:
Following tutorials for vanilla Emacs or Doom Emacs is difficult when your config has custom keybindings. When a tutorial says "press C-x C-f" or "press SPC f f", conflicts arise.

**Decision**:
Implement a hot-swappable profile system with three profiles:
1. `default-vanilla` - Stock Emacs (no evil, no custom bindings)
2. `default-doom` - Doom Emacs defaults (evil + SPC leader)
3. `bw-vanilla` - Custom setup (current config)

Each profile controls: theme, keybindings, evil mode, UI elements.

**Packages for default-doom**:
Sourced from Doom's actual `packages.el` files on GitHub to ensure perfect fidelity.
Organized by Doom module categories (`:editor evil`, `:ui doom`, `:completion vertico`, etc.)

**Consequences**:
- Can follow any vanilla or Doom tutorial without conflicts
- `M-x switch-profile` changes entire environment
- Profile switch requires restart for package changes
- Theme/keybindings hot-swap within session

**Alternatives Considered**:
1. Separate Emacs installations - Too heavy, hard to maintain
2. Chemacs2 - Profile manager, but adds complexity
3. Conditional loading only - Chose this approach, single init.el

---

## ADR-002: No Symlinks for Dotfiles

**Date**: 2026-01-23

**Status**: Enforced (Learned from catastrophic failure)

**Context**:
Symlink-based dotfile management (stow, chezmoi) caused catastrophic data loss when something went wrong. Emacs packages, configs, hours of work - all destroyed.

**Decision**:
Live environment (`~/`) is source of truth. Manual `cp` to `~/dotfiles/` for version control. No symlinks, no sync scripts, no automation.

**Consequences**:
- Slightly more manual work
- No risk of symlink-related data loss
- Clear mental model: edit in place, copy when ready

---

## ADR-003: Discoverable Interface - No Hidden Menus

**Date**: 2026-01-24

**Status**: Accepted

**Context**:
Emacs has deeply nested keybinding trees. Many useful features are hidden behind prefix keys that aren't visible anywhere on screen. Example: in vterm, `[` opens a prompt navigation menu - but nothing indicates this. Users must memorize or stumble upon these features.

**Philosophy**:
We do not concern ourselves with crowding the interface. At any given time, the keyboard inputs needed to use any aspect of Emacs should not require prior memorization and must be explicitly visible as on-screen hints.

**Decision**:
At any screen where a unique which-key panel or minibuffer command is one key press away, a hint for it should be visible somewhere (e.g., centered in the modeline/status bar).

For most screens, this means displaying:
- `SPC` → Leader menu
- `C-c` → Mode commands
- `M-x` → All commands
- Mode-specific prefixes (e.g., `[`/`]` in vterm for prompt navigation)

**Implementation Requirements**:
1. Modeline segment showing available prefix keys for current major mode
2. Dynamic updates when mode changes
3. Brief descriptive labels (e.g., "SPC:leader C-c:mode M-x:cmd")
4. Mode-specific hints (vterm shows `[]:prompts`, magit shows its prefixes, etc.)

**Consequences**:
- Slightly busier modeline
- Zero-memorization onboarding for any Emacs mode
- Features are used because they're visible, not forgotten because they're hidden
- Consistent discoverability across all modes

**Alternatives Considered**:
1. Rely on which-key alone - Still requires knowing the first key to press
2. Documentation/cheatsheets - External to the interface, gets stale
3. Hydras everywhere - Too intrusive, blocks workflow

**Amendment (2026-02-05): State-Aware Hints**

The keyhint strip now adapts to evil state. In insert mode (vterm), it shows
`ESC, SPC:👑leader` instead of `SPC:👑leader`, telling the user exactly how
to reach the leader from their current context. Three bugs were fixed:

1. Buffer context inversion — `bw/format-keyhints` was called inside
   `with-current-buffer *keyhints*`, reading `evil-state` from the wrong buffer
2. Leader detection via wrong keymap layer — `lookup-key evil-normal-state-map`
   misses bindings in evil's intercept maps; switched to checking `bw/leader-map` directly
3. Missing `evil-emacs-state-entry-hook` — strip didn't update on C-z toggle

See `vault/org/notes/evil-states-and-keyhints.org` for full technical writeup.
