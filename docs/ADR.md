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
