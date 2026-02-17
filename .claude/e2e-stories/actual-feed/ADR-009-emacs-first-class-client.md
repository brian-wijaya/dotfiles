# ADR-009: Emacs as First-Class Client via JSON-RPC

## Status
Accepted

## Context
The target user (senior engineers, researchers, terminal-centric professionals) may live in Emacs 8+ hours per day. For them, a GUI app is the secondary interface. The Emacs client must feel native: buffer-based rendering, keyboard-first, composable with org-mode.

Candidates considered:
- **Embedded web view (xwidget-webkit)**: Technically possible in Emacs 28+. But: violates Emacs-native principle, inconsistent rendering, not keyboard-first. Rejected.
- **eww (Emacs web browser)**: Limited HTML rendering. Not suitable for rich structured data. Rejected.
- **Thin JSON-RPC client with buffer rendering**: Pure Elisp. Same API as GUI clients. Text rendering with faces and text properties. Composable with org-mode. Selected.

## Decision
The Emacs client (`actual-feed.el`) is a JSON-RPC 2.0 client connecting to the local Actual Feed backend via Unix domain socket (Linux preferred) or HTTP localhost. It uses the same API as GUI clients — no Emacs-specific backend endpoints. Six buffer types: feed, item detail, search, tripwires, budget, pipeline.

Deep org-mode integration: capture templates, custom `[[actual-feed:id]]` link type, agenda TODOs from tripwire fires, tag inheritance. Media playback via mpv.el with IPC-based budget tracking. Evil-compatible keybindings. consult/embark integration. Minimum Emacs version: 27.1.

All operations work from keyboard only. No mouse required for any operation. No web view or embedded browser.

## Consequences
- **Positive**: Native Emacs experience. Composable with existing workflow (org-mode, Evil, consult/embark). Same API means feature parity with GUI clients. mpv integration provides media playback with budget tracking.
- **Negative**: No inline images or video in Emacs — media is delegated to external players. Thumbnails not visible (text descriptions only). Users without mpv get explicit error: reason code `MEDIA_PLAYER_MISSING`, message "mpv not found — install mpv for media playback and budget tracking", with install instructions per platform (per ADR-011: no silent degradation).

## Constraints Implemented
- EC-070: Emacs client uses the same JSON-RPC API as GUI clients
- EC-071: All operations work from keyboard only — no mouse required
- EC-072: No web view or embedded browser in Emacs
- OC-070: mpv is the recommended media player, not VLC or browser
- OC-071: Org-mode integration is deep, not superficial
- OC-072: Minimum Emacs version: 27.1
