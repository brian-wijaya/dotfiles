# Emacs Notes

Niche debugging notes extracted from main MEMORY.md. Referenced from there.

## Emacs 30 which-key: C subrs can't be advised OR fset-wrapped
- `which-key--show-page`, `which-key--show-popup`, and `which-key--update` are C subrs in the built-in Emacs 30 which-key
- `advice-add` silently appears to work but advice NEVER fires on C subrs
- `fset` changes the Lisp symbol-function, but the C-level idle timer calls `which-key--update` directly as a C function, bypassing the Lisp symbol entirely — so fset wrappers are never called by the timer
- `fset` wrappers DO work when called from Lisp (e.g., `(which-key--update)`) — this is deceptive during testing
- `which-key-posframe--show-buffer` is ALSO a C subr — advice-add :override appears to work but C code calling via `which-key-custom-show-popup-function` still bypasses the advice wrapper
- **Working solution**: set `which-key-custom-show-popup-function` to a NEW pure-Lisp symbol (`bw/which-key-show-wrapper`) that calls the override function directly — forces C code through Lisp dispatch
- The override renders sectioned layout with `:align-to` display properties, then calls `posframe-show`
- `(which-key-mode 1)` clears `which-key-replacement-alist` — use `(unless which-key-mode ...)` on reload
- See [emacs30-subr-advice.md](emacs30-subr-advice.md) for details

## x11_key timing & keyboard handoff
- When sending multi-key sequences to Emacs via x11_key, don't interleave with `emacs_elisp` calls (sit-for, etc.) as this can consume/interfere with key events
- Send keys sequentially with just x11_key, then screenshot after
- Keyboard handoff only needed when targeting user's display (:0). NOT needed for agent's own display (:99) — agent has exclusive control
- Prefer `emacs_elisp` over x11_key whenever possible — only use x11_key for testing `post-self-insert-hook`, Evil state transitions, TAB dispatch, CDLaTeX key sequences
- **NEVER use user's live Emacs** (localhost:8585) for testing — launch separate daemon on :99 (`DISPLAY=:99 emacs --daemon=claude-test`)

## YASnippet yas-key-syntaxes functions
- Functions in `yas-key-syntaxes` must MOVE POINT backward (e.g. `(backward-char 2)`) — YASnippet extracts key as `(buffer-substring-no-properties (point) original)`
- Return value only matters if it's `'again` (to retry same function); any other return is ignored
- Returning a number (like `(- 2)` or `(- (point) 2)`) does NOT move point — this was a multi-hour debugging trap
- Correct pattern: `(when (>= (point) (+ (point-min) N)) (backward-char N) nil)`
