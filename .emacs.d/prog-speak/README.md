# Prog-Speak

Deterministic English surface-form normalization for reduced reading overhead.

Removes articles (a, an, the) and de-inflects third-person singular verbs using the NIH SPECIALIST Lexicon.

## Usage

```elisp
;; Load
(load-file "~/vault/programs/prog-speak/prog-speak.el")

;; Toggle overlay mode (non-destructive)
M-x prog-speak-toggle

;; Rewrite buffer (destructive, requires confirmation)
M-x prog-speak-rewrite-buffer

;; Rewrite region (destructive)
M-x prog-speak-rewrite-region
```

## Example

Before:
> The function returns the values and processes the data.

After:
> function return value and process data.

## Files

| File | Purpose |
|------|---------|
| prog-speak.el | Main elisp package |
| prog-speak-lemmas.el | Verb lemma dictionary (20k+ mappings) |
| generate-lemmas.py | Script to regenerate lemmas from SPECIALIST |

## Setup

No system configuration needed. Just load in Emacs:

```elisp
;; Add to init.el
(load-file "~/vault/programs/prog-speak/prog-speak.el")
```

## How It Works

1. **Article removal**: Strips lowercase "a", "an", "the" surrounded by whitespace
2. **Verb de-inflection**: Converts third-person singular verbs to base form
   - Dictionary lookup (primary): 20k+ verb mappings from NIH SPECIALIST Lexicon
   - Heuristic fallback: Standard English morphology rules

## Scope

Only transforms text in:
- Org-mode prose (not code blocks, drawers)
- Programming mode comments/docstrings
- Plain text buffers
