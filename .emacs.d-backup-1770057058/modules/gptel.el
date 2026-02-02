;;; gptel.el --- LLM client -*- lexical-binding: t; -*-
(require 'gptel)
(load (expand-file-name "secrets.el" user-emacs-directory) t t)

(setq gptel-model 'claude-sonnet-4-20250514
      gptel-backend (gptel-make-anthropic "Claude"
                      :stream t
                      :key bw/anthropic-api-key))
