;;; latex.el --- AUCTeX, CDLaTeX, LazyTab, Calc integration -*- lexical-binding: t; -*-
;;
;; Layered fast math input system based on karthinks' approach:
;; Layer 0: AUCTeX (texmathp, preview, folding)
;; Layer 1: CDLaTeX (backtick symbols, quote modifiers, TAB expansion)
;; Layer 2: YASnippet auto-expansion (configured in yasnippet.el)
;; Layer 3: CDLaTeX-YASnippet TAB bridge
;; Layer 4: LazyTab (matrix/table entry via orgtbl-mode)
;; Layer 5: Calc → LaTeX conversion

;; =========================================================================
;; Layer 0: AUCTeX
;; =========================================================================

(use-package latex
  :ensure auctex
  :demand t
  :hook ((LaTeX-mode . prettify-symbols-mode)
         (LaTeX-mode . outline-minor-mode))
  :config
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-PDF-mode t))

;; Larger previews (1.25× default)
(use-package preview
  :ensure nil
  :after latex
  :hook ((LaTeX-mode . bw/preview-larger-previews))
  :config
  (defun bw/preview-larger-previews ()
    (setq preview-scale-function
          (lambda () (* 1.25 (funcall (preview-scale-from-face)))))))

;; =========================================================================
;; Layer 1: CDLaTeX
;; =========================================================================

(use-package cdlatex
  :demand t
  :hook ((LaTeX-mode . turn-on-cdlatex))
  :config
  ;; Custom matrix/table entries for LazyTab
  (add-to-list 'cdlatex-command-alist
               '("smat" "Insert smallmatrix env"
                 "\\left( \\begin{smallmatrix} ? \\end{smallmatrix} \\right)"
                 lazytab-position-cursor-and-edit nil nil t))
  (add-to-list 'cdlatex-command-alist
               '("bmat" "Insert bmatrix env"
                 "\\begin{bmatrix} ? \\end{bmatrix}"
                 lazytab-position-cursor-and-edit nil nil t))
  (add-to-list 'cdlatex-command-alist
               '("pmat" "Insert pmatrix env"
                 "\\begin{pmatrix} ? \\end{pmatrix}"
                 lazytab-position-cursor-and-edit nil nil t))
  (add-to-list 'cdlatex-command-alist
               '("tbl" "Insert table"
                 "\\begin{table}\n\\centering ? \\caption{}\n\\end{table}\n"
                 lazytab-position-cursor-and-edit nil t nil)))

;; =========================================================================
;; Layer 3: CDLaTeX-YASnippet TAB Bridge
;; =========================================================================
;;
;; When inside a YASnippet field, TAB tries CDLaTeX expansion first.
;; If CDLaTeX can't expand, falls through to yas-next-field.

(with-eval-after-load 'yasnippet
  (with-eval-after-load 'cdlatex

    (defun bw/cdlatex-in-yas-field ()
      "Let CDLaTeX expand inside YASnippet fields."
      (when-let* ((_ (overlayp yas--active-field-overlay))
                  (end (overlay-end yas--active-field-overlay)))
        (if (>= (point) end)
            ;; At field end: try CDLaTeX, fall through to yas-next-field if no match
            (let ((s (thing-at-point 'sexp)))
              (unless (and s (assoc (substring-no-properties s)
                                    cdlatex-command-alist-comb))
                (yas-next-field-or-maybe-expand)
                t))
          ;; Inside field: let CDLaTeX act, clamp to field boundary
          (let (cdlatex-tab-hook minp)
            (setq minp
                  (min (save-excursion (cdlatex-tab) (point))
                       (overlay-end yas--active-field-overlay)))
            (goto-char minp) t))))

    (defun bw/yas-next-field-or-cdlatex ()
      "TAB dispatch: try CDLaTeX first, then YASnippet field navigation."
      (interactive)
      (if (or (bound-and-true-p cdlatex-mode)
              (bound-and-true-p org-cdlatex-mode))
          (cdlatex-tab)
        (yas-next-field-or-maybe-expand)))

    (define-key yas-keymap (kbd "<tab>") #'bw/yas-next-field-or-cdlatex)
    (define-key yas-keymap (kbd "TAB") #'bw/yas-next-field-or-cdlatex)
    (add-hook 'cdlatex-tab-hook #'bw/cdlatex-in-yas-field)))

;; =========================================================================
;; Layer 4: LazyTab — matrix/table entry via orgtbl-mode
;; =========================================================================
;;
;; Workflow: bmat<TAB> → org-table appears → fill cells → C-c C-c → LaTeX output
;; Inlined from karthink/lazytab (tiny, stable package).

(with-eval-after-load 'cdlatex
  (require 'org-table)

  (defun lazytab-position-cursor-and-edit ()
    "Position cursor at ? mark and start org-table editing."
    (cdlatex-position-cursor)
    (lazytab-orgtbl-edit))

  (defun lazytab-orgtbl-edit ()
    "Activate orgtbl-mode for matrix/table entry in LaTeX buffers."
    (when (derived-mode-p 'LaTeX-mode 'latex-mode)
      (advice-add 'orgtbl-ctrl-c-ctrl-c :after #'lazytab-orgtbl-replace)
      (orgtbl-mode 1)
      (open-line 1)
      (insert "\n|")))

  (defun lazytab-orgtbl-replace (_)
    "Convert org-table to LaTeX and replace in buffer."
    (interactive "P")
    (unless (org-at-table-p) (user-error "Not at a table"))
    (let* ((table (org-table-to-lisp))
           (params '(:backend latex :raw t))
           (replacement-table
            (if (texmathp)
                (lazytab-orgtbl-to-amsmath table params)
              (orgtbl-to-latex table params))))
      (kill-region (org-table-begin) (org-table-end))
      (open-line 1)
      (push-mark)
      (insert replacement-table)
      (align-regexp (region-beginning) (region-end) "\\(\\s-*\\)& ")
      (orgtbl-mode -1)
      (advice-remove 'orgtbl-ctrl-c-ctrl-c #'lazytab-orgtbl-replace)))

  (defun lazytab-orgtbl-to-amsmath (table params)
    "Convert org TABLE to amsmath array format."
    (orgtbl-to-generic
     table
     (org-combine-plists
      '(:splice t :lstart "" :lend " \\\\" :sep " & " :hline nil :llend "")
      params)))

  (defun lazytab-cdlatex-or-orgtbl-next-field ()
    "In orgtbl-mode, advance to next cell instead of CDLaTeX expansion."
    (when (and (bound-and-true-p orgtbl-mode)
               (org-table-p)
               (looking-at "[[:space:]]*\\(?:|\\|$\\)")
               (let ((s (thing-at-point 'sexp)))
                 (not (and s (assoc s cdlatex-command-alist-comb)))))
      (call-interactively #'org-table-next-field)
      t))

  (defun lazytab-org-table-next-field-maybe ()
    "TAB in orgtbl: try CDLaTeX first, then org-table-next-field."
    (interactive)
    (if (bound-and-true-p cdlatex-mode)
        (cdlatex-tab)
      (org-table-next-field)))

  (add-hook 'cdlatex-tab-hook #'lazytab-cdlatex-or-orgtbl-next-field 90)
  (with-eval-after-load 'org-table
    (define-key orgtbl-mode-map (kbd "<tab>") #'lazytab-org-table-next-field-maybe)
    (define-key orgtbl-mode-map (kbd "TAB") #'lazytab-org-table-next-field-maybe)))

;; =========================================================================
;; Layer 5: Calc → LaTeX conversion
;; =========================================================================

(defun bw/latex-math-from-calc ()
  "Evaluate `calc' on region or current line, replace with LaTeX output.
Uses Calc's algebraic entry to convert expressions to LaTeX notation."
  (interactive)
  (cond
   ((region-active-p)
    (let* ((beg (region-beginning))
           (end (region-end))
           (string (buffer-substring-no-properties beg end)))
      (kill-region beg end)
      (insert (calc-eval `(,string calc-language latex
                                   calc-prefer-frac t
                                   calc-angle-mode rad)))))
   (t
    (let ((l (thing-at-point 'line)))
      (end-of-line 1) (kill-line 0)
      (insert (calc-eval `(,l calc-language latex
                              calc-prefer-frac t
                              calc-angle-mode rad)))))))

(provide 'bw-latex)
;;; latex.el ends here
