;;; kotlin-ts-mode.el --- tree-sitter based Kotlin mode -*- lexical-binding: t; -*-

(require 'treesit)

(defvar kotlin-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'kotlin
   :feature 'keyword
   '((["fun" "val" "var" "class" "object" "interface" "package" "import" 
       "if" "else" "when" "for" "while" "return" "try" "catch" "finally"
       "public" "private" "protected" "internal" "abstract" "open" "final"
       "override" "companion" "const" "suspend" "inline"] @font-lock-keyword-face))
   
   :language 'kotlin
   :feature 'string
   '((string_literal) @font-lock-string-face)
   
   :language 'kotlin
   :feature 'comment
   '((line_comment) @font-lock-comment-face
     (multiline_comment) @font-lock-comment-face)
   
   :language 'kotlin
   :feature 'function
   '((simple_identifier) @font-lock-function-name-face
     (:match "^[a-z_][a-zA-Z0-9_]*$" @font-lock-function-name-face))
   
   :language 'kotlin
   :feature 'type
   '((type_identifier) @font-lock-type-face))
  "Font-lock settings for Kotlin.")

;;;###autoload
(define-derived-mode kotlin-ts-mode prog-mode "Kotlin"
  "Major mode for editing Kotlin files, powered by tree-sitter."
  :syntax-table nil
  
  (when (treesit-ready-p 'kotlin)
    (treesit-parser-create 'kotlin)
    
    (setq-local treesit-font-lock-settings kotlin-ts-mode--font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '((comment)
                  (keyword string)
                  (type function)
                  (variable operator)))
    
    (treesit-major-mode-setup)))

(provide 'kotlin-ts-mode)
;;; kotlin-ts-mode.el ends here
