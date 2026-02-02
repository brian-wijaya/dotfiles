;;; install-devdocs.el --- Batch install devdocs documentation -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Install a curated set of 97 devdocs packages (~1.2 GiB).
;;
;; Usage:
;;   M-x load-file RET ~/.emacs.d/install-devdocs.el RET
;;   M-x my-install-all-devdocs
;;
;; Categories:
;;   - Web Platform: html, css, javascript, dom, svg, http
;;   - Systems Languages: c, cpp, rust, zig, go
;;   - Shells: bash, zsh, nushell, jq, man
;;   - Specialty Languages: ocaml, nim, r, gnu_cobol, elisp, php, phoenix
;;   - JS Ecosystem: node, npm, bun, typescript, react, angular, svelte, etc.
;;   - Python Ecosystem: flask, fastapi, matplotlib, tensorflow, scikit_learn, etc.
;;   - Data Systems: sqlite, mariadb, redis, duckdb
;;   - Infrastructure: docker, kubernetes, terraform, ansible, nginx
;;   - And more...
;;
;; To regenerate or modify this list, see the conversation that created it
;; or edit my-devdocs-to-install below.

;;; Code:

(require 'devdocs)

(defvar my-devdocs-to-install
  '(;; Web Platform
    "html" "css" "javascript" "dom" "svg" "http" "browser_support_tables"
    ;; Systems Languages
    "c" "cpp" "rust" "zig" "go"
    ;; Shells & CLI
    "bash" "zsh" "nushell" "jq" "man"
    ;; Specialty Languages
    "ocaml" "nim" "r" "gnu_cobol" "elisp" "php" "phoenix"
    ;; JS Runtimes & Toolchain
    "node" "npm" "bun" "yarn" "typescript" "esbuild" "vite" "prettier" "eslint"
    ;; JS Frameworks & Libraries
    "react" "redux" "react_router" "vueuse" "angular" "svelte" "nextjs" "astro"
    "htmx" "jquery" "express" "axios" "ramda" "date_fns" "es_toolkit" "threejs"
    ;; CSS & Styling
    "sass" "tailwindcss"
    ;; Python Ecosystem
    "flask" "werkzeug" "fastapi" "django_rest_framework" "requests" "click"
    "matplotlib" "scikit_learn" "scikit_image" "statsmodels"
    "tensorflow" "tensorflow_cpp"
    ;; Data Systems
    "sqlite" "mariadb" "redis" "duckdb"
    ;; Infrastructure & DevOps
    "docker" "kubernetes" "kubectl" "terraform" "ansible"
    "nginx" "nginx_lua_module" "apache_http_server"
    ;; Build Systems
    "cmake" "gnu_make"
    ;; Testing
    "jest" "vitest" "cypress" "playwright"
    ;; Documentation & Visualization
    "latex" "markdown" "jsdoc" "graphviz" "gnuplot"
    ;; Package Managers
    "homebrew" "nix"
    ;; Desktop & Graphics
    "electron" "vulkan" "i3" "react_native"
    ;; Math & Scientific
    "eigen3"
    ;; CMS
    "wordpress"
    ;; Utilities
    "git" "spring_boot")
  "Curated list of devdocs to install.")

(defun my-install-all-devdocs ()
  "Install all devdocs in `my-devdocs-to-install'."
  (interactive)
  (let ((total (length my-devdocs-to-install))
        (count 0)
        (failed nil))
    (dolist (doc my-devdocs-to-install)
      (setq count (1+ count))
      (message "[%d/%d] Installing %s..." count total doc)
      (condition-case err
          (devdocs-install doc)
        (error
         (push doc failed)
         (message "Failed to install %s: %s" doc (error-message-string err)))))
    (if failed
        (message "Done! Installed %d/%d. Failed: %s"
                 (- total (length failed)) total failed)
      (message "Done! Installed all %d devdocs." total))))

(defun my-devdocs-install-one (doc)
  "Install a single DOC from `my-devdocs-to-install' with completion."
  (interactive
   (list (completing-read "Install devdoc: " my-devdocs-to-install nil t)))
  (devdocs-install doc))

(provide 'install-devdocs)
;;; install-devdocs.el ends here
