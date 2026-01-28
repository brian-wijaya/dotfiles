;;; evil-leader.el --- SPC leader keymap structure -*- lexical-binding: t; -*-
;;
;; Creates the leader key and all prefix sub-keymaps.
;; No bindings here — those go in bindings-doom-full.el or bindings-doom-basic.el.

;; Leader map (SPC in normal mode)
(define-prefix-command 'bw/leader-map)
(evil-set-leader 'normal (kbd "SPC"))
(evil-define-key 'normal 'global (kbd "<leader>") 'bw/leader-map)

;; Sub-keymaps for each prefix
(define-prefix-command 'bw/leader-a-map) ; applications
(define-prefix-command 'bw/leader-b-map) ; buffers
(define-prefix-command 'bw/leader-c-map) ; code
(define-prefix-command 'bw/leader-f-map) ; files
(define-prefix-command 'bw/leader-g-map) ; git
(define-prefix-command 'bw/leader-h-map) ; help
(define-prefix-command 'bw/leader-hr-map) ; help reload
(define-prefix-command 'bw/leader-i-map) ; insert
(define-prefix-command 'bw/leader-n-map) ; notes
(define-prefix-command 'bw/leader-o-map) ; open
(define-prefix-command 'bw/leader-oh-map) ; open refs
(define-prefix-command 'bw/leader-p-map) ; project
(define-prefix-command 'bw/leader-q-map) ; quit
(define-prefix-command 'bw/leader-s-map) ; search
(define-prefix-command 'bw/leader-t-map) ; toggle
(define-prefix-command 'bw/leader-w-map) ; windows

;; Bind sub-keymaps to leader
(define-key bw/leader-map (kbd "a") 'bw/leader-a-map)
(define-key bw/leader-map (kbd "b") 'bw/leader-b-map)
(define-key bw/leader-map (kbd "c") 'bw/leader-c-map)
(define-key bw/leader-map (kbd "f") 'bw/leader-f-map)
(define-key bw/leader-map (kbd "g") 'bw/leader-g-map)
(define-key bw/leader-map (kbd "h") 'bw/leader-h-map)
(define-key bw/leader-map (kbd "i") 'bw/leader-i-map)
(define-key bw/leader-map (kbd "n") 'bw/leader-n-map)
(define-key bw/leader-map (kbd "o") 'bw/leader-o-map)
(define-key bw/leader-map (kbd "p") 'bw/leader-p-map)
(define-key bw/leader-map (kbd "q") 'bw/leader-q-map)
(define-key bw/leader-map (kbd "s") 'bw/leader-s-map)
(define-key bw/leader-map (kbd "t") 'bw/leader-t-map)
(define-key bw/leader-map (kbd "w") 'bw/leader-w-map)

;; Nested prefixes
(define-key bw/leader-h-map (kbd "r") 'bw/leader-hr-map)
(define-key bw/leader-o-map (kbd "h") 'bw/leader-oh-map)
