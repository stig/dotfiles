(ido-mode t)

;; Auto refresh buffers
(global-auto-revert-mode 1)

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

;; Transparently open compressed files
(auto-compression-mode t)

;; Enable syntax highlighting for older Emacsen that have it off
(global-font-lock-mode t)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top

;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

;; Undo/redo window configuration with C-c <left>/<right>
(winner-mode 1)

;; Never insert tabs
(set-default 'indent-tabs-mode nil)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; Don't break lines for me, please
;(setq-default truncate-lines t)

;; Show active region
(transient-mark-mode 1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; Remove text in active region if inserting text
;;;; (delete-selection-mode 1)

;; Save a list of recent files visited. (open recent file with C-x f)
(recentf-mode 1)
(setq recentf-max-saved-items 100) ;; just 20 is too recent

;; Save minibuffer history
(savehist-mode 1)
(setq history-length 1000)

(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "places"))


(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; I like seeing matching parens
(show-paren-mode 1)

(setq
 ;; Improve pasting behaviour with programs outside Emacs
 x-select-enable-clipboard t
 x-select-enable-primary t
 save-interprogram-paste-before-kill t
 mouse-yank-at-point t

 require-final-newline t

 ;; Also auto refresh dired, but be quiet about it
 global-auto-revert-non-file-buffers t
 auto-revert-verbose nil

 ;; Show keystrokes in progress
 echo-keystrokes 0.1

 ;; Move files to trash when deleting
 delete-by-moving-to-trash t

 ;; Always display line and column numbers
 line-number-mode t
 column-number-mode t

 ;; Don't litter my file tree with backup files
 backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))

 ;; Setting Super ＆ Hyper keys for Apple keyboard, for emacs running in OS X
 mac-command-modifier 'meta ; sets the Command key to Meta
 mac-option-modifier 'super ; sets the Option key to Super
 mac-control-modifier 'control ; sets the Control key to Control
 ns-function-modifier 'hyper  ; set Mac's Fn key to Hyper

 ispell-program-name "aspell"
 ispell-dictionary "british"

 ;; I don't want my computer beeping at me, thank you very much.
 visible-bell t


 ;; Real emacs knights don't use shift to mark things
 shift-select-mode nil)

(provide 'setup-defaults)
