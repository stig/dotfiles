; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please ... jeez
(setq inhibit-startup-message t)


(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

(add-to-list 'load-path "~/.emacs.d/local")
(require 'ensure-packages)

(setq ensure-packages
      '(better-defaults 
        dash
        cider
        clj-refactor
        clojure-cheatsheet
        clojure-mode
        clojure-snippets
        magit
        graphviz-dot-mode
        leuven-theme
        markdown-mode
        multiple-cursors))

(ensure-packages-install-missing)

;; I don't want my computer beeping at me, thank you very much.
(setq visible-bell 1)

;; Allow pasting selection outside of Emacs
(setq x-select-enable-clipboard t)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; Real emacs knights don't use shift to mark things
(setq shift-select-mode nil)

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

;; Always display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Undo/redo window configuration with C-c <left>/<right>
(winner-mode 1)

;; Never insert tabs
(set-default 'indent-tabs-mode nil)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; Don't break lines for me, please
(setq-default truncate-lines t)

;; Show active region
(transient-mark-mode 1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; Remove text in active region if inserting text
(delete-selection-mode 1)

;; Lines should be 80 characters wide, not 72
(setq fill-column 80)
; (set-default 'fill-column 80)

;; Save a list of recent files visited. (open recent file with C-x f)
(recentf-mode 1)
(setq recentf-max-saved-items 100) ;; just 20 is too recent

;; Save minibuffer history
(savehist-mode 1)
(setq history-length 1000)

(setq ispell-program-name "aspell")
(setq ispell-dictionary "british")

; setting Super ＆ Hyper keys for Apple keyboard, for emacs running in OS X
(setq mac-command-modifier 'meta) ; sets the Command key to Meta
(setq mac-option-modifier 'super) ; sets the Option key to Super
(setq mac-control-modifier 'control) ; sets the Control key to Control
(setq ns-function-modifier 'hyper)  ; set Mac's Fn key to Hyper

;; Where to find Leiningen (and others)
(add-to-list 'exec-path "/usr/local/bin")

;; Turn on yasnippets
(yas-global-mode 1)

;; Jump to end of snippet definition on hitting return
(define-key yas-keymap (kbd "<return>") 'yas/exit-all-snippets)

;; Multiple cursors...
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)



;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))


;; Turn on Clojure refactoring minor mode
(add-hook 'clojure-mode-hook (lambda ()                             
                               (clj-refactor-mode 1)
                               (cljr-add-keybindings-with-prefix "C-c C-m")

                               ;; 
                               (define-key clj-refactor-map (kbd "C-x C-r") 'cljr-rename-file)
                               (define-key clojure-mode-map (kbd "s-j") 'clj-jump-to-other-file)

                               ))



;; full screen magit-status
;; http://whattheemacsd.com/setup-magit.el-01.html

(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))


;; Load a nice theme...
(load-theme 'leuven t)
