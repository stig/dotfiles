; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

;; I don't want my computer beeping at me, thank you very much.
(setq visible-bell 1)

(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t))

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



