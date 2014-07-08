; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

(add-to-list 'load-path "~/.emacs.d/setup")
(require 'setup-package)

;; Install extensions if they're missing
(defun init--install-packages ()
  (packages-install
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
     multiple-cursors)))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

(require 'setup-defaults)

;; Where to find Leiningen (and others)
(add-to-list 'exec-path "/usr/local/bin")

(add-hook 'after-init-hook '(lambda () (require 'setup-magit)))
(eval-after-load 'clojure-mode '(require 'setup-clojure-mode))

;; Turn on yasnippets
(yas-global-mode 1)

;; Jump to end of snippet definition on hitting return
(define-key yas-keymap (kbd "<return>") 'yas/exit-all-snippets)

;; Multiple cursors...
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-a") 'mc/mark-all-like-this)

;; Load a nice theme...
(load-theme 'leuven t)

;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))
