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
   '(cider
     clj-refactor
     clojure-cheatsheet
     clojure-mode
     clojure-snippets
     graphviz-dot-mode
     leuven-theme
     magit
     markdown-mode
     maxframe
     multiple-cursors
     better-defaults)))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

(require 'setup-defaults)

;; Load a nice theme...
(load-theme 'leuven t)

;; Where to find Leiningen (and others)
(add-to-list 'exec-path "/usr/local/bin")

(add-hook 'after-init-hook '(lambda () (require 'setup-magit)))
(add-hook 'after-init-hook '(lambda () (require 'setup-helm)))

;; Turn on Clojure refactoring minor mode
(add-hook 'clojure-mode-hook (lambda ()                             
                               (clj-refactor-mode 1)
                               (cljr-add-keybindings-with-prefix "C-c C-m")
                               (define-key clj-refactor-map (kbd "C-x C-r") 'cljr-rename-file)
                               (define-key clojure-mode-map (kbd "s-j") 'clj-jump-to-other-file)))

(eval-after-load 'clojure-mode '(require 'setup-clojure-mode))

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(setq nrepl-hide-special-buffers t)



(require 'maxframe)
(setq mf-max-width 650)
(add-hook 'window-setup-hook 'maximize-frame t)


;; Turn on yasnippets
(yas-global-mode 1)
(define-key yas-keymap (kbd "<return>") 'yas/exit-all-snippets)


;; Multiple cursors...
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-a") 'mc/mark-all-like-this)


(require 'markdown-mode)
(setq markdown-command "kramdown")


;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

