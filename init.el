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



;; Load a nice theme...
(load-theme 'leuven t)

;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))
