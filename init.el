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
     clojure-mode
     graphviz-dot-mode
     leuven-theme
     magit
     helm-projectile
     markdown-mode
     maxframe
     multiple-cursors
     windsize)))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

(require 'setup-defaults)
(require 'setup-mail)

;; Load a nice theme...
(load-theme 'leuven t)

;; Where to find Leiningen (and others)
(add-to-list 'exec-path "/usr/local/bin")

(add-hook 'after-init-hook '(lambda () (require 'setup-magit)))
(add-hook 'after-init-hook '(lambda () (require 'setup-helm)))

;; Turn on Clojure refactoring minor mode
(add-hook 'clojure-mode-hook (lambda ()
                               (clj-refactor-mode 1)
                               (cljr-add-keybindings-with-prefix "s-r")
                               (define-key clojure-mode-map (kbd "s-o") 'clj-jump-to-other-file)))

(eval-after-load 'clojure-mode '(require 'setup-clojure-mode))

;; require or autoload paredit-mode
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

;(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(setq nrepl-hide-special-buffers t)

(toggle-frame-fullscreen)

;; Turn on yasnippets
(yas-global-mode 1)
(define-key yas-keymap (kbd "<return>") 'yas/exit-all-snippets)

(projectile-global-mode)

;; Multiple cursors...
(require 'multiple-cursors)
(global-set-key (kbd "C-s-c C-s-c") 'mc/edit-lines)
(global-set-key (kbd "s-<right>") 'mc/mark-next-like-this)
(global-set-key (kbd "s-<left>") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-a") 'mc/mark-all-like-this)

;; Windsize
(require 'windsize)
(global-set-key (kbd "C-s-<up>") 'windsize-up)
(global-set-key (kbd "C-s-<down>") 'windsize-down)
(global-set-key (kbd "C-s-<right>") 'windsize-right)
(global-set-key (kbd "C-s-<left>") 'windsize-left)

(require 'markdown-mode)
(setq markdown-command "kramdown")

;; Clean up whitespace atrocities in files on save
(add-hook 'before-save-hook 'whitespace-cleanup)

(defun replace-smart-quotes (beg end)
  "Replace 'smart quotes' in buffer or region with ascii quotes."
  (interactive "r")
  (format-replace-strings '(("\x201C" . "\"")
                            ("\x201D" . "\"")
                            ("\x2018" . "'")
                            ("\x2019" . "'"))
                          nil beg end))

;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))
