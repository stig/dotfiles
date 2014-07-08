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

(add-to-list 'load-path "~/.emacs.d/setup")
(require 'setup-defaults)

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



;; full screen magit-status
;; http://whattheemacsd.com/setup-magit.el-01.html
(require 'magit)
(global-set-key (kbd "C-c C-m") 'magit-status)

(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

;; Load a nice theme...
(load-theme 'leuven t)
