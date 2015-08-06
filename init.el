;; Turn off mouse interface early in startup to avoid momentary display
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

;; Resize screen by pixels rather than by line/row
(setq frame-resize-pixelwise t)

;; Paths to dependencies
(setq site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory))
(setq settings-dir  (expand-file-name "settings"  user-emacs-directory))

;; Set up load path
(add-to-list 'load-path settings-dir)
(add-to-list 'load-path site-lisp-dir)

;; Keep emacs Custom-settings in a separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Keep secret settings (passwords etc) in a separate file also.
(setq secret-file (expand-file-name "secret.el" user-emacs-directory))
(load secret-file)

;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))


;; 70 characters is too little
(setq-default fill-column 78)

;; wrap by default
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; s-q is too close to M-q which I use for reflowing text _all the
;; damn time_.
(global-set-key (kbd "s-q") nil)

(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)

(require 'default-settings)
(require 'tramp-settings)
(require 'modeline-settings)

(with-eval-after-load 'scala-mode2 (require 'scala-settings))

;; Join line below
(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))

(require 'server)
(unless (server-running-p)
  (server-start))

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))


;; For markdown-mode
(setq markdown-command "multimarkdown")

(add-hook 'after-init-hook 'global-company-mode)

;; These settings must be loaded after packages have been initialised
(add-hook 'after-init-hook
          (lambda ()
            (require 'smartparens-settings)
            (require 'magit-settings)
            (require 'helm-settings)
            (require 'mc-settings)
            (require 'clojure-settings)
            (require 'sphinx-frontend)
            (require 'aggressive-indent-settings)

            ;; Always recognise tables in text modes
            (require 'table)
            (add-hook 'text-mode-hook 'table-recognize)

            ;; Turn on yasnippets
            (yas-global-mode 1)
            (define-key yas-keymap (kbd "<return>") 'yas/exit-all-snippets)

            ;; Invoke `helm-git-grep' from isearch.
            (global-set-key (kbd "C-c g") 'helm-git-grep)
            (define-key isearch-mode-map (kbd "C-c g") 'helm-git-grep-from-isearch)

            ;; Invoke `helm-git-grep' from other helm.
            (eval-after-load 'helm
              '(define-key helm-map (kbd "C-c g") 'helm-git-grep-from-helm))

            (global-set-key (kbd "C-s-<up>") 'windsize-up)
            (global-set-key (kbd "C-s-<down>") 'windsize-down)
            (global-set-key (kbd "C-s-<right>") 'windsize-right)
            (global-set-key (kbd "C-s-<left>") 'windsize-left)

            (global-set-key (kbd "C-x t") 'sane-term)
            (global-set-key (kbd "C-x T") 'sane-term-create)
            (set-default 'sane-term-shell-command "/bin/zsh")))
