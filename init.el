;; Turn off mouse interface early in startup to avoid momentary display
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Turn off splash screen
(setq inhibit-startup-message t)

;; change to use visible bell once http://debbugs.gnu.org/cgi/bugreport.cgi?bug=21662 is fixed
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; Keep emacs Custom-settings in a separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Keep secret settings (passwords etc) in a separate file also.
(setq secret-file (expand-file-name "secret.el" user-emacs-directory))
(load secret-file)

(let ((default-directory "/usr/local/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;; Fill at 78 columns rather than the default of 70
(setq-default fill-column 78)

;; wrap by default
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Disable `s-q' (kill-emacs) as it is too close to M-q which I use for
;; reflowing text.
(global-set-key (kbd "s-q") nil)

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(define-key ctl-x-4-map "t" 'toggle-window-split)

(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)

;; Auto refresh buffers
(global-auto-revert-mode 1)

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

;; Transparently open compressed files
(auto-compression-mode t)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top

;; Norwegian characters
(global-set-key (kbd "s-'") (kbd "æ"))
(global-set-key (kbd "s-\"") (kbd "Æ"))
(global-set-key (kbd "s-O") (kbd "Ø"))
(global-set-key (kbd "s-o") (kbd "ø"))
(global-set-key (kbd "s-A") (kbd "Å"))
(global-set-key (kbd "s-a") (kbd "å"))

;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

;; Never insert tabs
(set-default 'indent-tabs-mode nil)

;; Show active region
(transient-mark-mode 1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; Save a list of recent files visited. (open recent file with C-x f)
(recentf-mode 1)
(setq recentf-max-saved-items 100) ;; just 20 is too recent

;; Save minibuffer history
(savehist-mode 1)
(setq history-length 1000)

(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "places"))

(show-paren-mode 1)

;; Improve pasting behaviour with programs outside Emacs
(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      mouse-yank-at-point t)

(setq require-final-newline t)

;; Auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

(setq delete-by-moving-to-trash t)

;; Don't litter my file tree with backup files
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

(setq mac-command-modifier 'meta
      mac-option-modifier 'super
      mac-control-modifier 'control
      ns-function-modifier 'hyper)


(setq ispell-program-name "aspell"
      ispell-dictionary "british")


;; Join line below
(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))


;; Display whitespace annoyances
(require 'whitespace)
(setq whitespace-style '(face empty tabs trailing))
(global-whitespace-mode t)

(global-set-key (kbd "s-w") 'whitespace-cleanup)



(require 'server)
(unless (server-running-p)
  (server-start))

;; Fix to allow editing remote files over ssh
(put 'temporary-file-directory
     'standard-value
     '((file-name-as-directory "/tmp")))

;; Allow ssh+sudo with tramp
(set-default 'tramp-default-proxies-alist
             (quote ((".*" "\\`root\\'" "/ssh:%h:"))))

;; SSH is faster than the default scp mode (apparently)
(setq tramp-default-method "ssh")


;;
;; Packages installed with package.el
;;


(package-initialize)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))

(unless (package-installed-p 'use-package)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)

(use-package magit
  :ensure t

  :bind ("M-m" . magit-status)

  :init
  (setq magit-git-executable "/usr/bin/git"
        git-commit-summary-max-length 65
        magit-diff-refine-hunk 'all
        magit-push-always-verify nil)

  :config
  (use-package magit-gh-pulls
    :ensure t
    :config
    (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)))

(use-package markdown-mode
  :ensure t
  :mode "\\.md'"

  :init
  (setq markdown-command "multimarkdown"))

(use-package smartparens
  :ensure t

  :config
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)

  ;; Add smartparens-strict-mode to all sp--lisp-modes hooks. C-h v sp--lisp-modes
  ;; to customize/view this list.
  (mapc (lambda (mode)
          (add-hook (intern (format "%s-hook" (symbol-name mode))) 'smartparens-strict-mode))
        sp--lisp-modes)

  ;; Conveniently set keys into the sp-keymap, limiting the keybinding to buffers
  ;; with SP mode activated
  (mapc (lambda (info)
          (let ((key (kbd (car info)))
                (function (car (cdr info))))
            (define-key sp-keymap key function)))
        '(("C-M-f" sp-forward-sexp)
          ("C-M-b" sp-backward-sexp)

          ("C-M-d" sp-down-sexp)
          ("C-M-a" sp-backward-down-sexp)
          ("C-S-a" sp-beginning-of-sexp)
          ("C-S-d" sp-end-of-sexp)

          ("C-M-e" sp-up-sexp)

          ("C-M-u" sp-backward-up-sexp)
          ("C-M-t" sp-transpose-sexp)

          ("C-M-n" sp-next-sexp)
          ("C-M-p" sp-previous-sexp)

          ("C-M-k" sp-kill-sexp)
          ("C-M-w" sp-copy-sexp)

          ("C-M-<delete>" sp-unwrap-sexp)
          ("C-M-<backspace>" sp-backward-unwrap-sexp)

          ("C-<right>" sp-forward-slurp-sexp)
          ("C-<left>" sp-forward-barf-sexp)
          ("C-M-<left>" sp-backward-slurp-sexp)
          ("C-M-<right>" sp-backward-barf-sexp)

          ("M-D" sp-splice-sexp)
          ("C-M-<delete>" sp-splice-sexp-killing-forward)
          ("C-M-<backspace>" sp-splice-sexp-killing-backward)
          ("C-S-<backspace>" sp-splice-sexp-killing-around)

          ("C-]" sp-select-next-thing-exchange)
          ("C-<left_bracket>" sp-select-previous-thing)
          ("C-M-]" sp-select-next-thing)

          ("M-F" sp-forward-symbol)
          ("M-B" sp-backward-symbol)

          ("H-t" sp-prefix-tag-object)
          ("H-p" sp-prefix-pair-object)
          ("H-s c" sp-convolute-sexp)
          ("H-s a" sp-absorb-sexp)
          ("H-s e" sp-emit-sexp)
          ("H-s p" sp-add-to-previous-sexp)
          ("H-s n" sp-add-to-next-sexp)
          ("H-s j" sp-join-sexp)
          ("H-s s" sp-split-sexp)))

  ;; This is from authors config, seems to let you jump to the end of the current
  ;; sexp with paren?
  (define-key emacs-lisp-mode-map (kbd ")") 'sp-up-sexp))

(use-package aggressive-indent
  :ensure t

  :config
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'puppet-mode-hook #'aggressive-indent-mode)
  (add-hook 'clojure-mode-hook #'aggressive-indent-mode)
  (add-hook 'css-mode-hook #'aggressive-indent-mode))

(use-package auto-complete
  :ensure t
  :config
  (ac-config-default))

(use-package helm
  :ensure t

  ;;  :init
  ;;  (setq helm-command-prefix-key "C-c h")

  :bind (("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)))

(use-package ac-helm
  :ensure t
  :bind ("C-." . ac-complete-with-helm))

(use-package helm-git-grep
  :ensure t

  :bind ("C-c g" . helm-git-grep)

  :config
  (define-key isearch-mode-map (kbd "C-c g") 'helm-git-grep-from-isearch)
  (define-key helm-map (kbd "C-c g") 'helm-git-grep-from-helm))

(use-package helm-git-files
  :ensure t

  :bind ("C-c f" . helm-git-files))

(use-package multiple-cursors
  :ensure t

  :bind (("C-c a" . mc/edit-lines)
         ("C-c C-a" . mc/mark-all-dwim)
         ("s-n" . mc/mark-next-like-this)
         ("s-p" . mc/mark-previous-like-this)))

(use-package auto-complete-rst
  :mode "\\.rst\'"
  :config
  (auto-complete-rst-init)
  (setq auto-complete-rst-other-sources
        '(ac-source-filename
          ac-source-abbrev
          ac-source-dictionary
          ac-source-yasnippet)))

(use-package clojure-mode
  :mode "\\.clj\\'"
  :ensure t

  :config
  (defun clj--src-file-name-from-test (name)
    (s-with name
      (s-replace "/test/" "/src/")
      (s-replace "_test.clj" ".clj")))

  (defun clj--test-file-name-from-src (name)
    (s-with name
      (s-replace "/src/" "/test/")
      (s-replace ".clj" "_test.clj")))

  (defun clj--other-file-name ()
    (let ((name (buffer-file-name)))
      (if (string-match-p "/test/" name)
          (clj--src-file-name-from-test name)
        (clj--test-file-name-from-src name))))

  (defun clj-jump-to-other-file (arg)
    (interactive "P")
    (let ((file (clj--other-file-name)))
      (cond
       ((file-exists-p file) (find-file file))
       (arg (find-file file)
            (save-buffer))
       (t (error "%s not found." file)))))

  (defun clj-jump-to-other-file-other-window (arg)
    (interactive "P")
    (let ((file (clj--other-file-name)))
      (if (or (file-exists-p file) arg)
          (find-file-other-window file)
        (error "%s not found." file))))

  (bind-key "C-c o" 'clj-jump-to-other-file clojure-mode-map)
  (bind-key "C-c C-o" 'clj-jump-to-other-file-other-window clojure-mode-map)

  ;;  (define-key clojure-mode-map (kbd "C-c o") 'clj-jump-to-other-file)

  (use-package clj-refactor
    :ensure t
    :config
    (dolist (mapping '(("route" . "compojure.route")
                       ("timbre" . "taoensso.timbre")
                       ("component" . "com.stuartsierra.component")
                       ("d" . "datomic.api")))
      (add-to-list 'cljr-magic-require-namespaces mapping t))

    :config
    (defun my-clojure-mode-hook ()
      (clj-refactor-mode 1)
      (yas-minor-mode 1) ; for adding require/use/import
      ;;(cljr-add-keybindings-with-prefix "C-c C-m")
      )

    (add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

    (use-package cljr-helm
      :ensure t
      :init
      (bind-key "C-c r" 'cljr-helm clojure-mode-map)))

  (use-package clojure-mode-extra-font-locking
    :ensure t)

  (use-package cider
    :ensure t
    :init
    (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
    (setq cider-repl-result-prefix ";; => ")

    :config
    (use-package ac-cider
      :ensure t
      :init
      (add-hook 'cider-mode-hook 'ac-flyspell-workaround)
      (add-hook 'cider-mode-hook 'ac-cider-setup)
      (add-hook 'cider-repl-mode-hook 'ac-cider-setup)
      (eval-after-load "auto-complete"
        '(progn
           (add-to-list 'ac-modes 'cider-mode)
           (add-to-list 'ac-modes 'cider-repl-mode))))))

(use-package puppet-mode
  :ensure t
  :mode "\\.pp'")

(use-package editorconfig
  :ensure t)

(use-package table
  :ensure t
  :init
  (add-hook 'text-mode-hook 'table-recognize))

;;;; cider
;; (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
