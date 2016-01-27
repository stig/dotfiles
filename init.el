;; Turn off mouse interface early in startup to avoid momentary display
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Turn off splash screen
(setq inhibit-startup-message t)

;; change to use visible bell once http://debbugs.gnu.org/cgi/bugreport.cgi?bug=21662 is fixed
(setq visible-bell nil)
(setq ring-bell-function 'ignore)


;; Keep local settings in a separate file
(setq local-file (expand-file-name "local.el" user-emacs-directory))
(if (file-exists-p local-file)
    (load local-file))

;; Keep emacs Custom-settings in a separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(if (file-exists-p custom-file)
    (load custom-file))

;; Keep secret settings (passwords etc) in a separate file also.
(setq secret-file (expand-file-name "secret.el" user-emacs-directory))
(if (file-exists-p secret-file)
    (load secret-file))

(let ((default-directory (expand-file-name "~/.homebrew/share/emacs/site-lisp")))
  (if (file-exists-p default-directory)
      (normal-top-level-add-subdirs-to-load-path)))

;; Fill at 78 columns rather than the default of 70
(setq-default fill-column 78)

;; wrap by default
(add-hook 'text-mode-hook 'turn-on-auto-fill)


(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)
    (next-line)))

(global-set-key (kbd "s-c") 'comment-or-uncomment-region-or-line)

(defun replace-smart-quotes (beg end)
  "Replace 'smart quotes' in buffer or region with ascii quotes."
  (interactive "r")
  (format-replace-strings '(("\x201C" . "\"")
                            ("\x201D" . "\"")
                            ("\x2018" . "'")
                            ("\x2019" . "'"))
                          nil beg end))

;; Disable `s-q' (kill-emacs) as it is too close to M-q which I use for
;; reflowing text.
(global-set-key (kbd "s-q") nil)

;; Let's just close window, rather than kill emacs
(global-set-key (kbd "C-x C-c") nil)

(global-set-key (kbd "C-c e") (lambda ()
                                (interactive)
                                (find-file "~/.emacs.d/init.el")))

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

;; Allow ssh+sudo with tramp
(set-default 'tramp-default-proxies-alist
             (quote ((".*" "\\`root\\'" "/ssh:%h:"))))

;;
;; Packages installed with package.el
;;


(package-initialize)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))

(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.milkbox.net/packages/"))

(unless (package-installed-p 'use-package)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)

(use-package leuven-theme
  :ensure t
  :config
  (load-theme 'leuven t))

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
  (sp-pair "'" nil :actions :rem)

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

  ;; In Lisp modes, let ')' go to end of sexp
  (bind-key ")" 'sp-up-sexp emacs-lisp-mode-map)
  (bind-key ")" 'sp-up-sexp lisp-mode-map))

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

(use-package ac-ispell
  :ensure t
  :init
  ;; Completion words longer than 4 characters
  (custom-set-variables
   '(ac-ispell-requires 8)
   '(ac-ispell-fuzzy-limit 8))

  :config
  (ac-ispell-setup)

  (add-hook 'git-commit-mode-hook 'ac-ispell-ac-setup)
  (add-hook 'mail-mode-hook 'ac-ispell-ac-setup))

(use-package helm
  :ensure t

  :bind (("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-mini)
         ("C-x 4 b" . helm-mini)
         ("C-x C-f" . helm-find-files)))

(use-package ac-helm
  :ensure t
  :bind ("C-." . ac-complete-with-helm))

(use-package wgrep-ag
  :ensure t)

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

(use-package yasnippet
  :config
  (yas-global-mode 1)
  (add-hook 'term-mode-hook
            (lambda() (yas-minor-mode -1))))

(use-package clojure-mode
  :pin melpa-stable
  :mode "\\.clj\\'"
  :ensure t

  :config
  (bind-key ")" 'sp-up-sexp clojure-mode-map)

  (use-package clj-refactor
    :pin melpa-stable
    :ensure t
    :config
    (dolist (mapping '(("route" . "compojure.route")
                       ("timbre" . "taoensso.timbre")
                       ("component" . "com.stuartsierra.component")
                       ("d" . "datomic.api")
                       ("io" . "clojure.java.io")
                       ("tc" . "clojure.test.check")
                       ("gen" . "clojure.test.check.generators")
                       ("prop" . "clojure.test.check.properties")
                       ("prop'" . "com.gfredericks.test.chuck.properties")))
      (add-to-list 'cljr-magic-require-namespaces mapping t))

    :config
    (defun my-clojure-mode-hook ()
      (clj-refactor-mode 1)
      ;;(cljr-add-keybindings-with-prefix "C-c C-m")
      )

    (add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

    (use-package cljr-helm
      :ensure t
      :init
      (bind-key "C-c r" 'cljr-helm clojure-mode-map)))

  (use-package clojure-mode-extra-font-locking
    :ensure t)
  )


(use-package cider
  :pin melpa-stable
  :ensure t
  :init
  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
  (setq cider-repl-result-prefix ";; => ")

  :config
  (bind-key ")" 'sp-up-sexp cider-repl-mode-map)

  (use-package ac-cider
    :ensure t
    :init
    (add-hook 'cider-mode-hook 'ac-flyspell-workaround)
    (add-hook 'cider-mode-hook 'ac-cider-setup)
    (add-hook 'cider-repl-mode-hook 'ac-cider-setup)
    (eval-after-load "auto-complete"
      '(progn
         (add-to-list 'ac-modes 'cider-mode)
         (add-to-list 'ac-modes 'cider-repl-mode)))))

(use-package puppet-mode
  :mode "\\.pp'")

(use-package editorconfig
  :ensure t)

(use-package sbt-mode
  :mode "\\.sbt\\'"
  :init
  ;; compilation-skip-threshold tells the compilation minor-mode
  ;; which type of compiler output can be skipped. 1 = skip info
  ;; 2 = skip info and warnings.
  (setq compilation-skip-threshold 1)
  :config
  ;; Bind C-a to 'comint-bol when in sbt-mode. This will move the
  ;; cursor to just after prompt.
  (bind-key "C-a" 'comint-bol)

  ;; Bind M-RET to 'comint-accumulate. This will allow you to add
  ;; more than one line to scala console prompt before sending it
  ;; for interpretation. It will keep your command history cleaner.
  (bind-key "M-RET" 'comint-accumulate))

(use-package scala-mode2
  :mode ("\\.scala\\'" . scala-mode)
  :config
  ;; sbt-find-definitions is a command that tries to find (with grep)
  ;; the definition of the thing at point.
  (bind-key "M-." 'sbt-find-definitions)
  ;; use sbt-run-previous-command to re-compile your code after changes
  (bind-key "C-x '" 'sbt-run-previous-command))

(use-package gist
  :ensure t
  :bind ("C-x g l" . gist-list))

(use-package yagist
  :ensure t
  :bind ("C-x g c" . yagist-region-or-buffer))

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode))

(use-package org
  :ensure t
  :bind ("C-x a" . org-agenda)
  :init
  (setq org-babel-clojure-backend 'cider)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (clojure . t)
     (dot . t)
     (sh . t))))

(use-package sane-term
  :bind (("C-x t" . sane-term)
         ("C-x T" . sane-term-create)))

(use-package tramp-term
  :bind ("C-x C-t" . tramp-term))

(use-package graphviz-dot-mode
  :init
  (setq graphviz-dot-view-command "open -a Graphviz %s"))
