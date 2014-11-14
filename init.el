;; Turn off mouse interface early in startup to avoid momentary display
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

;; Resize screen by pixels rather than by line/row
(setq frame-resize-pixelwise t)

(require 'server)
(unless (server-running-p)
  (server-start))

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
; (package-refresh-contents)

(defun package-install? (package-name)
  (when (not (package-installed-p package-name))
    (package-install package-name)))

(ido-mode t)

;; Auto refresh buffers
(global-auto-revert-mode 1)

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

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

;; Norwegian characters
(global-set-key (kbd "s-'") (kbd "æ"))
(global-set-key (kbd "s-\"") (kbd "Æ"))
(global-set-key (kbd "s-O") (kbd "Ø"))
(global-set-key (kbd "s-o") (kbd "ø"))
(global-set-key (kbd "s-A") (kbd "Å"))
(global-set-key (kbd "s-a") (kbd "å"))

;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

;; Undo/redo window configuration with C-c <left>/<right>
(winner-mode 1)

;; Never insert tabs
(set-default 'indent-tabs-mode nil)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

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

(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "places"))


;; Removed as default in Emacs 24.4
;;(require 'uniquify)
;;(setq uniquify-buffer-name-style 'post-forward)

;; I like seeing matching parens
(show-paren-mode 1)

(setq
 ;; Improve pasting behaviour with programs outside Emacs
 x-select-enable-clipboard t
 x-select-enable-primary t
 save-interprogram-paste-before-kill t
 mouse-yank-at-point t

 require-final-newline t

 ;; Also auto refresh dired, but be quiet about it
 global-auto-revert-non-file-buffers t
 auto-revert-verbose nil

 ;; Show keystrokes in progress
 echo-keystrokes 0.1

 ;; Move files to trash when deleting
 delete-by-moving-to-trash t

 ;; Always display line and column numbers
 line-number-mode t
 column-number-mode t

 ;; Don't litter my file tree with backup files
 backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))

 ;; Setting Super ＆ Hyper keys for Apple keyboard, for emacs running in OS X
 mac-command-modifier 'meta ; sets the Command key to Meta
 mac-option-modifier 'super ; sets the Option key to Super
 mac-control-modifier 'control ; sets the Control key to Control
 ns-function-modifier 'hyper  ; set Mac's Fn key to Hyper

 ispell-program-name "aspell"
 ispell-dictionary "british"

 ;; I don't want my computer beeping at me, thank you very much.
 visible-bell t


 ;; Real emacs knights don't use shift to mark things
 shift-select-mode nil)

(package-install? 'helm-projectile)
(package-install? 'graphviz-dot-mode)

;; Load a nice theme...
(package-install? 'leuven-theme)
(load-theme 'leuven t)

;; Where to find Leiningen (and others)
(add-to-list 'exec-path "/usr/local/bin")

(package-install? 'magit)
(require 'magit)
(global-set-key (kbd "M-m") 'magit-status)

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
(setq ediff-autostore-merges t)

(package-install? 'helm)
;; must set before helm-config,  otherwise helm use default
;; prefix "C-x c", which is inconvenient because you can
;; accidentially pressed "C-x C-c"
(setq helm-command-prefix-key "C-c h")

(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)


(package-install? 'clojure-mode)

(package-install? 'align-cljlet)
(require 'align-cljlet)

(package-install? 'paredit) ;; required by clj-refactor
(package-install? 'clj-refactor)

;; Turn on Clojure refactoring minor mode
(add-hook 'clojure-mode-hook (lambda ()
                               (clj-refactor-mode 1)
                               (cljr-add-keybindings-with-prefix "C-c r")
                               (define-key clojure-mode-map (kbd "C-c o") 'clj-jump-to-other-file)))

(require 's)

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


(package-install? 'cider)

(package-install? 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; (add-hook 'cider-repl-mode-hook 'company-mode)
;; (add-hook 'cider-mode-hook 'company-mode)

(package-install? 'smartparens)
(require 'smartparens-config)
(smartparens-global-mode t)
(show-smartparens-global-mode t)

;; Add smartparens-strict-mode to all sp--lisp-modes hooks. C-h v sp--lisp-modes
;; to customize/view this list.
(mapc (lambda (mode)
        (add-hook (intern (format "%s-hook" (symbol-name mode))) 'smartparens-strict-mode))
      sp--lisp-modes)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

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
(define-key emacs-lisp-mode-map (kbd ")") 'sp-up-sexp)


;(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(setq nrepl-hide-special-buffers t)

;; Turn on yasnippets
(yas-global-mode 1)
(define-key yas-keymap (kbd "<return>") 'yas/exit-all-snippets)

(package-install? 'helm-projectile)
(projectile-global-mode)
(setq projectile-enable-caching t
      projectile-switch-project-action 'projectile-dired
      projectile-use-git-grep t)

(package-install? 'helm-git-grep)
(global-set-key (kbd "C-c g") 'helm-git-grep)
;; Invoke `helm-git-grep' from isearch.
(define-key isearch-mode-map (kbd "C-c g") 'helm-git-grep-from-isearch)
;; Invoke `helm-git-grep' from other helm.
(eval-after-load 'helm
  '(define-key helm-map (kbd "C-c g") 'helm-git-grep-from-helm))

(package-install? 'multiple-cursors)
(require 'multiple-cursors)
(global-set-key (kbd "C-c a") 'mc/edit-lines)
(global-set-key (kbd "s-n") 'mc/mark-next-like-this)
(global-set-key (kbd "s-p") 'mc/mark-previous-like-this)

;; Windsize
(package-install? 'windsize)
(require 'windsize)
(global-set-key (kbd "C-s-<up>") 'windsize-up)
(global-set-key (kbd "C-s-<down>") 'windsize-down)
(global-set-key (kbd "C-s-<right>") 'windsize-right)
(global-set-key (kbd "C-s-<left>") 'windsize-left)

(package-install? 'markdown-mode)
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


;; Fix to allow editing remote files over ssh
(put 'temporary-file-directory 'standard-value '((file-name-as-directory "/tmp")))

;; Allow ssh+sudo with tramp
(set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:"))))

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

(global-set-key (kbd "C-x C-c") 'save-buffers-kill-emacs)


;; Function for inserting the current date
(defun insert-current-date (arg)
    (interactive "P")
    (insert (format-time-string "%Y-%m-%d")))

(global-set-key (kbd "C-x M-d") 'insert-current-date)

(package-install? 'sane-term)
(global-set-key (kbd "C-x t") 'sane-term)
(global-set-key (kbd "C-x T") 'sane-term-create)
(set-default 'sane-term-shell-command "/bin/zsh")

(fset 'join-line-below [down ?\M-^])
(global-set-key (kbd "M-s-^") 'join-line-below)

;;(package-install? 'aggressive-indent)
;;(global-aggressive-indent-mode 1)
;;(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
;;(add-hook 'css-mode-hook #'aggressive-indent-mode)
