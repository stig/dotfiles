(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

;; Bootstrap `use-package' and `dash'
(unless (and (package-installed-p 'use-package)
	     (package-installed-p 'dash))
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'bind-key)
  (package-install 'dash))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(require 'dash)

(setq user-full-name "Stig Brautaset")
(setq user-mail-address "stig@brautaset.org")

(use-package helm
  :ensure t
  :init

  ;; Ag buffer names are insanely long...
  (setq helm-buffer-max-length 35)

  :bind (("M-x" . helm-M-x)
	 ("M-y" . helm-show-kill-ring)
	 ("C-x b" . helm-mini)
	 ("C-x C-b" . helm-buffers-list)
	 ("C-x 4 b" . helm-mini)
	 ("C-x C-f" . helm-find-files))

  :config
  (require 'helm-buffers)
  ;; Magit creates many buffers, most of which are not interesting.
  (add-to-list 'helm-boring-buffer-regexp-list "\\*magit")

  ;; TAGS files are rarely interesting.
  (add-to-list 'helm-boring-buffer-regexp-list "TAGS"))

(use-package swiper
  :ensure t
  :bind (("C-x /" . swiper)))

(use-package mu4e
;;      :load-path (lambda () (expand-file-name "~/play/mu/mu4e/"))
      :load-path "/usr/local/share/emacs/site-lisp/mu/mu4e/"
      :init
      (setq mu4e-mu-binary "/usr/local/bin/mu"))

(setq mu4e-attachment-dir "~/Downloads")

(setq mu4e-view-show-images t)

;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

(setq mu4e-view-show-addresses t)

(setq mu4e-compose-complete-ignore-address-regexp
      "no-?reply")

(setq mu4e-headers-fields '((:human-date . 10)
                            (:flags . 4)
                            (:maildir . 16)
                            (:mailing-list . 8)
                            (:from . 22)
                            (:thread-subject . nil)))

(require 'mu4e-contrib)

(add-hook 'mu4e-view-mode-hook
	  (lambda()
            ;; try to emulate some of the eww key-bindings
            (local-set-key (kbd "<tab>") 'shr-next-link)
            (local-set-key (kbd "<backtab>") 'shr-previous-link)))

(add-to-list 'mu4e-headers-actions
              '("in browser" . mu4e-action-view-in-browser) t)
(add-to-list 'mu4e-view-actions
              '("in browser" . mu4e-action-view-in-browser) t)

(setq shr-use-colors nil)

(setq mu4e-bookmarks
      (list
       (make-mu4e-bookmark
        :name "Flagged Messages"
        :query "flag:flagged"
        :key ?f)
       (make-mu4e-bookmark
        :name "Unread Messages (Private)"
        :query "flag:unread AND maildir:/Private/* AND NOT maildir:/Private/spam"
        :key ?u)
       (make-mu4e-bookmark
        :name "Unread Messages (Work)"
        :query "flag:unread AND maildir:/Work/INBOX"
        :key ?w)
       (make-mu4e-bookmark
        :name "Today's messages"
        :query "date:today..now"
        :key ?t)
       (make-mu4e-bookmark
        :name "Last 7 days"
        :query "date:7d..now"
        :key ?7)))

(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun my-emacs-org-sig ()
  (concat "; " (car (split-string (emacs-version) " ("))
	  ", " (car (split-string (org-version nil t) " ("))))

(defun my-emacs-mu4e-sig ()
  (concat "; " (car (split-string (emacs-version) " ("))
	  ", Mu4e " (car (split-string mu4e-mu-version))))

(setq mu4e-maildir-shortcuts '(("/Private/INBOX" . ?i)
                               ("/Private/spam" . ?s)
                               ("/Private/Archive" . ?a)
                               ("/Work/INBOX" . ?I)
                               ("/Work/spam" . ?S)
                               ("/Work/archive" . ?A)))

;; Messages sent via GMail gets added to sent mail on the server side
(setq mu4e-sent-folder "/Private/sent")

;; I don't want to sync drafts
(setq mu4e-drafts-folder "/drafts")

(setq mu4e-trash-folder
      (lambda (msg)
        (if (string-prefix-p "/Work/" (mu4e-msg-field msg :maildir))
            "/Work/trash" "/Private/trash")))

(setq mu4e-refile-folder
      (lambda (msg)
        (if (string-prefix-p "/Work/" (mu4e-msg-field msg :maildir))
            "/Work/archive" "/Private/Archive")))

(defun sb/maildir-match (needle)
  `(lambda (msg)
     (when msg
       (s-contains-p ,needle
		     (mu4e-message-field msg :maildir)))))

(defun sb/to-match (needle)
  `(lambda (msg)
     (when msg
       (-some (lambda (entry)
		(s-contains-p ,needle (cdr entry)))
	      (mu4e-message-field msg :to)))))

(setq mu4e-contexts
      (list
       (make-mu4e-context
	:name "Org"
	:match-func (sb/to-match "emacs-orgmode")
	:vars '((user-mail-address . "stig@brautaset.org")
		(mu4e-compose-signature . (my-emacs-org-sig))))

       (make-mu4e-context
	:name "Mu"
	:match-func (sb/to-match "mu-discuss")
	:vars '((user-mail-address . "stig@brautaset.org")
		(mu4e-compose-signature . (my-emacs-mu4e-sig))))

       (make-mu4e-context
	:name "Private"
	:match-func (sb/maildir-match "/Private/")
	:vars '((user-mail-address . "stig@brautaset.org")
		(mu4e-compose-signature . nil)))

       (make-mu4e-context
	:name "GitHub"
	:match-func (sb/to-match "@reply.github.com")
	:vars '((user-mail-address . "sbrautaset@laterpay.net")
		(mu4e-compose-signature . nil)))

       (make-mu4e-context
	:name "JIRA"
	:match-func (sb/to-match "@laterpay.atlassian.net")
	:vars '((user-mail-address . "sbrautaset@laterpay.net")
		(mu4e-compose-signature . nil)))

       (make-mu4e-context
	:name "Work"
	:match-func (sb/maildir-match "/Work/")
	:vars '((user-mail-address . "sbrautaset@laterpay.net")
		(mu4e-compose-signature . (get-string-from-file "~/Dropbox/Config/LaterPay.signature"))))))

(setq mu4e-context-policy nil)
(setq mu4e-compose-context-policy 'ask)

(setq mu4e-user-mail-address-list
      '("stig@brautaset.org"
        "stig.brautaset@icloud.com"
        "sbrautaset@laterpay.net"))

(setq mu4e-compose-dont-reply-to-self t)

(setq mu4e-sent-messages-behavior
      (lambda ()
        (if (string= (message-sendmail-envelope-from) "sbrautaset@laterpay.net")
            'delete 'sent)))

(setq mu4e-headers-skip-duplicates t)

(setq smtpmail-queue-mail nil
    smtpmail-queue-dir   "~/Maildir/queue/cur")

(setq mu4e-change-filenames-when-moving t)

(setq mu4e~update-buffer-height 3)

(setq mu4e-hide-index-messages t)
(setq mu4e-update-interval nil)

(setq message-alternative-emails
      (regexp-opt mu4e-user-mail-address-list))

(setq message-from-selected-index 0)
(defun message-loop-from ()
  (interactive)
  (setq message-article-current-point (point))
  (goto-char (point-min))
  (if (eq message-from-selected-index (length mu4e-user-mail-address-list))
      (setq message-from-selected-index 0) nil)
  (while (re-search-forward "^From:.*$" nil t)
    (replace-match (concat "From: " user-full-name " <" (nth message-from-selected-index mu4e-user-mail-address-list) ">")))
  (goto-char message-article-current-point)
  (setq message-from-selected-index (+ message-from-selected-index 1)))

(add-hook 'message-mode-hook
	  (lambda ()
            (define-key message-mode-map "\C-c\C-f\C-f" 'message-loop-from)))

(use-package org-mu4e)

(use-package mu4e :bind (:map mu4e-compose-mode-map
                              ("C-c x" . org-mode)))

(use-package org-mime
  :ensure t
  :bind (:map message-mode-map
              ("C-c h" . org-mime-htmlize))
  :init
  (setq org-mime-preserve-breaks nil))

(setq message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program "/usr/local/bin/msmtp")

(setq message-kill-buffer-on-exit t)

(defun my4e~query-lists-command ()
  (concat
   "mu find --fields v 'list:.* AND date:1m.. AND flag:new' | sort -u"))

(defun my4e~headers-ask-for-list ()
  (let* ((output (shell-command-to-string
		  (my4e~query-lists-command)))
         (lists (split-string output "\n")))
    (ivy-completing-read "[mu4e] Jump to list: " lists)))

(defun my4e-headers-jump-to-list (listid)
  (interactive
   (let ((listid (my4e~headers-ask-for-list)))
     (list listid)))
  (when listid
    (mu4e-mark-handle-when-leaving)
    (mu4e-headers-search (format "flag:new list:\"%s\"" listid))))

(define-key mu4e-headers-mode-map (kbd "l") 'my4e-headers-jump-to-list)

(setq mu4e-compose-format-flowed t)

(use-package flyspell-lazy
  :ensure t
  :config
  (defun my-message-setup-routine ()
    (flyspell-mode 1))
  (add-hook 'mu4e-compose-mode-hook 'my-message-setup-routine))

(setq mu4e-get-mail-command "mbsync -a")

(use-package flymake
  :bind (:map flymake-mode-map
	 ("M-n" . flymake-goto-next-error)
	 ("M-p" . flymake-goto-prev-error)))

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(global-set-key (kbd "s-'") (kbd "æ"))
(global-set-key (kbd "s-\"") (kbd "Æ"))
(global-set-key (kbd "s-O") (kbd "Ø"))
(global-set-key (kbd "s-o") (kbd "ø"))
(global-set-key (kbd "s-A") (kbd "Å"))
(global-set-key (kbd "s-a") (kbd "å"))

(setq ispell-program-name "aspell"
      ispell-dictionary "british")

(global-set-key (kbd "C-x 8 ' l") (kbd "ł"))

(use-package lilypond-mode
  :load-path "/Applications/LilyPond.app/Contents/Resources/share/emacs/site-lisp/"
  :mode (("\\.ly\\'" . LilyPond-mode)
	 ("\\.ily\\'" . LilyPond-mode))
  :init
  (autoload 'LilyPond-mode "lilypond-mode" "LilyPond Editing Mode" t)
  (add-hook 'LilyPond-mode-hook (lambda () (turn-on-font-lock))))

(use-package magit
  :ensure t
  :bind (("M-m" . magit-status)
	 ("s-b" . magit-blame)))

(use-package magithub
  :ensure t
  :after magit
  :init
  (setq magithub-clone-default-directory "~/work")
  :config
  (magithub-feature-autoinject t))

(use-package git-link
  :ensure t
  :bind ("C-c g l" . git-link))

(use-package gist
  :ensure t
  :bind (("C-x g l" . gist-list)
         ("C-x g c" . gist-region-or-buffer-private))
  :init

  ;; The defaults for these are too small for gists
  (setq max-specpdl-size 3000)
  (setq max-lisp-eval-depth 2000)

  (setq gist-ask-for-description t))

(defun ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
		   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
(defun add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)

(use-package git-auto-commit-mode
  :ensure t
  :init
  (setq gac-shell-and "; and "))

(use-package company
  :ensure t
  :init
  ;; https://emacs.stackexchange.com/a/10838
  (setq company-dabbrev-downcase nil)
  :config
  (global-company-mode))

(set-face-attribute 'default nil :height 150)

(add-to-list 'exec-path "/usr/local/bin")

(setq mac-pass-command-to-system nil)

(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq inhibit-startup-message t)

(setq visible-bell t)

(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control))))

(defun my-toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
   nil 'fullscreen
   (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

(global-set-key (kbd "M-<f11>") 'my-toggle-fullscreen)

(setq frame-resize-pixelwise t)

(global-set-key (kbd "s-q") nil)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(if (file-exists-p custom-file)
    (load custom-file))

(require 'server)
(unless (server-running-p)
  (server-start))

(setq require-final-newline t)

(require 'whitespace)
(setq whitespace-style '(face empty tabs trailing))
(global-whitespace-mode t)

(global-set-key (kbd "s-w") 'whitespace-cleanup)

(setq mac-command-modifier 'meta
      mac-option-modifier 'super
      mac-control-modifier 'control
      ns-function-modifier 'hyper)

(global-auto-revert-mode 1)

(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

(auto-compression-mode t)

(defalias 'yes-or-no-p 'y-or-n-p)

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

(global-set-key (kbd "C-c s") 'eshell)

(set-default 'tramp-default-proxies-alist
             (quote ((".*" "\\`root\\'" "/ssh:%h:"))))

(add-to-list 'tramp-default-proxies-alist
             '((regexp-quote (system-name)) nil nil))

(setq temporary-file-directory "/tmp/")

(setq tramp-default-method "ssh")

(defun sudo-edit-current-file ()
  (interactive)
  (let ((position (point)))
    (find-alternate-file
     (if (file-remote-p (buffer-file-name))
         (let ((vec (tramp-dissect-file-name (buffer-file-name))))
           (tramp-make-tramp-file-name
            "sudo"
            (tramp-file-name-user vec)
            (tramp-file-name-host vec)
            (tramp-file-name-localname vec)))
       (concat "/sudo:root@localhost:" (buffer-file-name))))
    (goto-char position)))

(defun tramp-aware-woman (man-page-path)
  (interactive)
  (let ((dir (eshell/pwd)))
    (woman-find-file
     (if (file-remote-p dir)
         (let ((vec (tramp-dissect-file-name dir)))
           (tramp-make-tramp-file-name
            (tramp-file-name-method vec)
            (tramp-file-name-user vec)
            (tramp-file-name-host vec)
            man-page-path))
       man-page-path))))

(setq save-interprogram-paste-before-kill t)

(setq mouse-yank-at-point t)

(setq recentf-max-saved-items 100)

(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "places"))

(savehist-mode 1)

(show-paren-mode 1)

(global-set-key (kbd "M-j")
                (lambda ()
		  (interactive)
		  (join-line -1)))

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

(global-set-key (kbd "C-x t") 'ert)

(setq rcirc-default-nick "stigbra")
(setq rcirc-default-full-name "Stig Brautaset")

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

(use-package flymake-css
  :ensure t
  :config
  (add-hook 'css-mode-hook 'flymake-css-load))

(use-package aggressive-indent
  :ensure t

  :config
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'clojure-mode-hook #'aggressive-indent-mode)
  (add-hook 'css-mode-hook #'aggressive-indent-mode))

(use-package editorconfig
  :ensure t
  :init
  (setq editorconfig-exclude-modes '(org-mode))
  (setq editorconfig-mode-lighter " EC")
  :config
  (editorconfig-mode))

(use-package ag :ensure t)

(use-package wgrep-ag
  :ensure t
  :init
  (setq ag-group-matches t))

(use-package projectile
  :ensure t
  :init
  (setq projectile-mode-line
	'(:eval (format " Proj[%s]"
			(projectile-project-name))))
  :config
  (projectile-mode))

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

(use-package multiple-cursors
  :ensure t

  :bind (("C-c M-e" . mc/edit-lines)
         ("C-c M-a" . mc/mark-all-dwim)
         ("s-n" . mc/mark-next-like-this)
         ("s-p" . mc/mark-previous-like-this)))

(use-package apples-mode
  :ensure t)

(use-package sphinx-frontend
  :ensure t)

(use-package auto-complete-rst
  :ensure t
  :mode "\\.rst\'"
  :config
  (auto-complete-rst-init)
  (setq auto-complete-rst-other-sources
        '(ac-source-filename
	  ac-source-abbrev
	  ac-source-dictionary
	  ac-source-yasnippet)))

(use-package graphviz-dot-mode
  :ensure t
  :init
  (setq default-tab-width 8)
  (setq graphviz-dot-view-command "open -a Graphviz %s"))

(use-package fish-mode
  :ensure t)

(use-package osx-trash
  :ensure t
  :init
  (setq delete-by-moving-to-trash t)
  :config
  (osx-trash-setup))

(use-package plantuml-mode
  :ensure t
  :mode "\\.puml\\'"
  :init
  (setq puml-plantuml-jar-path
	(-last-item (directory-files "/usr/local/opt/plantuml/libexec" t))))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode))

(add-hook 'text-mode-hook 'visual-line-mode)

(use-package markdown-mode
  :ensure t
  :bind (("M-<up>" . markdown-move-list-item-up)
	 ("M-<down>" . markdown-move-list-item-down)))

(use-package boxquote :ensure t)

(use-package org
      :ensure org-plus-contrib
;;      :load-path (lambda () (expand-file-name "~/play/org-mode/lisp"))

      :bind (("C-c l" . org-store-link)
             ("C-c a" . org-agenda)
             ("C-c c" . org-capture)
             ("C-c b" . org-iswitchb)
             ("C-s-<return>" . org-insert-subheading)
             ("C-s-S-<return>" . org-insert-todo-subheading)
             :map org-mode-map
             ("C-c x" . mu4e-compose-mode)
             ("C-n" . org-next-link)
             ("C-p" . org-previous-link))

      :init
      (defun capture-blog-post-file ()
        (let* ((title (read-string "Slug: "))
      	 (slug (replace-regexp-in-string "[^a-z0-9]+" "-" (downcase title))))
          (expand-file-name
           (format "~/blog/articles/%s/%s.org"
      	     (format-time-string "%Y" (current-time))
      	     slug))))
      
      (setq org-default-notes-file "~/org/captured.org")
      
      (setq org-capture-templates
            '(("t" "TODOs")
      	("tt" "Plain TODO entry (with initial content if marked)" entry (file "")
      	 "* TODO %?\n\n  %i")
      	("tl" "TODO entry with link" entry (file "")
      	 "* TODO %?\n\n  %a\n\n  %i")
      	("tr" "Process email" entry (file "")
      	 "* TODO %:subject\n  SCHEDULED: %^t\n  %a\n\n  %?")
      	("te" "To Expense" entry (file "")
      	 "* TODO %:subject  :EXPENSE:\n  SCHEDULED: %^t\n\n  %a\n")
      	("tp" "New Project" entry (file "")
      	 "* PROJ %^{Project Name} :PROJ:\n  :LOGBOOK:\n  - Added: %U\n  :END:")
      
      	("m" "Meter Readings")
      	("mg" "Gas Meter" table-line (file "notes/gas-consumption.org")
      	 "|%^{Reading Time}T|%^{Reading Value}|%^{Price Per Litre|0.65}"
      	 :table-line-pos "II-1")
      	("me" "Electricity Meter" table-line (file "notes/electricity-consumption.org")
      	 "|%^{Reading Time}T|%^{Reading Value}|%^{Price Per Unit|0.1333}"
      	 :table-line-pos "II-1")
      
      	("T" "Trip" entry (file "")
      	 (file "templates/trip.org") :empty-lines 1)
      
      	("n" "Note" entry (file+datetree "Notes.org")
      	 "* %^{Subject}\n\n  %?"
      	 :empty-lines 1
      	 :clock-in t)
      
      	("a" "Absence")
      	("ah" "Holiday" entry (file+olp "Absence.org" "2018") "* %^{Reason} :Holiday:\n  %^{From}t--%^{To}t\n\n  %?%^{Holidays}p")
      	("as" "Sick leave" entry (file+olp "Absence.org" "2018") "* %^{Reason} :Sick:\n  %^{From}t--%^{To}t\n\n  %?%^{Sickdays}p")
      	("ao" "Other leave" entry (file+olp "Absence.org" "2018") "* %^{Reason} :Other:\n  %^{From}t--%^{To}t\n\n  %?%^{Days}p")
      
      	("P" "password" entry (file "~/Org/passwords.org.gpg")
      	 "* %^{Title}\n %^{URL}p %^{USERNAME}p %^{PASSWORD}p" :empty-lines 1)
      
      	("l" "Learning Log" entry (file+datetree "Learning.org")
      	 "* %^{Title} %^g\n  %?")
      
      	("b" "Blog Post" plain
      	 (file capture-blog-post-file)
      	 (file "templates/blog-post.org"))
      
      	("r" "GTD Review" entry (file+datetree "GTDReview.org")
      	 (file "templates/gtd-review.org")
      	 :empty-lines 1
      	 :clock-in t
      	 :jump-to-captured t)
      
      	("i" "New Invoice" plain (file "invoices/Invoices.org")
      	 (file "templates/invoice.org")
      	 :empty-lines 1 :immediate-finish t :jump-to-captured t)))
      (setq org-refile-targets '((org-agenda-files :maxlevel . 3)
      			   (org-agenda-files :tag . "PROJ")))
      
      ;; Allow refiling to sub-paths
      (setq org-refile-use-outline-path 'file)
      
      (setq org-refile-allow-creating-parent-nodes 'confirm)

      ;; I don't rely on many properties, so this should speed up my Agenda
      ;; view, according to http://orgmode.org/worg/agenda-optimization.html
      (setq org-agenda-ignore-properties '(effort appt stats))

      ;; When hitting C-c C-z to take a note, always put it in the LOGBOOK drawer
      (setq org-log-into-drawer t)

      ;; Sometimes I accidentally edit non-visible parts of org document. This
      ;; helps, apparently.
      (setq org-catch-invisible-edits 'show-and-error)

      ;; If running interactively, I want export to copy to the kill-ring
      (setq org-export-copy-to-kill-ring 'if-interactive)

      (setq org-hide-emphasis-markers t)

      (setq org-element-use-cache nil)

      (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

      ;; Tell Org where to find ditaa jar
      (setq org-ditaa-jar-path
            (-last-item (directory-files "/usr/local/opt/ditaa/libexec" t)))
      
      (setq org-plantuml-jar-path
            (-last-item (directory-files "/usr/local/opt/plantuml/libexec" t)))
      
      ;; Always include stderr output for shell
      (setq org-babel-default-header-args:sh
            '((:prologue . "exec 2>&1")
              (:epilogue . ":")))

      ;; I don't want to show these in the TODO list,
      ;; because they'll show in the Agenda anyway.
      (setq org-agenda-todo-ignore-scheduled 'all
            org-agenda-todo-ignore-deadlines 'all)
      
      ;; Make tags-todo search ignore scheduled items too
      (setq org-agenda-tags-todo-honor-ignore-options t)
      
      (setq org-log-done 'time)
      
      (setq org-stuck-projects '("-SOMEDAY/PROJ" ("TODO" "WAITING") nil ""))
      
      (setq org-agenda-custom-commands
            '(("u" "Current Tasks" tags-todo "-SOMEDAY/TODO")
      	("p" "Current Projects" tags-todo "-SOMEDAY/!PROJ")
      	("S" "Someday" tags-todo "SOMEDAY")
      	("w" todo "WAITING")))
      :config

      

      (org-babel-do-load-languages
       'org-babel-load-languages
       '((emacs-lisp . t)
         (clojure . t)
         (python . t)
         (gnuplot . t)
         (lilypond . t)
         (ditaa . t)
         (plantuml . t)
         (applescript . t)
         (dot . t)
         (sql . t)
         (shell . t))))

(use-package ob-applescript
  :ensure t)

;; Tell Org where to find ditaa jar
(setq org-ditaa-jar-path
      (-last-item (directory-files "/usr/local/opt/ditaa/libexec" t)))

(setq org-plantuml-jar-path
      (-last-item (directory-files "/usr/local/opt/plantuml/libexec" t)))

;; Always include stderr output for shell
(setq org-babel-default-header-args:sh
      '((:prologue . "exec 2>&1")
        (:epilogue . ":")))

(use-package ox-jira
  :ensure t)

(use-package ox-md)

(require 'ox-beamer)
(use-package ox-rst)

(use-package org-tree-slide
  :bind (("<f8>" . org-tree-slide-mode)
         ("S-<f8>" . org-tree-slide-skip-done-toggle)

         :map org-tree-slide-mode-map
         ("<f7>" . org-tree-slide-move-previous-tree)
         ("<f8>" . org-tree-slide-mode)
         ("<f9>" . org-tree-slide-move-next-tree)
         ("<f12>" . org-tree-slide-content)))

(use-package org-passwords
  :init

  (setq org-passwords-time-opened "30 min")

  ;; Where's my passwords file?
  (setq org-passwords-file "~/Org/passwords.org.gpg")

  ;; Use completion for org elements
  (setq org-completion-use-ido t)

  :bind (("C-c P P" . org-passwords)
         ("C-c P g" . org-passwords-generate-password)
         :map org-passwords-mode-map
         ("C-c C-c u" . org-passwords-copy-username)
         ("C-c C-c p" . org-passwords-copy-password)
         ("C-c C-c o" . org-passwords-open-url)))

(defun all-invoice-ids ()
  (-non-nil
   (org-map-entries (lambda ()
                      (org-entry-get nil "InvoiceId"))
                    nil
                    '("~/Org/invoices/Invoices.org"))))

(defun max-invoice-id ()
  (apply #'max
         (mapcar #'string-to-number
                 (all-invoice-ids))))

(defun next-invoice-id ()
  (number-to-string
   (+ 1
      (max-invoice-id))))

;; Prefer YMD to the crazy american MDY
(setq calendar-date-style 'iso)

;; Include Calendar/Diary information in Agenda
(setq org-agenda-include-diary t)

(defun last-weekday-of-month-p (date)
  (let* ((day-of-week (calendar-day-of-week date))
         (month (calendar-extract-month date))
         (year (calendar-extract-year date))
         (last-month-day (calendar-last-day-of-month month year))
         (month-day (cadr date)))

    (or
     ;; it's the last day of the month & it is a weekday
     (and (eq month-day last-month-day)
	  (memq day-of-week '(1 2 3 4 5)))

     ;; it's a friday, and it's the last-but-one or last-but-two days
     ;; of the month
     (and (eq day-of-week 5)
	  (or (eq month-day (1- last-month-day))
              (eq month-day (1- (1- last-month-day))))))))

(setq org-publish-project-alist
      '(("blog_rss"
	 :base-directory "~/blog"
	 :base-extension "org"
	 :rss-image-url "https://www.brautaset.org/images/logo.png"
	 :html-link-home "https://www.brautaset.org/"
	 :html-link-use-abs-url t
	 :rss-extension "xml"
	 :publishing-directory "~/public_html"
	 :publishing-function (org-rss-publish-to-rss)
	 :section-numbers nil
	 :exclude ".*"            ;; To exclude all files...
	 :include ("rss.org")     ;; ... except rss.org.
	 :table-of-contents nil)

	("blog_static"
	 :base-directory "~/blog"
	 :publishing-directory "~/public_html"
	 :base-extension "css\\|jpg\\|png\\|pdf\\|html"
	 :recursive t
	 :publishing-function org-publish-attachment)

	("blog_html"
	 :base-directory "~/blog"
	 :publishing-directory "~/public_html"
	 :publishing-function org-html-publish-to-html
	 :recursive t
	 :makeindex t
	 :section-numbers nil
	 :time-stamp-file nil
	 :with-toc nil

	 :html-doctype "html5"
	 :html-footnotes-section "<div id=\"footnotes\"><!--%s-->%s</div>"
	 :html-link-up "/"
	 :html-link-home "/"
	 :html-home/up-format "
<div id=\"org-div-home-and-up\">
  <nav>
    <ul>
      <li><a accesskey=\"H\" href=\"%s\"> Home </a></li>
      <li><a accesskey=\"p\" href=\"/publications.html\"> Publications </a></li>
      <li><a accesskey=\"A\" href=\"/about.html\"> About </a></li>
      <li><a accesskey=\"c\" href=\"/contact.html\"> Contact </a></li>
      <li>Licence: <a accesskey=\"l\" href=\"https://creativecommons.org/licenses/by-sa/4.0/\">CC BY-SA 4.0</a></li>
    </ul>
  </nav>
</div>"
	 :html-head "
<link rel=\"stylesheet\" type=\"text/css\" href=\"/css/main.css\" />
<link rel=\"icon\" type=\"image/png\" href=\"/images/icon.png\" />"

	 :html-head-extra "
<script type=\"text/javascript\">
if(/brautaset/.test(window.location.hostname)) {
  (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
  (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
  m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
  })(window,document,'script','//www.google-analytics.com/analytics.js','ga');
  ga('create', 'UA-4113456-6', 'auto');
  ga('send', 'pageview');
}
</script>"
	 :html-head-include-default-style nil
	 :html-head-include-scripts nil

	 :html-preamble nil
	 :html-postamble-format auto
	 :html-metadata-timestamp-format "%e %B %Y")))

(use-package org-drill)

(defun sb/org-time-max (a b)
  (if (org-time>= a b)
      a
    b))

(defun sb/org-time-min (a b)
  (if (org-time>= a b)
      b
    a))

(defun sb/org-columns--summary-max-time (values fmt)
  (reduce #'sb/org-time-max values))

(defun sb/org-columns--summary-min-time (values fmt)
  (reduce #'sb/org-time-min values))

(defun sb/org-collect-confirmed (property)
  "Return `PROPERTY' for `CONFIRMED' entries"
  (if (equal "[X]" (org-entry-get nil "CONFIRMED"))
      (org-entry-get nil property)
    "0"))

(defun sb/org-collect-confirmed-alt (compound-property)
  "Return `PROPERTY' for `CONFIRMED' entries"
  (let ((props (s-split-words compound-property)))
    (if (equal "[X]" (org-entry-get nil (car props)))
        (org-entry-get nil (cadr props))
      "0")))

(setq org-columns-summary-types
      '(("X+" org-columns--summary-sum sb/org-collect-confirmed)
        ("XX+" org-columns--summary-sum sb/org-collect-confirmed-alt)
        ("max-time" . sb/org-columns--summary-max-time)
        ("min-time" . sb/org-columns--summary-min-time)))

(setq org-attach-store-link-p t)

(use-package helm-org
  :bind (:map org-mode-map
              ("C-c h" . helm-org-in-buffer-headings)
              ("C-c f" . helm-org-agenda-files-headings)))

(defun sb/ediff-prepare-buffer ()
  (when (memq major-mode '(org-mode emacs-lisp-mode))
    (outline-show-all)))

(add-hook 'ediff-prepare-buffer-hook #'sb/ediff-prepare-buffer)

(use-package elpy
  :ensure t
  :config
  (elpy-enable)
  (add-hook 'elpy-mode-hook 'flymake-mode))

(define-coding-system-alias 'UTF-8 'utf-8)

(use-package python-switch-quotes
  :ensure t
  :bind ("C-c '" . python-switch-quotes))

(use-package pipenv
  :hook (python-mode . pipenv-mode)
  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended))

(use-package flymake-python-pyflakes
  :ensure t
  :init
  (setq flymake-python-pyflakes-executable "flake8")
  :config
  (add-hook 'python-mode-hook 'flymake-python-pyflakes-load))

(defun sb/disable-all-themes ()
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

(defun sb/load-theme (theme)
  "Enhance `load-theme' by first disabling enabled themes."
  (sb/disable-all-themes)
  (load-theme theme))

(setq sb/hydra-selectors
      "abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defun sb/sort-themes (themes)
  (sort themes (lambda (a b) (string< (symbol-name a) (symbol-name b)))))

(defun sb/hydra-load-theme-heads (themes)
  (mapcar* (lambda (a b)
	     (list (char-to-string a) `(sb/load-theme ',b) (symbol-name b)))
	   sb/hydra-selectors themes))

(defun sb/hydra-theme-switcher ()
  (interactive)
  (call-interactively
   (eval `(defhydra sb/hydra-select-themes (:hint nil :color pink)
	    "Select Theme"
	    ,@(sb/hydra-load-theme-heads (sb/sort-themes (custom-available-themes)))
	    ("DEL" (sb/disable-all-themes))
	    ("RET" nil "done" :color blue)))))

(use-package leuven-theme
  :defer t
  :ensure t)

(use-package material-theme
  :ensure t
  :config
  (load-theme 'material-light))

(use-package writegood-mode
  :ensure t
  :init
  (add-hook 'text-mode-hook 'writegood-mode))

(use-package string-inflection
  :bind (("C-c C-s C-v" . string-inflection-all-cycle)
	 ("C-c C-s C-c" . string-inflection-camelcase)
	 ("C-c C-s C-k" . string-inflection-kebab-case)
	 ("C-c C-s C-u" . string-inflection-upcase)))

(use-package yaml-mode :ensure t)

(use-package flymake-yaml
  :ensure t
  :config
  (add-hook 'yaml-mode-hook 'flymake-yaml-load))
