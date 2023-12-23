;;; -*- lexical-binding: t; -*-

;;; Commentary:

;; This is my (Stig Brautaset) Emacs configuration. It used to be an
;; Org document that I tangled into ~/.config/emacs/init.el, but I'm
;; getting more comfortable with Emacs lisp and the indirection is
;; starting to grate on me.

;;; Who am I?

;; Set up my name, email address, and login. I use the email in my
;; Notmuch config below, and some of these show up in Org mode's
;; export metadata.

(setq user-full-name "Stig Brautaset"
      user-mail-address "sonar_columns_0n@icloud.com"
      user-login-name "stig")

;;; Better defaults:

;; Ask for textual confirmation rather than use a pop-up file dialog.
(setq use-file-dialog nil)

;; Make right-click do something sensible
(when (display-graphic-p)
  (context-menu-mode))

;; Configure Control/Meta/super/hyper keys

;; Ideally I should probably set these only when I'm using my
;; Moonlander keyboard, but I almost always do so it's not something
;; I've investigated yet.
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta
	mac-control-modifier 'control
	mac-function-modifier 'hyper
	mac-option-modifier 'super))


;; Don't require two spaces to end a sentence. This makes it easier to
;; collaborate, and work with texts I didn't originally write.
(setq sentence-end-double-space nil)

;; End all files in a newline. Insert one if there isn't one already.
(setq require-final-newline t)

;; Ensure we don't use unencrypted auth sources. This could take the
;; form of e.g. saving auth info to =~/.authinfo=, or =~/.netrc=
;; rather than =~/.authinfo.gpg=, for example.
(setq auth-sources (quote ("~/.authinfo.gpg")))

;; Make emacs query passwords in the minibuffer, rather than through
;; an external program.
(setq epg-pinentry-mode 'loopback)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq calendar-date-style 'iso)

;; Put Customize settings in a separate file. I prefer to code my
;; configuration, most of the time, but sometimes it's good enough. I
;; do want them in a separate file though.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Store backup files in a separate folder. I don't like backup files
;; all over my disk. This places them in a separate directory.
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backups" user-emacs-directory))))

;; Support recursive minibuffers. I like to be able to use my
;; kill-ring in the minibuffer.
(setq enable-recursive-minibuffers t)

;; Upcase-dwim and dowcase-dwim. Acts like `upcase-word' with no
;; region selected, and `upcase-region' when one is.
(keymap-global-set "M-u" #'upcase-dwim)
(keymap-global-set "M-l" #'downcase-dwim)

;; Prefer UTF-8
(setq locale-coding-system 'utf-8)
(prefer-coding-system 'utf-8)


;; The GUI Emacs gets exec path from the system, rather than the login
;; shell. We have to load `PATH' et. al. from the shell to get access
;; to programs installed by Nix. Copy `PATH' and `MANPATH' from my
;; login shell so these variables are available in Eshell. (And
;; elsewhere in Emacs.)
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;;;; Which Key Mode

;; This shows hints about what keyboard bindings are available if I
;; type a prefix but hesitate before completing the full command.
(require 'which-key)
(which-key-mode)

;;;; Orderless

(require 'orderless)
(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion))))

;;; Appearance:

(set-face-attribute 'default nil :font "JetBrains Mono" :height 150)
(set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" :height 150)
(set-face-attribute 'variable-pitch nil :font "Optima" :height 1.3)

(mapc (lambda (hook)
        (add-hook hook #'variable-pitch-mode))
      (quote (adoc-mode-hook
	      markdown-mode-hook
	      message-mode-hook
	      org-mode-hook)))

(require 'org-faces)
(set-face-attribute 'org-block nil :foreground 'unspecified :inherit 'fixed-pitch)
(set-face-attribute 'org-table nil :inherit 'fixed-pitch)
(set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

;;; Misc

;; Consult
(require 'consult)
(keymap-global-set "C-c r" #'consult-ripgrep)

(require 'consult-imenu)
(keymap-global-set "C-c i" #'consult-imenu)


;; Unfill paragraphs and regions. The default binding for =M-q= fills
;; a paragraph. Very good. But sometimes I want to /unfill/,
;; particularly when editing markdown that is going to end up on
;; GitHub. Otherwise the result has lots of hard linebreaks. This
;; happens every time I edit a PR description in Magit, for example.
;; Credit: https://gist.github.com/heikkil/a3edf506046c84f6f508edbaf005810a
(defun endless/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command #'endless/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (if (eq major-mode 'org-mode)
        (call-interactively #' org-fill-paragraph)
      (call-interactively #'fill-paragraph))))

(keymap-global-set "M-q" #'endless/fill-or-unfill)

;; Toggle Window Split function. Re-arranges horizontally split
;; windows to be vertically split, and vice versa. I found it on
;; StackOverflow, once upon a time but now can't find the link.
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


(keymap-set ctl-x-4-map "t" 'toggle-window-split)

;; Ediff: Picking both sides in a conflict. If both branches add an
;; entry to a list I may want to pick both sides. This adds `d' as a
;; shortcut to do that. I can use `~' to swap the A and B buffers,
;; which lets me choose A then B, OR B then A. Credits:
;; http://stackoverflow.com/a/29757750/5950.
(defun sb/ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents
                     ediff-current-difference 'B
                     ediff-control-buffer))))

(defun sb/add-d-to-ediff-mode-map ()
  (keymap-set ediff-mode-map "d" 'sb/ediff-copy-both-to-C))

(add-hook 'ediff-keymap-setup-hook 'sb/add-d-to-ediff-mode-map)

;; Ediff: Automatically Unfold Org files. This snippet makes sure that
;; Org buffers don't start folded, as ediff is rather useless in that
;; case. (Credit: Oleh Krehel on emacs-orgmode mailing list.)
(defun sb/ediff-prepare-buffer ()
  (when (memq major-mode '(org-mode emacs-lisp-mode))
    (outline-show-all)))

(add-hook 'ediff-prepare-buffer-hook #'sb/ediff-prepare-buffer)

;; Delete buffer and the file it is visiting. Copied from
;; https://github.com/sulami/dotfiles/blob/master/emacs/.emacs/README.org#delete-buffer-file
(defun sb/delete-file-and-buffer ()
  "Deletes a buffer and the file it's visiting."
  (interactive)
  (when-let* ((file-name (buffer-file-name))
              (really (yes-or-no-p (format "Delete %s? "
                                           file-name))))
    (delete-file file-name)
    (kill-buffer)))

;; This lets me rapidly switch to a different frame/window. I use this
;; mainly when resolving conflicts in ediff merge, since I need to
;; swap between two frames there.
(require 'ace-window)
(keymap-global-set "C-S-s-<tab>" 'ace-window)
(keymap-global-set "M-`" 'ace-window)


(require 'docker)

(require 'dumb-jump)
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate 100)

;; To edit code examples in a separate buffer from Markdown. (Similar
;; to code blocks in Org.)
(require 'edit-indirect)

(require 'expand-region)
(keymap-global-set "C-=" #'er/expand-region)

(require 'git-link)
(keymap-global-set "C-c g l" #'git-link)

(require 'multiple-cursors)

(require 'rg)
(setq rg-command-line-flags '("--hidden" "--glob='!.git'"))
(rg-enable-default-bindings)

(require 'server)
(unless (server-running-p)
  (server-start))

(require 'string-inflection)

(require 'sudo-edit)



;;;; Magit & Forge

;; I use Magit, a git porcelain for Emacs, all day. I rarely use the
;; git cli any more. I've seen someone suggest learning Emacs just to
;; run Magit.
;;
;; Forge is an extension to Magit that lets me create & manipulate
;; pull requests on GitHub / GitLab etc.

(require 'magit)
(require 'forge)

(keymap-global-set "C-S-s-m" #'magit-status)
(keymap-global-set "C-S-s-b" #'magit-blame-addition)

(with-eval-after-load "project"
  (keymap-set project-prefix-map "m" #'magit-status)

  ;; Replace vc-el with Magit in project commands
  (setq project-switch-commands
	(remove '(project-vc-dir "VC-Dir") project-switch-commands))
  (add-to-list 'project-switch-commands '(magit-status "Magit" nil) t))

;;; Minor Modes:

;; I want history in my minibuffer.
(savehist-mode 1)

;; I like it when Emacs knows where I was last time I opened a file.
(save-place-mode 1)

;; Put the cursor on any of =()[]{}= and Emacs shows the matching closing/opening one.
(show-paren-mode 1)

;; Transparently open compressed files. I like it when Emacs
;; transparently opens compressed files.
(auto-compression-mode t)

(require 'flymake)
(keymap-set flymake-mode-map "M-n" 'flymake-goto-next-error)
(keymap-set flymake-mode-map "M-p" 'flymake-goto-prev-error)

;; Spell check as I write (including comments & strings in code)
(add-hook 'text-mode-hook #'flyspell-mode)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)

;; Visual fill column mode / visual line mode. This is a great
;; combination. In particular it means that text is flowing
;; automatically, rather than me having to use the manual fill
;; commands. This is great for editing.
(defun sb/visual-fill-column-hook ()
  (setq visual-fill-column-center-text t)
  (setq visual-fill-column-width 90))

(add-hook 'visual-fill-column-mode-hook 'sb/visual-fill-column-hook)

(mapc (lambda (hook)
	(add-hook hook #'visual-fill-column-mode)
	(add-hook hook #'visual-line-mode))
      (quote (adoc-mode-hook
	      markdown-mode-hook
	      message-mode-hook
	      org-mode-hook)))

;; Line numbers are useful when remotely pairing.
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(require 'yasnippet)
(add-hook 'prog-mode-hook #'yas-minor-mode)


;; Automatically create JIRA links for things that looks like them.
;; For this I've adapted snippets from Alex ter Weele and Robin
;; Schroer.
(defun sb/bug-reference-setup ()
  (setq bug-reference-bug-regexp
        (rx
         (group
          (group
           ;; Jira projects have at least 2 characters, hence we call
           ;; any + one-or-more to avoid false positives.
	   symbol-start
           (any upper)
           (one-or-more upper)
           "-"
           (any "1-9") (zero-or-more digit)
	   symbol-end)))
        bug-reference-url-format "https://circleci.atlassian.net/browse/%s"))

(add-hook 'bug-reference-mode-hook 'sb/bug-reference-setup)
(add-hook 'text-mode-hook 'bug-reference-mode)
(add-hook 'prog-mode-hook 'bug-reference-mode)

(require 'company)
(global-company-mode)
(keymap-global-set "TAB" #'company-indent-or-complete-common)

(require 'direnv)
(add-hook 'after-init-hook #'direnv-mode)

(require 'marginalia)
(marginalia-mode)

(require 'orgalist)
(add-hook 'message-mode-hook #'orgalist-mode)
(add-hook 'git-commit-setup-hook #'orgalist-mode)
(add-hook 'markdown-mode-hook #'orgalist-mode)

(require 'subword)
(add-hook 'prog-mode-hook #'subword-mode)

(require 'whitespace)
(setq whitespace-style '(face empty tabs trailing))
(keymap-global-set "C-x w" #'whitespace-cleanup)
(global-whitespace-mode)

(winner-mode)
;;;; SmartParens

;; Structural editing is a must when editing lisp, and it has bled
;; into other aspects of programming for me. In particular the ability
;; to remove surrounding parens / quotes with `sp-unwrap-sexp' is
;; incredibly useful even in text mode.

(require 'smartparens-config)

(add-hook 'text-mode-hook #'turn-on-smartparens-mode)
(add-hook 'org-mode-hook #'turn-on-smartparens-strict-mode)
(add-hook 'prog-mode-hook #'turn-on-smartparens-strict-mode)
(add-hook 'cider-repl-mode-hook #'turn-on-smartparens-strict-mode)
(add-hook 'minibuffer-setup-hook #'turn-on-smartparens-strict-mode)

(keymap-set smartparens-mode-map "C-M-f" 'sp-forward-sexp)
(keymap-set smartparens-mode-map "C-M-b" 'sp-backward-sexp)

(keymap-set smartparens-mode-map "C-M-u" 'sp-backward-up-sexp)
(keymap-set smartparens-mode-map "C-s-d" 'sp-down-sexp)

(keymap-set smartparens-mode-map "C-<right>" 'sp-forward-slurp-sexp)
(keymap-set smartparens-mode-map "C-<left>" 'sp-forward-barf-sexp)

(keymap-set smartparens-mode-map "C-M-<left>" 'sp-backward-slurp-sexp)
(keymap-set smartparens-mode-map "C-M-<right>" 'sp-backward-barf-sexp)

(keymap-set smartparens-mode-map "C-M-t" 'sp-transpose-sexp)
(keymap-set smartparens-mode-map "C-M-k" 'sp-kill-sexp)

(keymap-set smartparens-mode-map "s-u" 'sp-unwrap-sexp)

;;;; Projectile

(require 'projectile)

;; Register project type for non-standard Clojure projects with weird
;; test file names. Override the project type in .dir-locals.el.
(projectile-register-project-type 'lein-legacy '("project.clj" ".projectile-lein-legacy")
                                  :project-file "project.clj"
                                  :compile "lein compile"
                                  :test "lein test"
                                  :test-prefix "test_")

(keymap-global-set "C-c p" #'projectile-command-map)

(setq projectile-create-missing-test-files 'projectile-find-file
      projectile-project-search-path '("~/src")
      projectile-switch-project-action 'projectile-find-file)

(add-hook 'after-init-hook #'projectile-mode)

;;;; Eglot

(require 'eglot)

(defun sb/maybe-format-buffer ()
  (when (eglot-managed-p)
    (eglot-format-buffer)))

(add-hook 'before-save-hook #'sb/maybe-format-buffer)

(setq eglot-confirm-server-initiated-edits nil)
;; Disable the timeout, as it occasionally triggers on startup for
;; newer clojure-lsp. This is due to some analysis pass, particularly
;; on the first time we open a project, taking a bit of time.
(setq eglot-connect-timeout nil)
(setq eglot-sync-connect nil)
(setq eglot-extend-to-xref t)

(keymap-set eglot-mode-map "C-c e r" 'eglot-rename)
(keymap-set eglot-mode-map "C-c C-l" 'eglot-code-actions)
(keymap-set eglot-mode-map "M-i" 'eglot-find-implementation)

(dolist (hook '(clojure-mode-hook
                dockerfile-mode-hook
                go-mode-hook
                json-mode-hook
                nix-mode-hook
                shell-mode-hook
                yaml-mode-hook))
  (add-hook hook #'eglot-ensure))

(require 'eglot-java)
(add-hook 'java-mode-hook 'eglot-java-mode)
(add-hook 'eglot-java-mode-hook (lambda ()
  (define-key eglot-java-mode-map (kbd "C-c l n") #'eglot-java-file-new)
  (define-key eglot-java-mode-map (kbd "C-c l x") #'eglot-java-run-main)
  (define-key eglot-java-mode-map (kbd "C-c l t") #'eglot-java-run-test)
  (define-key eglot-java-mode-map (kbd "C-c l N") #'eglot-java-project-new)
  (define-key eglot-java-mode-map (kbd "C-c l T") #'eglot-java-project-build-task)
  (define-key eglot-java-mode-map (kbd "C-c l R") #'eglot-java-project-build-refresh)))

(require 'org-superstar)
(add-hook 'org-mode-hook #'org-superstar-mode)

(require 'vertico)
(vertico-mode)
(setq vertico-resize t)

;;; Major Modes:

;; I'm not a fan, but some repos I interact with use Adoc and it's a
;; PITA to edit such documents without a helping hand.
(autoload 'adoc-mode "adoc-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.adoc\\'" . adoc-mode))


(require 'csv-mode)

;; Elfeed is an Emacs (RSS & Atom) feed reader. `org-elfeed' is an
;; extension that stores the feed config in `elfeed.org' rather than
;; `custom.el'.
(require 'elfeed)
(require 'elfeed-org)

(setq rmh-elfeed-org-files '("~/Documents/Elfeed/feeds.org"))
(add-hook 'after-init-hook #'elfeed-org)

(setq elfeed-db-directory "~/Documents/Elfeed/db")

(require 'go-mode)
(require 'json-mode)

(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
(setq markdown-fontify-code-blocks-natively t)
(setq markdown-asymmetric-header t)

(with-eval-after-load "markdown-mode"
  ;; I essentially don't use Markdown
  ;; outside GitHub any more
  (keymap-set markdown-mode-map "M-<up>" #'markdown-move-list-item-up)
  (keymap-set markdown-mode-map "M-<down>" #'markdown-move-list-item-down))

(require 'nix-mode)
(require 'nix-sandbox)


(require 'plantuml-mode)
(require 'ob-plantuml)
(add-to-list 'auto-mode-alist '("\\.puml\\'" . plantuml-mode))

(setq plantuml-indent-level 4
      plantuml-default-exec-mode 'executable
      plantuml-font-lock-keywords t
      org-plantuml-exec-mode 'plantuml)

(defun sb/no-tabs-in-plantuml ()
  (setq indent-tabs-mode nil))

(add-hook 'plantuml-mode-hook #'sb/no-tabs-in-plantuml)

(require 'protobuf-mode)
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))

(defun sulami/init-protobuf-imenu ()
  "Sets up imenu support for Protobuf.

Stolen from Spacemacs."
  (setq
   imenu-generic-expression
   '((nil "^[[:space:]]*\\(message\\|service\\|enum\\)[[:space:]]+\\([[:alnum:]]+\\)" 2))))

(add-hook 'protobuf-mode-hook #'sulami/init-protobuf-imenu)

(defun sulami/init-hugsql-imenu ()
  (when (string-suffix-p ".hug.sql" (buffer-file-name))
    (setq imenu-generic-expression
	  '((nil "^--[[:space:]]:name[[:space:]]+\\([[:alnum:]-]+\\)" 1)))))
(add-hook 'sql-mode-hook #'sulami/init-hugsql-imenu)

(require 'terraform-mode)
(require 'yaml-mode)


;;;; Verb

;; This is a package I use for interacting with REST HTTP APIs. Before
;; arriving at it I tried restclient, walkman, and http.el. All have
;; their strengths and weaknesses, but Verb seems the most
;; well-rounded feature set. The hierachical inheritance is what
;; really sold me, as it works really well with REST APIs to reduce
;; boilerplate.

(require 'verb)
(with-eval-after-load "org"
  (keymap-set org-mode-map "C-c C-r" verb-command-map))

(defun sb/api-token-for-host (host)
  "Return a token for the specified host."
  (let ((found (nth 0 (auth-source-search :host host :create nil))))
    (when found
      (let ((secret (plist-get found :secret)))
	(if (functionp secret)
	    (funcall secret)
	  secret)))))

;;;; Org

;; The Org manual expects the =C-c {l,a,c}= keybindings to be
;; available in any mode, so define them globally. I prefer to follow
;; conventions. It makes reading the manual and tutorials a lot
;; easier!

(keymap-global-set "C-c l" #'org-store-link)
(keymap-global-set "C-c a" #'org-agenda)
(keymap-global-set "C-c c" #'org-capture)



;; don't indent relative to headline
(setq org-adapt-indentation nil)

;; no initial indent in source code
(setq org-edit-src-content-indentation 0)

;; When hitting C-c C-z to take a note, always put it in the LOGBOOK drawer
(setq org-log-into-drawer t)
(setq org-catch-invisible-edits 'smart)

;; If running interactively, I want export to copy to the kill-ring
(setq org-export-copy-to-kill-ring 'if-interactive)

;; I don't normally  want section numbers in exports
(setq org-export-with-section-numbers nil)

;; Omit TOC unless I explicitly put it in
(setq org-export-with-toc nil)
(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

(setq org-table-header-line-p t)

(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)")
			  (sequence "PROJ(p)" "|" "COMPLETE")
			  (sequence "|" "CANCELLED")))

(setq org-stuck-projects '("-someday/PROJ" ("TODO" "WAITING") nil ""))

;; Allow refiling to sub-paths
(setq org-refile-use-outline-path 'file)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-refile-targets '((nil . (:todo . "PROJ"))
			   (nil . (:level . 1))
			   (nil . (:level . 2))))
(setq org-goto-interface 'outline-path-completion)
(setq org-outline-path-complete-in-steps nil)
(setq org-agenda-include-diary t)
(setq org-directory "~/Documents/Org")
(setq org-agenda-files "~/Documents/Org/Agenda/org-agenda-files.txt")

(setq org-agenda-clockreport-parameter-plist '(:link t :maxlevel 2 :fileskip0 t))
(setq org-agenda-tags-todo-honor-ignore-options t)
(setq org-agenda-todo-ignore-scheduled 'future)
(setq org-agenda-todo-ignore-deadlines 'far)
(setq org-agenda-todo-ignore-with-date 'future)
(setq org-agenda-todo-ignore-timestamp 'future)
(setq org-agenda-skip-deadline-prewarning-if-scheduled t)
(setq org-agenda-skip-timestamp-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)

(setq org-agenda-custom-commands
      '(("d" "Day Agenda" agenda ""
	 ((org-agenda-span 'day)
	  (org-agenda-tag-filter-preset '("-someday" "-inbox"))))
	("x" "E[x]tra tasks" tags-todo "-someday-inbox/TODO")
	("g" . "Getting Things Done")
	("gA" "Agenda minus recurring tasks"
	 agenda ""
	 ((org-agenda-tag-filter-preset '("-recurring"))))
	("gi" "Inbox" tags "inbox")
	("gs" "Someday"
	 ((todo "PROJ")
	  (tags-todo "-proj/TODO"))
	 ((org-agenda-tag-filter-preset '("+someday"))))
	("gc" "Review for Tasks Complete"
	 todo "TODO"
	 ((org-agenda-tag-filter-preset '("-someday" "-recurring" "-gtd"))))
	("gw" "Waiting tasks" todo "WAITING")
	("ga" "Archivable"
	 ((tags "-proj/DONE")
	  (tags "-proj/CANCELLED")))
	("gp" "Projects" tags-todo "-someday/PROJ")))

(setq org-default-notes-file
      (expand-file-name "Agenda/agenda.org" org-directory))

(setq org-capture-templates
      `(("d" "Do ASAP" entry (file "") "* TODO %?\nSCHEDULED: %t")
	("t" "TODO" entry (file "") "* TODO %? :inbox:")
	("l" "TODO with [l]ink" entry (file "") "* TODO %? :inbox:\ncf %a ")

	("w" "Weekly summary" plain
	 (file (lambda () (format-time-string "~/Documents/Org/weekly-summaries/%Y/%F.org")))
	 (file "weekly-summaries/template.org")
	 :immediate-finish t
         :jump-to-captured t)

	("b" "Things to Brag about!" entry
	 (file+olp+datetree "bragging.org")
	 "* %?"
	 :time-prompt t)

	("B" "Blog Post" plain
	 (function (lambda ()
		     (find-file (format "~/blog/content/posts/000-DRAFT-%#x.org"
					(time-convert nil 'integer)))))
	 (file "Templates/post.org"))

        ("r" "New GTD Review" plain
	 (file ,(format-time-string "GTD/%F.org"))
	 (file "GTD/review-template.org"))))

(org-babel-do-load-languages 'org-babel-load-languages
			     '((emacs-lisp . t)
			       (sql . t)
			       (shell . t)
			       (plantuml . t)))

;;;; Org Roam

(require 'org-roam)
(require 'org-roam-protocol)

(setq org-roam-directory (file-truename "~/Documents/Org"))
(setq org-roam-node-display-template
      (concat "${title:*} " (propertize "${tags:20}" 'face 'org-tag)))

(setq org-roam-capture-templates
      '(("d" "default" plain "%?" :target
	 (file+head "Roam/%<%Y%m%d%H%M%S>-${slug}.org"
		    "#+title: ${title}\n")
	 :unnarrowed t)))

(keymap-global-set "C-c n f" #'org-roam-node-find)
(keymap-global-set "C-c n r" #'org-roam-ref-add)

(keymap-set org-mode-map "C-c n i" #'org-roam-node-insert)
(keymap-set org-mode-map "C-c n t" #'org-roam-tag-add)

(add-hook 'after-init-hook #'org-roam-db-autosync-mode)

(defun sb/org-roam-node-random ()
  "Open a random Org Roam Node (ignoring Dailies)"
  (interactive)
  (org-roam-node-random nil
			(lambda (roam-node)
			  (not
			   (org-roam-dailies--daily-note-p
			    (org-roam-node-file roam-node))))))

(require 'org-roam-dailies)
(setq org-roam-dailies-directory "Daily/")
(keymap-global-set "C-c d" #'org-roam-dailies-map)

(require 'org-roam-ui)
(setq org-roam-ui-open-on-start nil)

;;;; Org Present

(require 'org-present)
(require 'visual-fill-column)

(setq org-present-hide-stars-in-headings nil)

(defun sb/org-present-start ()
  (org-display-inline-images)
  (org-present-read-only)
  (org-present-hide-cursor)

  ;; Bigger fonts when presenting
  (setq-local face-remapping-alist
	      '((default (:height 1.5) variable-pitch)
                (header-line (:height 4.0) variable-pitch)
                (org-document-title (:height 1.75) org-document-title)
                (org-code (:height 1.55) org-code)
                (org-verbatim (:height 1.55) org-verbatim)
                (org-block (:height 1.4) org-block)
                (org-block-begin-line (:height 0.7) org-block)))

  (flyspell-mode 0)

  ;; Set a blank header line string to create blank space at the top
  (setq header-line-format " ")

  (setq-local org-hide-emphasis-markers t))

(defun sb/org-present-quit ()
  (org-remove-inline-images)
  (org-present-read-write)
  (org-present-show-cursor)

  ;; Reset text sizes
  (setq-local face-remapping-alist
	      '((default variable-pitch default)))

  ;; Clear the header line string so that it isn't displayed
  (setq header-line-format nil)

  (setq-local org-present-hide-stars-in-headings nil)
  (setq-local org-hide-emphasis-markers nil))

(add-hook 'org-present-mode-hook 'sb/org-present-start)
(add-hook 'org-present-mode-quit-hook 'sb/org-present-quit)


;;; Clojure

;; The main programming language I use at work.

(require 'clojure-mode)

(setq clojure-align-style 'align-arguments)

(require 'cider)

;; Rely on LSP for eldoc and completions
(remove-hook 'eldoc-documentation-functions #'cider-eldoc)
(remove-hook 'completion-at-point-functions #'cider-complete-at-point)

;; Clear keybinding I never use for Cider, so we can use it for LSP
;; prefix.
(keymap-unset cider-mode-map "C-c C-l" t)

(setq cider-repl-display-help-banner nil)
(setq cider-repl-pop-to-buffer-on-connect nil)
(setq cider-repl-use-pretty-printing t)
(setq nrepl-log-messages nil)

;; Play nice with LSP
(setq cider-eldoc-display-for-symbol-at-point nil) ; use lsp
(setq cider-font-lock-dynamically nil) ; use lsp semantic tokens
(setq cider-prompt-for-symbol nil) ; use lsp
(setq cider-use-xref nil) ; use lsp

;;;; Kaocha-runner - lets me run test using Kaocha in CIDER.

(with-eval-after-load "clojure-mode"
  (require 'kaocha-runner)
  (keymap-set clojure-mode-map "C-c k t" 'kaocha-runner-run-test-at-point)
  (keymap-set clojure-mode-map "C-c k r" 'kaocha-runner-run-tests)
  (keymap-set clojure-mode-map "C-c k a" 'kaocha-runner-run-all-tests)
  (keymap-set clojure-mode-map "C-c k w" 'kaocha-runner-show-warnings)
  (keymap-set clojure-mode-map "C-c k h" 'kaocha-runner-hide-windows))

;;;; clj-refactor

;; Additional refactoring support. Much of it will probably be usurped
;; by clojure-lsp, but I still keep it around for its "magic
;; requires", which I very much appreciate.
(setq cljr-add-ns-to-blank-clj-files nil)
(setq cljr-auto-clean-ns nil)
(setq cljr-auto-sort-ns nil)
(setq cljr-eagerly-build-asts-on-startup nil)
(setq cljr-warn-on-eval nil)

(with-eval-after-load "clojure-mode"
  (with-eval-after-load "yasnippet"
    (require 'clj-refactor)

    (add-to-list 'cljr-magic-require-namespaces '("s" . "clojure.spec.alpha"))
    (add-to-list 'cljr-magic-require-namespaces '("gen" . "clojure.spec.gen.alpha"))
    (add-to-list 'cljr-magic-require-namespaces '("m" . "malli.core"))
    (add-to-list 'cljr-magic-require-namespaces '("mg" . "malli.generator"))

    (keymap-set clojure-mode-map "C-c C-r h" #'hydra-cljr-help-menu/body)
    (add-hook 'clojure-mode-hook #'clj-refactor-mode)))

;;;; Jarchive -- lets me open project dependencies inside jar archives

(require 'jarchive)
(add-hook 'clojure-mode-hook #'jarchive-setup)


;;;; Carve -- allows me to find dead code

;; Unfortunately carve.el is not available on melpa/nix, so I had to
;; download it locally and load it.

(load "~/src/dotfiles/emacs/carve.el")

;;; Notmuch

;; I like to use Emacs for all writing, including email. I don't want
;; a separate program to read and write emails, so it follows I must
;; read email in Emacs too. I use Notmuch for this.


(require 'notmuch)
(add-hook 'notmuch-message-mode-hook #'turn-off-auto-fill)

(setq notmuch-multipart/alternative-discouraged '("text/x-amp-html" "text/plain" "text/html"))
(setq notmuch-search-oldest-first nil)
(setq notmuch-hello-thousands-separator ",")
(setq notmuch-archive-tags (list "-inbox" "+archived"))
(setq notmuch-mua-cite-function (quote message-cite-original-without-signature))
(setq notmuch-fcc-dirs '(("sonar_columns_0n@icloud.com" . "icloud/Sent +sent")
			 ("sonar_columns_0n@icloud.com" . "work/Sent +sent")))
(setq notmuch-saved-searches
      (quote
       ((:name "Personal Inbox" :query "tag:personal and tag:inbox and not tag:builds" :key "p" :search-type tree)
	(:name "CCI Inbox" :query "tag:work and tag:inbox and not tag:builds" :key "c" :search-type tree)
	(:name "Builds" :query "tag:builds" :key "b")
	(:name "Flagged" :query "tag:flagged" :key "f" :search-type tree)
	(:name "Waiting For" :query "tag:waiting_for" :key "w" :search-type tree)
	(:name "Drafts" :query "tag:draft" :key "d")
	(:name "Recent" :query "date:7d.. and not tag:lists" :key "r" :search-type tree))))

(setq notmuch-tagging-keys
      (quote
       (("a" notmuch-archive-tags "Archive")
	("u" notmuch-show-mark-read-tags "Mark read")
	("f" ("+flagged") "Flag")
	("s" ("+spam" "-inbox") "Mark as spam")
	("d" ("+deleted" "-inbox") "Delete")
	("w" ("-inbox" "+waiting_for") "Waiting For"))))

;; Sending mail with MSMTP

(setq sendmail-program "msmtp"
      message-send-mail-function 'message-send-mail-with-sendmail
      message-kill-buffer-on-exit t
      message-directory "~/Maildir"
      message-sendmail-envelope-from 'header
      mail-envelope-from 'header
      mail-specify-envelope-from t)

;; I don't like using my email inbox as a todo list. When I receive an
;; email I need to act on but can't yet for some reason, I link to it
;; from my Org mode agenda and archive it. When Org agenda prompts me
;; I can click on the link and immediately get to the mail in my
;; archive, and can reply to it from there.

(require 'ol-notmuch)
(require 'org-mime)
