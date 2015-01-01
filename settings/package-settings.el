(require 'package)

(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/") t)

(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Specific packages we want to load from specific repos - otherwise
;; the latest version is used, regardless of which repo it comes from.

(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(clj-refactor . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(clojure-mode . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(helm . "melpa-stable") t)

(unless (file-exists-p "~/.emacs.d/elpa/archives/melpa")
  (package-refresh-contents))

(defun package-install? (package)
  (when (not (package-installed-p package))
    (package-install package)))

(package-install? 'aggressive-indent)
(package-install? 'align-cljlet)
(package-install? 'cider)
(package-install? 'clj-refactor)
(package-install? 'clojure-mode)
(package-install? 'company)
(package-install? 'graphviz-dot-mode)
(package-install? 'helm)
(package-install? 'helm-git-grep)
(package-install? 'wsd-mode)
(package-install? 'jekyll-modes)
(package-install? 'markdown-mode)
(package-install? 'multiple-cursors)
(package-install? 'paredit)
(package-install? 'sane-term)
(package-install? 'smartparens)
(package-install? 'windsize)

(provide 'package-settings)
