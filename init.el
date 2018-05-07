(require 'package)

(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
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

;;    (add-to-list 'load-path (expand-file-name "~/play/org-mode/lisp"))

    (let ((dotfiles-dir (file-name-directory (or (buffer-file-name)
						 load-file-name))))
      (mapc #'org-babel-load-file
	    (remove (concat dotfiles-dir "init.org")
		    (directory-files dotfiles-dir t "\\w+\\.org$"))))
