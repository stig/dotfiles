;; Turn on Clojure refactoring minor mode
(add-hook 'clojure-mode-hook (lambda ()                             
                               (clj-refactor-mode 1)
                               (cljr-add-keybindings-with-prefix "C-c C-m")
                               (define-key clj-refactor-map (kbd "C-x C-r") 'cljr-rename-file)
                               (define-key clojure-mode-map (kbd "s-j") 'clj-jump-to-other-file)))


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


(provide 'setup-clojure-mode)
