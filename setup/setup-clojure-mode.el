;; Turn on Clojure refactoring minor mode
(add-hook 'clojure-mode-hook (lambda ()                             
                               (clj-refactor-mode 1)
                               (cljr-add-keybindings-with-prefix "C-c C-m")
                               (define-key clj-refactor-map (kbd "C-x C-r") 'cljr-rename-file)
                               (define-key clojure-mode-map (kbd "s-j") 'clj-jump-to-other-file)))




(provide 'setup-clojure-mode)
