;; Turn on Clojure refactoring minor mode
(add-hook 'clojure-mode-hook
          (lambda ()
            (clj-refactor-mode 1)
            (cljr-add-keybindings-with-prefix "C-c r")
            (define-key clojure-mode-map (kbd "C-c o") 'clj-jump-to-other-file)))

(setq nrepl-hide-special-buffers t)

;;(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)


(provide 'clojure-settings)
