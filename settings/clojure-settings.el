(require 'clj-refactor)
(require 'cljr-helm)
(require 'clojure-mode-extra-font-locking)

(dolist (mapping '(("route" . "compojure.route")
                   ("timbre" . "taoensso.timbre")
                   ("component" . "com.stuartsierra.component")
                   ("d" . "datomic.api")))
  (add-to-list 'cljr-magic-require-namespaces mapping t))

(add-hook 'clojure-mode-hook
          (lambda ()
            (clj-refactor-mode 1)
            (define-key clojure-mode-map (kbd "C-c o") 'clj-jump-to-other-file)
            (define-key clojure-mode-map (kbd "C-c r") 'cljr-helm)))

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(setq cider-repl-result-prefix ";; => ")

(provide 'clojure-settings)
