(require 'magit)
(require 'magit-gitflow)



(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)

(require 'magit-gh-pulls)
(add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)




(setq magit-diff-refine-hunk 'all)

(provide 'magit-settings)
