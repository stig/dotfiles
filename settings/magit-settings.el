(require 'magit)
(require 'magit-gitflow)

(setq git-commit-summary-max-length 65)
(setq magit-push-always-verify nil)

(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)

(require 'magit-gh-pulls)
(add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)

(setq magit-git-executable "/usr/bin/git")

(global-set-key (kbd "M-m") 'magit-status)

(provide 'magit-settings)
