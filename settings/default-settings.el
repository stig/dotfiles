(ido-mode t)


;; Enable syntax highlighting for older Emacsen that have it off
(global-font-lock-mode t)

;; Undo/redo window configuration with C-c <left>/<right>
(winner-mode 1)


;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)






 ;; Show keystrokes in progress
 echo-keystrokes 0.1

 ;; Always display line and column numbers
 line-number-mode t
 column-number-mode t



 ;; I don't want my computer beeping at me, thank you very much.
 visible-bell nil

 ;; Real emacs knights don't use shift to mark things
 shift-select-mode nil)


(global-set-key (kbd "s-c") 'comment-or-uncomment-region-or-line)

(global-set-key (kbd "C-x C-c") 'save-buffers-kill-emacs)

(global-set-key (kbd "C-x M-d") 'insert-current-date)

(setq graphviz-dot-view-command "open -a Graphviz %s")

(setq ediff-autostore-merges t)

(provide 'default-settings)
