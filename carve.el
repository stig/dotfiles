;;; carve.el --- emacs integration with borkdude/carve

;; Copyright © 2023 Oliver Hine
;;
;; Author: Oliver Hine

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; https://github.com/oliyh/carve.el

;;; Code:
(require 'projectile)

(defgroup carve nil
  "Settings for carve"
  :group 'tools)

(defcustom carve-command "carve"
  "*The carve command to run"
  :type 'string
  :group 'carve)

(defun carve--kill-buffer ()
  (interactive)
  (kill-buffer))

(defun carve--add-to-ignore ()
  (interactive)
  (unless (get-text-property (point) 'carve-ignored)
    (let* ((carve-symbol (get-text-property (point) 'carve-symbol))
           (project-root (projectile-project-root))
           (carve-directory (concat project-root ".carve/"))
           (carve-ignore-file (concat carve-directory "ignore"))
           (inhibit-read-only t))
      (print (concat "Adding " carve-symbol " to " carve-ignore-file))
      (add-text-properties (line-beginning-position) (line-end-position) (list 'font-lock-face (cons 'foreground-color "darkgray")
                                                                               'carve-ignored t))
      (unless (file-exists-p carve-directory)
        (make-directory carve-directory))
      (let ((buf (find-file-noselect carve-ignore-file)))
        (with-current-buffer buf
          (goto-char (point-max))
          (insert "\n")
          (insert carve-symbol)
          (save-buffer))))))

(defun carve--re-run-last-command ()
  (interactive)
  (let ((project-root (get-text-property (point-min) 'project-root))
        (carve-opts (get-text-property (point-min) 'carve-opts)))
    (carve--run-carve project-root carve-opts)))

(defvar carve-mode-map nil
  "Keymap for `carve-mode'.")

(unless carve-mode-map
  (setq carve-mode-map (make-sparse-keymap))
  (define-key carve-mode-map "q" 'carve--kill-buffer)
  (define-key carve-mode-map "i" 'carve--add-to-ignore)
  (define-key carve-mode-map "g" 'carve--re-run-last-command))

(defun carve-mode ()
  "Minor mode for viewing carve output.
\\{carve-mode-map}"
  (interactive)
  (setq minor-mode 'carve-mode
        mode-name "Carve")
  (use-local-map carve-mode-map))

(defun carve--walk-output ()
  (interactive)
  (let (prop-alist)
    (goto-char (point-min))
    (forward-line 2)
    (while (looking-at "\\([^ ]+\\) \\(.+\\)$")
      (let ((file-path (match-string 1))
	    (carved-symbol (match-string 2)))
        (add-text-properties (line-beginning-position) (line-end-position) (list 'carve-symbol carved-symbol))
	(when (assoc carved-symbol prop-alist)
	  (error "Duplicated property %s" carved-symbol))
	(setq prop-alist (cons (cons file-path carved-symbol) prop-alist)))
      (forward-line 1))
    prop-alist))

(defun carve--run-carve (project-root carve-opts)
  (let ((buf (get-buffer-create "*carve-output*"))
        (inhibit-read-only t)
        (command (string-join (cons carve-command carve-opts) " ")))
    (with-current-buffer buf
      (erase-buffer)
      (setq default-directory project-root) ;; run carve process in correct directory
      (select-window (display-buffer buf))
      (print (concat "Running: " command))

      ;; put command that was run on the first line
      (goto-char (point-min))
      (insert command)
      (add-text-properties (line-beginning-position) (line-end-position) (list 'project-root project-root
                                                                               'carve-opts carve-opts))
      (insert "\n\n")
      (let ((process (apply 'start-file-process "carve" buf carve-command carve-opts)))
        (while (accept-process-output process)))

      (carve--walk-output)
      (grep-mode) ;; enables file linking, q for quit
      (carve-mode) ;; for custom actions

      ;; leave cursor on first entry
      (goto-char (point-min))
      (forward-line 2))))

(defun carve-project ()
  (interactive)
  (let* ((current-file-directory (file-name-directory buffer-file-name))
         (project-root (projectile-project-root))
         (has-config (file-exists-p (concat project-root ".carve/config.edn")))
         (carve-opts (if has-config
                         (list "--opts")
                       (list "--opts" "{:paths [\"src\" \"test\"] :report {:format :text}}"))))
    (carve--run-carve project-root carve-opts)))

(defun carve-ns ()
  (interactive)
  (let* ((project-root (projectile-project-root))
         (project-file-path (file-relative-name buffer-file-name project-root))
         (carve-opts (list "--opts" (concat "{:paths [\"" project-file-path "\"] :report {:format :text}}"))))
    (carve--run-carve project-root carve-opts)))

(global-set-key (kbd "C-c C-c p") 'carve-project)
(global-set-key (kbd "C-c C-c n") 'carve-ns)

;; todo
;; - check for existence of src / test and use them if present
;; - key which will delete the form it refers to?

;;; carve.el ends here
