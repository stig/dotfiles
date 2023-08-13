(tool-bar-mode -1)
(setq inhibit-startup-screen t)

;; I abhor Emacs beeping at me, and prefer a visual bell.
(setq visible-bell t)

;; Don't attempt to enable packages at startup; I install them with
;; Nix, so they are added to load-path already.
(setq package-enable-at-startup nil)
