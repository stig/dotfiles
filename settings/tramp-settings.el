;; Fix to allow editing remote files over ssh
(put 'temporary-file-directory 'standard-value '((file-name-as-directory "/tmp")))

;; Allow ssh+sudo with tramp
(set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:"))))

;; SSH is faster than the default scp mode (apparently)
(setq tramp-default-method "ssh")

(provide 'tramp-settings)
