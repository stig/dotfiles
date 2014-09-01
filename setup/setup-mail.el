(setq
 send-mail-function 'smtpmail-send-it
 message-send-mail-function 'smtpmail-send-it
 user-mail-address "stig.brautaset@icloud.com"
 user-full-name "Stig Brautaset"
 smtpmail-starttls-credentials '(("smtp.mail.me.com" 587 nil nil))
 smtpmail-auth-credentials  (expand-file-name "~/.authinfo")
 smtpmail-default-smtp-server "smtp.mail.me.com"
 smtpmail-smtp-server "smtp.mail.me.com"
 smtpmail-smtp-service 587
 smtpmail-debug-info t
 starttls-extra-arguments nil
 starttls-gnutls-program (executable-find "gnutls-cli")
 smtpmail-warn-about-unknown-extensions t
 starttls-use-gnutls t)

(provide 'setup-mail)
