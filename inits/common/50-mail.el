(use-package smtpmail
  :init
  (custom-set-variables
   '(smtpmail-default-smtp-server "smtp.daisychain.jp"))
  :custom
  (smtpmail-smtp-service        465)
  (smtpmail-local-domain        "daisychain.jp")
  (smtpmail-smtp-user           "t.inamori")
  (smtpmail-stream-type         'ssl)
  (smtpmail-mail-address        "t.inamori@daisychain.jp"))

(defun mail-buffer ()
  "Send the current buffer contents as a email.

If region is active, send the string in the region instead."
  (interactive)
  (let ((buf-to-send (current-buffer)))
    (message-mail "tinamo@yahoo.co.jp" (buffer-name buf-to-send))
    (message-goto-body)
    (insert (save-excursion
              (with-current-buffer buf-to-send
                (if (use-region-p)
                    (buffer-substring (region-beginning) (region-end))
                  (buffer-string)))))
    (message-send-and-exit)))
