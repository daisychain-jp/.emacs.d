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

(defun mail-simple-send (subject body)
  "Default function to send a mail with SUBJECT and BODY to myself."
  (save-excursion
    (message-mail "tinamo@yahoo.co.jp" subject)
    (message-goto-body)
    (insert body)
    (message-send-and-exit)))

(defun mail-buffer ()
  "Send the current buffer contents as a email.

If region is active, send the string in the region instead."
  (interactive)
  (let ((mail-subj (buffer-name))
        (mail-body (if (use-region-p)
                       (buffer-substring (region-beginning) (region-end))
                     (buffer-string))))
    (mail-simple-send mail-subj mail-body)))
