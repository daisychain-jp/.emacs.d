(defun mail-simple-send (mailto subject body)
  "Default function to send a mail with SUBJECT and BODY to myself."
  (save-excursion
    (let ((message-kill-buffer-on-exit t))
      (message-mail mailto subject)
      (message-goto-body)
      (insert body)
      (message-send-and-exit))))

(defun mail-buffer (mailto)
  "Send the current buffer contents as a email.

If region is active, send the string in the region instead."
  (let ((mail-subj (buffer-name))
        (mail-body (if (use-region-p)
                       (buffer-substring (region-beginning) (region-end))
                     (buffer-string))))
    (mail-simple-send mailto mail-subj mail-body)))
