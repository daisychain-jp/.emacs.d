(el-get-bundle recentf-ext)

(require 'recentf-ext)

(use-package recentf-ext
  :config
  (setq recentf-save-file (concat env-emacs-data-dir "/recentf"))
  (setq recentf-exclude '("\/data\/emacs\/junk\/"  ; open-junk-file
                          "\/mnt\/svr01\/"))
  (setq recentf-max-saved-items 2000)
  (setq recentf-auto-cleanup 'never)
  (recentf-load-list)
  (recentf-mode t)
  (run-with-idle-timer 30 t '(lambda () (with-suppressed-message (recentf-save-list))))
  )

(defmacro with-suppressed-message (&rest body)
  "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
  (declare (indent 0))
  (let ((message-log-max nil))
    `(with-temp-message (or (current-message) "") ,@body)))

(run-at-time 3600 3600 #'recentf-cleanup)
