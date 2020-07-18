(use-package battery
  :config
  (run-with-timer 60 60 #'kill-emacs-auto))

(defun kill-emacs-auto ()
  "Kill Emacs if remaining power is under low level."
  (let* ((data (and battery-status-function (funcall battery-status-function)))
         (percentage (car (read-from-string (cdr (assq ?p data)))))
         (power (cdr (assq ?L data))))
    (when (and (< percentage battery-load-low)
               (string= power "BAT"))
      (save-buffers-kill-emacs t))))
