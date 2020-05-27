(use-package battery
  :config
  (run-with-timer 0 60
                  (lambda ()
                    (let* ((data (and battery-status-function (funcall battery-status-function)))
                           (percentage (car (read-from-string (cdr (assq ?p data)))))
                           (power (cdr (assq ?L data))))
                      (when (and (<= percentage battery-load-critical)
                                 (string= power "BAT"))
                        (save-buffers-kill-emacs t))))))
