(use-package battery
  :config
  (run-with-timer 0 60
                  (lambda ()
                    (let* ((data (and battery-status-function (funcall battery-status-function)))
                           (percentage (car (read-from-string (cdr (assq ?p data))))))
                      (when (<= percentage battery-load-critical)
                        (save-buffers-kill-emacs t))))))
