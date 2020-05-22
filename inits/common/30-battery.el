(use-package battery
  :config
  (run-with-timer 0 60
                  (lambda ()
                    (when (<= (battery-percentage) battery-load-critical)
                      (save-buffers-kill-emacs t)))))

(defun battery-percentage ()
  "Return percentage of load battery."
  (let ((data (and battery-status-function (funcall battery-status-function))))
    (car (read-from-string (cdr (assq ?p data))))))
