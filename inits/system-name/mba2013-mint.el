;; ding
(setq visible-bell nil)

(defun system-alarm ()
  (let* ((vol (string-trim (shell-command-to-string "source ~/.bash_aliases && volget"))))
    (if (string= vol "0")
        (let ((visible-bell t))
          (beep))
      (start-process-shell-command "system-alarm" nil (format "mpg321 %s" gtd-finish-sound)))))

(defvar env-temp-dir "~/tmp/" "Directory for temporary files")
