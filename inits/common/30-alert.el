(use-package alert
  :straight t
  :config
  (alert-add-rule :status '(buried)
                  :mode   'eshell-mode
                  :style  'fringe)
  (alert-add-rule :status '(buried)
                  :mode   'org-mode
                  :style  'libnotify))

(defvar alarm-sound (expand-file-name "~/afile/cd/special/levelup.mp3"))
(alert-define-style 'alarm
                    :title "alarm"
                    :notifier
                    (lambda (info)
                      (let ((message (plist-get info :message))
                            (severity (plist-get info :severity))
                            (title (plist-get info :title))
                            (icon (plist-get info :icon))
                            (category (plist-get info :category))
                            (buffer (plist-get info :buffer))
                            (mode (plist-get info :mode))
                            (data (plist-get info :data))
                            (style (plist-get info :style))
                            (persistent (plist-get info :persistent))
                            (id (plist-get info :id))
                            (system-volume (string-trim (shell-command-to-string "volget"))))
                        (if (string= system-volume "0")
                            (alert message :severity severity :title title :icon icon :category category :buffer buffer :mode mode :data data :style 'fringe :persistent persistent :id id)
                          (start-process-shell-command "system-alarm" nil
                                                       (mapconcat #'shell-quote-argument
                                                                  (list "mpg321" alarm-sound)
                                                                  " "))))))
