(use-package recentf
  :custom
  (recentf-save-file (expand-file-name ".recentf"
                                       user-emacs-directory))
  (recentf-exclude '("\/var\/lib\/junk\/"
                     "\/mnt\/svr01\/"))
  (recentf-max-saved-items 2000)
  (recentf-auto-cleanup 600)
  :config
  (recentf-load-list)
  (recentf-mode 1)
  (run-with-idle-timer 3600 t #'recentf-save-list))
