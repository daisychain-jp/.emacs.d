;; include japanese holidays
(el-get-bundle japanese-holidays)
(use-package japanese-holidays
  :config
  (setq calendar-holidays
        (append japanese-holidays holiday-local-holidays holiday-other-holidays))
  (setq mark-holidays-in-calendar t))

(add-hook 'calendar-mode-hook
          (lambda ()
            (buffer-face-set 'calendar)))
