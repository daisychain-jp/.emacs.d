(use-package calendar
  :bind (:map calendar-mode-map
              ("v" . calendar-org-record-show-date))
  :hook (calendar-mode . (lambda ()
                           (buffer-face-set 'calendar)))
  :config
  (defun calendar-org-record-show-date ()
    "docstring"
    (interactive)
    (let* ((display-buffer-alist
            '(("\\*Org QL View\\*"
               (display-buffer-reuse-window display-buffer-at-bottom)
               )))
           (calendar-date-display-form calendar-iso-date-display-form)
           (date (calendar-date-string (calendar-cursor-to-date)))
           (d (parse-time-string date))
           (year (decoded-time-year d))
           (files (org-record-file year)))
      (org-ql-search files `(and (parent (heading ,date))
                                 (or (tags "journal")
                                     (tags "ac_make"))))
      (other-window 1))))

;; include japanese holidays
(use-package japanese-holidays
  :straight t
  :config
  (setq calendar-holidays
        (append japanese-holidays holiday-local-holidays holiday-other-holidays))
  (setq mark-holidays-in-calendar t))
