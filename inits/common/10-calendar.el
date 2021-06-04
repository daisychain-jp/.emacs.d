(use-package calendar
  :bind (:map calendar-mode-map
              ("v" . calendar-org-archive-show-summary-day))
  :init
  (defface calendar-buffer `((t . (:font "fontset-default"
                                         :height ,(my-adjust-font-size 400))))
    "Default face for calendar mode."
    :group 'calendar-faces)
  :hook (calendar-mode . (lambda ()
                           (buffer-face-set 'calendar-buffer))))

(defun calendar-org-archive-show-summary-day ()
  "Open a new window attached to the calendar view \
and show summary of the day which the cursor is pointing to in the calendar view."
  (interactive)
  (let* ((buf-name "Summary of the Day")
         (org-ql-view-buffer buf-name)
         (display-buffer-alist
          `((,buf-name
             (display-buffer-reuse-window display-buffer-at-bottom))))
         (calendar-date-display-form calendar-iso-date-display-form)
         (date (calendar-date-string (calendar-cursor-to-date)))
         (d (parse-time-string date))
         (year (decoded-time-year d))
         (files (org-record-file year)))
    (org-ql-search files `(and (parent (heading ,date))
                               (or (tags "journal")
                                   (tags-expanded "AC_CRAFT")
                                   (and (tags "ac_read")
                                        (not (tags "web")))
                                   (tags "ac_cook")
                                   (tags "ac_purchase")
                                   (tags "ac_solve"))))
    (other-window 1)))

;; include japanese holidays
(use-package japanese-holidays
  :straight t
  :config
  (setq calendar-holidays
        (append japanese-holidays holiday-local-holidays holiday-other-holidays))
  (setq mark-holidays-in-calendar t))
