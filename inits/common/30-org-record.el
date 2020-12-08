;;; org-record

(defun org-record-file (&optional year)
  "Return a path of record file.

If optional argument `YEAR' is passed that year's file is returned instead of current year's."
  (let* ((record-year (or year (ts-year (ts-now))))
         (record-file-cand (format "%s/archive/record_%s.org" env-org-dir record-year))
         (record-file
          (if (file-exists-p record-file-cand)
              record-file-cand
            (expand-file-name "archive/record_0000.org" env-org-dir))))
    (if (or (file-exists-p record-file)
            (file-symlink-p record-file))
        record-file
      nil)))
(defvar org-record-file (org-record-file))
(defun org-record-files ()
  "Return list of record files."
  (append (sort (file-expand-wildcards (format "%s/archive/record_*.org" env-org-dir)) 'string<)
          org-agenda-files-default))
(defvar org-record-files (org-record-files))
(defun org-record-find-date (date)
  "Find and visit the location of DATE in record file.

DATE must be a string representing the date to find and parsable with `format-time-string'.

If called interactively, it prompt the user to select the date to find."
  (interactive
   (cond
    ((eq major-mode 'calendar-mode)
     (list (calendar-date-string (calendar-cursor-to-date))))
    ((eq major-mode 'org-agenda-mode)
     (let* ((day (or (get-text-property (min (1- (point-max)) (point)) 'day)
                     (user-error "Don't know which date to open in calendar")))
            (date (calendar-gregorian-from-absolute day)))
       (list (calendar-date-string date))))
    (t (let ((date-select (org-read-date)))
         (list date-select)))))
  (let* ((d (parse-time-string date))
         (day (decoded-time-day d))
         (month (decoded-time-month d))
         (year (decoded-time-year d)))
    (find-file (org-record-file year))
    (org-datetree-find-iso-week-create `(,month ,day ,year))))
(defun org-record-subtree ()
  "Refile current subtree to record file using latest timestamp."
  (interactive)
  (let* ((ts (car (sort (org-timestamps-in-entry) #'ts>)))
         (year (ts-year (or ts (ts-now))))
         (archive-file (if (and (member "scrap" (org-get-local-tags))
                                (not (member (org-get-todo-state) '("DN" "PD"))))
                           (format "%s/archive/scrap_%s.org" env-org-dir year)
                         (org-record-file year))))
    (org-refile-to-datetree-using-ts-in-entry 'latest archive-file)
    (org-save-all-org-buffers)
    (setq this-command 'org-record-subtree)))
(defun org-agenda-record-subtree ()
  "Refile the entry or subtree belonging to the current agenda entry."
  (interactive)
  (org-agenda-archive-with 'org-record-subtree))
(defun org-ql-view-record-subtree ()
  "Refile the entry or subtree belonging to the current agenda entry."
  (interactive)
  (org-agenda-archive-with 'org-record-subtree)
  (org-ql-view-refresh))
(defun org-record-search (query)
  "Search org entries matched QUERY in record files using `org-ql-search'."
  (interactive (list (read-string "Query: ")))
  (let ((files (org-record-files)))
    (org-ql-search files query)))
(bind-keys :map org-agenda-mode-map
           ("$" . org-agenda-record-subtree)
           ("&" . org-agenda-ref-id-tieup-tree)
           ("C" . org-record-find-date)
           :map org-ql-view-map
           ("$" . org-ql-view-record-subtree)
           ("&" . org-agenda-ref-id-tieup-tree)
           :map calendar-mode-map
           ("C" . org-record-find-date))
(push '("Record entries" . org-record-search)
      org-ql-views)
