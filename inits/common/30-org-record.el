;;; org-record

(defun org-record-file (&optional year)
  "Return a path of record file.

If optional argument `YEAR' is passed that year's file is returned instead of current year's."
  (let* ((record-year (or year (ts-year (ts-now))))
         (record-file (format "%s/archive/record_%s.org" env-doc-dir record-year)))
    (if (or (file-exists-p record-file)
            (file-symlink-p record-file))
        record-file
      nil)))
(defvar org-record-file (org-record-file))
(defun org-record-files ()
  "Return list of record files."
  (append (sort (file-expand-wildcards (format "%s/archive/record_*.org" env-doc-dir)) 'string<)
          org-agenda-files-default))
(defvar org-record-files (org-record-files))
(defun org-record-find-date (date)
  "Find the location of DATE in record file.

If this called interactively, find today's entry of the datetree."
  (interactive (let ((date-select (org-read-date)))
                 (list date-select)))
  (let* ((d (parse-time-string date))
         (day (nth 3 d))
         (month (nth 4 d))
         (year (nth 5 d)))
    (find-file (org-record-file year))
    (org-datetree-find-iso-week-create `(,month ,day ,year))))
(defun org-record-subtree ()
  "Refile current subtree to record file using latest timestamp."
  (interactive)
  (let* ((ts (car (sort (org-timestamps-in-entry) #'ts>)))
         (year (ts-year (or ts (ts-now))))
         (archive-file (if (and (member "scrap" (org-get-local-tags))
                                (not (member (org-get-todo-state) '("DN" "PD"))))
                           (format "%s/archive/scrap_%s.org" env-doc-dir year)
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
           :map org-ql-view-map
           ("$" . org-ql-view-record-subtree)
           ("&" . org-agenda-ref-id-tieup-tree)
           :map global-map
           ("C-c R" . org-record-search))
(push '("Record entries" . org-record-search)
      org-ql-views)
