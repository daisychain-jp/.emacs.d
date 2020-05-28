;;; org-record

(defun org-record-file (&optional year)
  "Return a path of record file.
If optional argument 'YEAR passed, a file which contains the year's tree is used instead of this year's one.."
  (let* ((record-year (if year year (ts-year (ts-now))))
         (record-file (format "%s/record/%s_record.org" env-doc-dir record-year)))
    (if (or (file-exists-p record-file)
            (file-symlink-p record-file))
        record-file
      nil)))
(defvar org-record-file (org-record-file))
(defun org-record-files ()
  "Return list of record files."
  (append (sort (file-expand-wildcards (format "%s/record/*_record.org" env-doc-dir)) 'string<)
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
         (year (ts-year ts)))
    (org-refile-to-datetree-using-ts-in-entry 'latest (org-record-file year))
    (org-save-all-org-buffers)
    (setq this-command 'org-record-subtree)))
(defun org-agenda-record-subtree ()
  "Refile the entry or subtree belonging to the current agenda entry."
  (interactive)
  (org-agenda-archive-with 'org-record-subtree))
(defun org-record-search (query)
  "Search org entries matched QUERY in record files using `org-ql-search'."
  (interactive (list (read-string "Query: ")))
  (let ((files (org-record-file)))
    (org-ql-search files query)))
(bind-keys :map org-agenda-mode-map
           ("$" . org-agenda-record-subtree)
           ("&" . org-agenda-ref-id-tieup-tree)
           :map global-map
           ("C-c R" . org-record-search))
(push '("Record entries" . org-record-search)
      org-ql-views)
