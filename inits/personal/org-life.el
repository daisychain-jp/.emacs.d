(setq org-agenda-start-on-weekday 1)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-include-diary t)
(setq org-agenda-custom-commands
      '(("a" "Week-agenda"
         agenda ""
         ((org-agenda-skip-function
           (lambda ()
             ;; skip entry which has 'scrap' tag even if it has deadline
             (and (save-excursion
                    (let ((tags (org-get-tags)))
                      (member "scrap" tags)))
                  (progn (outline-next-heading) (point)))))))
        ("r" . "Search for all record files")
        ("rs" "Entries containing search words entry or headline."
         search ""
         ((org-agenda-files org-record-files)
          (org-agenda-sorting-strategy '(time-down))))
        ("rm" "Match a TAGS/PROP/TODO query in record file"
         tags ""
         ((org-agenda-files org-record-files)
          (org-agenda-sorting-strategy '(time-down))))
        ("o" "Someday entries"
         ((org-ql-search-block '(and (todo "SD")
                                     (tags "ac_make"))
                               ((org-ql-block-header "Someday to make")))
          (org-ql-search-block '(and (todo "SD")
                                     (tags "ac_purchase"))
                               ((org-ql-block-header "Someday to purchase")))
          (org-ql-search-block '(and (todo "SD")
                                     (tags "ac_cook"))
                               ((org-ql-block-header "Someday to cook")))
          (org-ql-search-block '(and (todo "SD")
                                     (not (tags "ac_purchase" "ac_cook" "ac_make")))
                               ((org-ql-block-header "Someday things"))))
         ((org-agenda-files org-record-files)))
        ("l" "Log entries in a week"
         agenda ""
         ((org-agenda-span 'week)
          (org-agenda-start-with-log-mode t)
          (org-agenda-include-inactive-timestamps nil)
          (org-agenda-include-diary t)
          (org-agenda-sorting-strategy
           '(time-up
             deadline-up
             todo-state-up
             priority-down))))
        ("L" "Log entry timeline on today with default org-agenda-prefix-format"
         agenda ""
         ((org-agenda-prefix-format (eval (car (get 'org-agenda-prefix-format 'standard-value))))
          (org-agenda-span 'week)
          (org-agenda-start-with-log-mode t)
          (org-agenda-include-inactive-timestamps nil)
          (org-agenda-include-diary t)
          (org-agenda-sorting-strategy
           '(time-up
             deadline-up
             todo-state-up
             priority-down))))
        ("e" . "Effort table")
        ("ei" "of doing task"
         tags "+Effort=>\"0\"/UG|IP"
         ((org-agenda-overriding-header "Today's Task")
          (org-local-columns-format "%26ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM(Time){:}")
          (org-agenda-view-columns-initially t)
          (org-agenda-sorting-strategy '(todo-state-up priority-down deadline-up))))
        ("ew" "of will-do task"
         tags "+Effort=>\"0\"/UG|IP|WD"
         ((org-agenda-overriding-header "This Week's Task")
          (org-local-columns-format "%26ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM(Time){:}")
          (org-agenda-view-columns-initially t)
          (org-agenda-sorting-strategy '(todo-state-up priority-down deadline-up))))
        ("ea" "of all task"
         tags "+Effort=>\"0\"/UG|IP|WD|TD"
         ((org-agenda-overriding-header "This Week's Task")
          (org-local-columns-format "%26ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM(Time){:}")
          (org-agenda-view-columns-initially t)
          (org-agenda-sorting-strategy '(todo-state-up priority-down deadline-up))))
        ("ed" "of done task"
         tags "+Effort=>\"0\"/DN|CX"
         ((org-agenda-overriding-header "Done task")
          (org-local-columns-format "%26ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM(Time){:}")
          (org-agenda-view-columns-initially t)
          (org-agenda-sorting-strategy '(todo-state-up priority-down deadline-up))))
        ("E" . "tasks without Effort")
        ("Ei" "doing task" tags "+Effort<\"0:01\"/UG|IP")
        ("Ew" "will-do task" tags "+Effort<\"0:01\"/UG|IP|WD")
        ("Ea" "all task" tags "+Effort<\"0:01\"/UG|IP|WD|TD")
        ("i" "Daily task list"
         ((org-ql-search-block `(heading ,(format-time-string "%Y-%m-%d %A"))
                               ((org-agenda-files `(,(org-record-file)))
                                (org-ql-block-header "Today's tree")))
          (org-ql-search-block '(or (todo "UG") (todo "IP"))
                               ((org-ql-block-header "Today's task")
                                (org-agenda-sorting-strategy
                                 '(priority-down))))
          (org-ql-search-block '(and (planning :on today)
                                     (not (todo "IP" "DN" "CX" "PD"))
                                     (not (tags "scrap"))
                                     (not (habit)))
                               ((org-ql-block-header "Scheduled/Deadlined on today")))
          (org-ql-search-block '(and (habit)
                                     (todo "TD")
                                     (scheduled :to today)
                                     (not (tags-inherited "ARCHIVE")))
                               ((org-ql-block-header "Habits to take")))
          (org-ql-search-block '(and (ts-active :on today)
                                     (not (or (todo "IP") (habit) (done))))
                               ((org-ql-block-header "Today's common event")))
          (org-ql-search-block '(and (done)
                                     (closed :on today))
                               ((org-ql-block-header "Completed tasks on today"))))
         ((org-agenda-sorting-strategy
           '(todo-state-up priority-down deadline-up))))
        ("w" "Weekly task list"
         ((org-ql-search-block `(heading ,(format-time-string "%G-W%V"))
                               ((org-agenda-files `(,(org-record-file)))
                                (org-ql-block-header "This week's tree")))
          (org-ql-search-block '(or (todo "WD"))
                               ((org-ql-block-header "This week's tasks")))
          (org-ql-search-block '(or (todo "UG") (todo "IP"))
                               ((org-ql-block-header "Today's tasks")))
          (org-ql-search-block '(and (planning :from 0 :to 6)
                                     (not (todo "IP" "DN" "CX" "PD"))
                                     (not (tags "scrap"))
                                     (not (habit)))
                               ((org-ql-block-header "Scheduled/Deadlined this week")))
          (org-ql-search-block '(and (and (ts-active :from 0 :to 6)
                                          (not (deadline))
                                          (not (scheduled))
                                          (not (closed)))
                                     (not (or (todo "IP" "WD") (done))))
                               ((org-ql-block-header "This week's common event"))))
         ((org-agenda-sorting-strategy
           '(todo-state-up priority-down deadline-up))))
        ("A" "Doable thing list"
         ((org-ql-search-block '(and (todo "DA")
                                     (not (tags "scrap"))())
                               ((org-ql-block-header "Doable things")))
          (org-ql-search-block '(and (todo "DA")
                                     (tags "scrap")
                                     (not (deadline :to -1)))
                               ((org-ql-block-header "Scraps"))))
         ((org-agenda-sorting-strategy
           '(todo-state-up priority-down deadline-up))))
        ("n" "Anniversary"
         ((org-ql-search-block `(heading ,(let ((week-ago (decode-time)))
                                            (cl-incf (nth 3 week-ago) -7)
                                            (format-time-string "%Y-%m-%d %A"
                                                                (apply #'encode-time
                                                                       week-ago))))
                               ((org-ql-block-header "One week ago")))
          (org-ql-search-block `(heading ,(let ((month-ago (decode-time)))
                                            (cl-incf (nth 4 month-ago) -1)
                                            (format-time-string "%Y-%m-%d %A"
                                                                (apply #'encode-time
                                                                       month-ago))))
                               ((org-ql-block-header "One month ago")))
          (org-ql-search-block `(heading ,(let ((half-a-year-ago (decode-time)))
                                            (cl-incf (nth 4 half-a-year-ago) -6)
                                            (format-time-string "%Y-%m-%d %A"
                                                                (apply #'encode-time
                                                                       half-a-year-ago))))
                               ((org-ql-block-header "Half a year ago")))
          (org-ql-search-block `(heading ,(let ((year-ago (decode-time)))
                                            (cl-incf (nth 5 year-ago) -1)
                                            (format-time-string "%Y-%m-%d %A"
                                                                (apply #'encode-time
                                                                       year-ago))))
                               ((org-ql-block-header "One year ago"))))
         ((org-agenda-files `,(org-record-files))))
        ("t" "All tasks"
         ((org-ql-search-block '(todo "UG")
                               ((org-ql-block-header "Urgent task")))
          (org-ql-search-block '(todo "IP")
                               ((org-ql-block-header "Today's task")))
          (org-ql-search-block '(todo "WD")
                               ((org-ql-block-header "This week's task")))
          (org-ql-search-block '(and (todo "TD")
                                     (not (habit)))
                               ((org-ql-block-header "Remaining task")))))
        ("d" "Done tasks"
         ((org-ql-search-block '(done)
                               ((org-ql-block-header "Done/Canceled/Pending task")))))
        ("#" "stuck project"
         ((org-ql-search-block '(and (tags "project")
                                     ;; exclude projects
                                     (not (children (todo "TD" "WD" "IP" "DA"))))
                               ((org-ql-block-header "Stuck projects")))))
        ("p" "Projects" tags "+project")
        ("h" "Habits in consistency graph"
         agenda ""
         ((org-agenda-span 'day)
          (org-agenda-use-time-grid nil)
          (org-agenda-prefix-format '((agenda . "")))
          (org-habit-show-all-today t)
          (org-habit-graph-column 32)
          (org-habit-preceding-days 14)
          (org-habit-following-days 21)
          (org-agenda-sorting-strategy '(scheduled-up))
          ;; display habits only
          (org-agenda-skip-function
           (lambda ()
             (and (save-excursion
                    (not (org-is-habit-p)))
                  (progn (outline-next-heading) (point)))))))
        ("g" "aggregated task list"
         ((tags "SCHEDULED<=\"<today>\"|DEADLINE<=\"<today>\"")
          (todo "UG|IP|DA")
          (agenda ""))
         ((org-agenda-sorting-strategy
           '(todo-state-up
             priority-down
             deadline-up
             category-up))
          (org-agenda-span 14)
          (org-agenda-show-all-dates t)
          (org-agenda-use-time-grid nil)
          (org-agenda-show-current-time-in-grid nil)
          (org-agenda-start-with-log-mode nil)
          (org-agenda-skip-deadline-if-done t)
          (org-deadline-warning-days 30)
          (org-agenda-todo-ignore-deadlines 'far)))))

(defvar auto-org-capture-file (make-temp-file "auto-org-capture" nil ".org"))
(defvar org-capture-todo-file (concat env-doc-dir "/priv_a/life.org"))

(defun org-goto-clocking-or-today ()
  "Go to currently clocking entry.

If no entry is clocked or CATEGORY on clocking entry is \"Repeated\",
go to today's entry in record file."
  (if (and (org-clocking-p)
           (save-excursion
             (with-current-buffer (org-clocking-buffer)
               (org-clock-jump-to-current-clock)
               (org-back-to-heading)
               (not (string=
                     (org-entry-get (point) "CATEGORY" t)
                     "Repeated")))))
      (org-clock-goto)
    (let* ((now (decode-time (current-time)))
           (day (nth 3 now))
           (month (nth 4 now))
           (year (nth 5 now))
           (org-refile-targets
            `((,org-record-file :regexp . ,(format "%04d-%02d-%02d" year month day)))))
      (find-file org-record-file)
      (org-datetree-find-iso-week-create `(,month ,day ,year) nil))))
(setq org-capture-templates
      `(("t" "Task"
         entry (id "adcd63ea-f81a-4909-b659-6e5794052fcc")
         "* TD %?\n  ADDED: %U\n")
        ("p" "Project"
         entry (id "adcd63ea-f81a-4909-b659-6e5794052fcc")
         "* %? [/] :project:\n  ADDED: %U\n  - [ ] insert ID property if necessary"
         :prepend t :jump-to-captured t)
        ("m" "Memo"
         entry (file+datetree ,org-record-file)
         "* %? %^g\n  ADDED: %U\n" :tree-type week)
        ("s" "Someday memo")
        ("ss" "any"
         entry (file+datetree ,org-record-file)
         "* SD %?\n  ADDED: %U\n  %a"
         :tree-type week)
        ("sr" "read"
         entry (file+datetree ,org-record-file)
         "* SD %? :ac_purchase:fd_book:\n  ADDED: %U\n  %a"
         :tree-type week)
        ("sR" "read (register to whisper as kindle)"
         entry (file+datetree ,org-record-file)
         "* SD %? :ac_purchase:fd_book:ap_whisper:%^{WP_URL1_FORMAT}p%^{WP_URL1}p%^{WP_ALERT}p\n  ADDED: %U\n  - [ ] insert ID property\n  %a"
         :tree-type week)
        ("sc" "cook"
         entry (file+datetree ,org-record-file)
         "* SD %? :ac_cook:\n  ADDED: %U\n  %a"
         :tree-type week)
        ("sp" "purchase"
         entry (file+datetree ,org-record-file)
         "* SD %? :ac_purchase:\n  ADDED: %U\n  %a"
         :tree-type week)
        ("sm" "make"
         entry (file+datetree ,org-record-file)
         "* SD %? :ac_make:\n  ADDED: %U\n  %a"
         :tree-type week)
        ("D" "Drill")
        ("Dd" "Drill entry in currently clocking or today's entry."
         entry (function org-goto-clocking-or-today)
         "* %i :drill:\n  [%?]")
        ("De" "English drill entry in currently clocking or today's entry."
         entry (function org-goto-clocking-or-today)
         "* %i :drill:fd_en:\n[%^C%?]\n- %a")
        ("S" "Web site for deferred checking (scrap)"
         entry (id "68d74115-1f70-448d-a76e-738e32b272d8")
         "* DA %a :scrap:ac_read:\nDEADLINE: %(org-capture-templates-insert-week-ahead)")
        ("$" "deferred checking with immediate finish"
         entry (id "68d74115-1f70-448d-a76e-738e32b272d8")
         "* DA %a :scrap:ac_read:\nDEADLINE: %(org-capture-templates-insert-week-ahead)"
         :immediate-finish t)
        ("M" "Memo to the clocked"
         item (clock)
         "- %i%?")
        ;; for auto refiling
        ("r" "note from region"
         entry (file+datetree ,org-record-file)
         "* %i\n  %U\n" :immediate-finish t :tree-type week)
        ("0" "note"
         entry (file ,auto-org-capture-file)
         "* %?\n  ADDED: %U")
        ("i" "TODO for auto refiling"
         entry (id "e6ee5322-dfb3-407b-846f-87a6ddd4705c")
         "%i" :immediate-finish t :prepend t)
        ("n" "memo for auto refiling"
         entry (file+datetree ,org-record-file)
         "%i" :immediate-finish t :prepend t :tree-type week)))
(defun org-capture-templates-insert-week-ahead ()
  "Insert timestamp of a week ahead."
  (let* ((current (decode-time (current-time)))
         (week-ahead (encode-time (nth 0 current)
                                  (nth 1 current)
                                  (nth 2 current)
                                  (+ (nth 3 current) 7)
                                  (nth 4 current)
                                  (nth 5 current))))
    (format-time-string "<%Y-%m-%d %a>" week-ahead)))
;; for auto refiling capture
(defun auto-org-capture (arg)
  (interactive "p")
  (cl-case arg
    (16 (find-file org-record-file)
        (goto-char (point-min)))
    (4  (find-file org-capture-todo-file)
        (goto-char (point-min)))
    (t  (org-capture nil (if (region-active-p) "r" "0")))))
(defun auto-org-capture-auto-refile ()
  (when (equal (org-capture-get :key) "0")
    (with-current-buffer (find-file-noselect auto-org-capture-file)
      (unwind-protect
          (org-capture-string
           (buffer-string)
           (if (string-match (concat org-ts-regexp "\\|\\* \\(UG\\|IP\\|WD\\|TD\\)")
                             (buffer-string))
               "i" "n"))
        (set-buffer-modified-p nil)
        (kill-this-buffer)
        (delete-file auto-org-capture-file)))))
(add-hook 'org-capture-mode-hook
          (lambda ()
            (skk-mode 1)
            (delete-other-windows)))
(add-hook 'org-capture-prepare-finalize-hook
          (lambda ()
            (unless (string-suffix-p "\n" (buffer-string))
              (goto-char (point-max))
              (insert "\n"))))
(add-hook 'org-capture-after-finalize-hook 'auto-org-capture-auto-refile)
(bind-keys ("C-c C" . counsel-org-capture))

(setq org-refile-targets
      `((org-agenda-files :tag . "project")
        (,(file-expand-wildcards (concat env-doc-dir "/**/*.org")) :tag . "refile")))


(defun org-tags-view-in-records (&optional todo-only match)
  "Invoke `org-tags-view' using predetermined agenda files plus record files.
The prefix args TODO-ONLY and MATCH are passed to 'org-tags-view.

If region is active, use the word in region for matching instead."
  (interactive)
  (let* ((record-cands (file-expand-wildcards (format "%s/archive/record_*.org" env-doc-dir)))
         (record-files (last record-cands (safe-length record-cands)))
         (org-agenda-files (append org-agenda-files-default
                                   (sort record-files 'string<)))
         (match-exp (if (region-active-p)
                        (buffer-substring (region-beginning) (region-end))
                      match)))
    (org-tags-view todo-only match-exp)))

(defconst org-project-property "PRJ_ID"
  "The property that is being used to use ref-id feature.")
(defcustom org-project-parent-tag-list
  '("project" "refile")
  "List of tag names which all parents in org-project feature must have.")

(defun org-project-lookup-children ()
  "Show all project tasks of this entry."
  (interactive)
  (let ((id (org-id-get)))
    (when id
      (org-ql-search org-record-files `(property ,org-project-property ,id)))))
(defun org-project-lookup-siblings ()
  "Show all sibling tasks in same project."
  (interactive)
  (let ((prj-id (org-entry-get (point) org-project-property)))
    (when prj-id
      (org-ql-search org-record-files `(property ,org-project-property ,prj-id)))))
(defun org-project-correlate-parent-child ()
  "Make parent-child relationship.
Children's `org-project-property' will be parent's ID.

This function must be called in parent entry
which has any one of `org-project-parent-tag-list'."
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (if (some (lambda (parent-tag)
                (member parent-tag (org-get-tags)))
              org-project-parent-tag-list)
        (let ((ref-id (org-id-get-create)))
          (when (org-goto-first-child)
            (cl-labels ((set-ref-id-to-siblings
                         ()
                         (org-set-property org-project-property ref-id)
                         (when (org-goto-sibling)
                           (set-ref-id-to-siblings))))
              (set-ref-id-to-siblings))))
      (message "This entry does not compliant with 'org-project-parent-tag-list"))))
(defun org-agenda-ref-id-tieup-tree ()
  "Tie up subtree by setting property."
  (interactive)
  (org-agenda-check-no-diary)
  (let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
                       (org-agenda-error)))
         (buffer (marker-buffer hdmarker))
         (pos (marker-position hdmarker))
         (inhibit-read-only t)
         newhead)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
        (widen)
        (goto-char pos)
        (org-show-context 'agenda)
        (call-interactively 'org-project-correlate-parent-child)
        (end-of-line 1)
        (setq newhead (org-get-heading))))))

;; clock-timer collaboration for fixed time tasks
(defvar org-clock-current-task-alert nil "ALERT property's value of currently clocked entry")
(add-hook 'org-clock-in-hook
          (lambda ()
            (let ((tl (org-entry-get (point) "TIMELIMIT" 'selective))
                  (tl-m (org-entry-get (point) "TIMELIMIT_MIN" 'selective))
                  (alert (org-entry-get (point) "ALERT" 'selective)))
              (when (cond
                     (tl (org-timer-set-timer tl))
                     (tl-m (org-timer-set-timer (string-to-number tl-m)))
                     ((org-get-todo-state) (org-timer-set-timer 25)))
                (setq org-clock-current-task-alert alert)))))
(defun org-clock-cleanup ()
  "Post process after org-clock is done."
  (when (and (boundp 'org-timer-countdown-timer)
             org-timer-countdown-timer)
    (org-timer-stop)
    (setq org-clock-current-task-alert nil)))
(add-hook 'org-clock-out-hook #'org-clock-cleanup)
(add-hook 'org-clock-cancel-hook #'org-clock-cleanup)
(add-hook 'org-timer-done-hook
          (lambda ()
            (when (org-clocking-p)
              (if (string= org-clock-current-task-alert "alarm")
                  (alert "Timer DN!" :style 'alarm)
                (alert "Timer DN!" :style 'fringe :mode 'org-mode :buffer (org-clocking-buffer) :severity 'trivial)))))

(defun org-capture-phrase (phrase &optional selective)
  "Search or capture PHRASE.
If there is an org entry whose heading is PHRASE, show it.
Otherwise capture it.

If called interactively with `C-u' prefix, namely SELECTIVE is '(4),
 capture PHRASE forcibly.
With `C-uC-u' prefix, search PHRASE forcibly."
  (interactive (list (if (use-region-p)
                         (buffer-substring (region-beginning) (region-end))
                       (read-string "Phrase: "))
                     current-prefix-arg))
  (let ((query-phrase `(and (heading ,phrase)
                            (tags "drill")))
        (rec-files (org-record-files)))
    (cond
     ((or (equal selective '(4))
          (and (not selective)
               (not (org-ql-select rec-files query-phrase))))
      (org-capture nil "De"))
     (t
      (org-ql-search rec-files query-phrase)))))
(push '("English phrase list"
        :buffers-files org-record-files
        :query (and (tags "drill")
                    (tags "fd_en")))
      org-ql-views)

(defun org-weekly-review-archive-candidates (due-date)
  "List candidate entries for archiving in weekly review ends with DUE-DATE."
  (interactive (list (org-read-date nil nil nil "Due date: ")))
  (let ((files (org-agenda-files)))
    (org-ql-search files `(or (todo "SD")
                              (and (not (habit))
                                   (not (tags "project"))
                                   (planning :to ,due-date))
                              (and (tags "scrap")
                                   (deadline :to ,due-date)))
      :sort '(date))))
