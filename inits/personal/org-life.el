(setq org-agenda-start-on-weekday 1)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-include-diary t)
(setq org-agenda-custom-commands
      `(("a" "Week-agenda"
         agenda ""
         ((org-agenda-skip-function
           (lambda ()
             ;; skip entry which has 'web' tag even if it has deadline
             (and (save-excursion
                    (let ((tags (org-get-tags)))
                      (member "web" tags)))
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
        ("o" . "someday list")
        ("om" "someday to make"
         ((org-ql-search-block `(and (todo ,org-todo-keyword-4)
                                     (tags "ac_make"))
                               ((org-ql-block-header "Someday to make"))))
         ((org-agenda-files org-record-files)
          (org-agenda-sorting-strategy '(priority-down))))
        ("op" "someday to purchase"
         ((org-ql-search-block `(and (todo ,org-todo-keyword-4)
                                     (tags "ac_purchase"))
                               ((org-ql-block-header "Someday to purchase"))))
         ((org-agenda-files org-record-files)
          (org-agenda-sorting-strategy '(priority-down))))
        ("oc" "someday to cook"
         ((org-ql-search-block `(and (todo ,org-todo-keyword-4)
                                     (tags "ac_cook"))
                               ((org-ql-block-header "Someday to cook"))))
         ((org-agenda-files org-record-files)
          (org-agenda-sorting-strategy '(priority-down))))
        ("O" "all someday entries"
         ((org-ql-search-block `(and (todo ,org-todo-keyword-4)
                                     (tags "ac_make"))
                               ((org-ql-block-header "Someday to make")))
          (org-ql-search-block `(and (todo ,org-todo-keyword-4)
                                     (tags "ac_purchase"))
                               ((org-ql-block-header "Someday to purchase")))
          (org-ql-search-block `(and (todo ,org-todo-keyword-4)
                                     (tags "ac_cook"))
                               ((org-ql-block-header "Someday to cook")))
          (org-ql-search-block `(and (todo ,org-todo-keyword-4)
                                     (not (tags "ac_purchase" "ac_cook" "ac_make")))
                               ((org-ql-block-header "Someday things"))))
         ((org-agenda-files org-record-files)))
        ("l" "Log entries in a week"
         agenda ""
         ((org-agenda-span (if (equal current-prefix-arg '(4))
                               'day 'week))
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
          (org-agenda-span (if (equal current-prefix-arg '(4))
                               'day 'week))
          (org-agenda-start-with-log-mode t)
          (org-agenda-include-inactive-timestamps nil)
          (org-agenda-include-diary t)
          (org-agenda-sorting-strategy
           '(time-up
             deadline-up
             todo-state-up
             priority-down))))
        ;; KEEP IN MIND
        ;; invoking `org-clock-sum-all' is required before showing effort table
        ("e" . "Effort table")
        ("ei" "today"
         ((org-ql-search-block `(or (todo ,org-warning-keyword-0)
                                    (todo ,org-todo-keyword-2)
                                    (and (clocked :on today)
                                         (or (todo) (done))
                                         (not (habit))
                                         (not (tags "web"))))
                               ((org-ql-block-header "Today's task"))))
         ((org-agenda-overriding-header "Today's Task")
          (org-overriding-columns-format "%26ITEM(Task) %Effort(Effort){:} %CLOCKSUM_T(Today){:} %CLOCKSUM(Total)")
          (org-agenda-view-columns-initially t)
          (org-agenda-sorting-strategy '(todo-state-up priority-down deadline-up))))
        ("eg" "this week"
         ((org-ql-search-block `(or (todo ,org-warning-keyword-0)
                                    (todo ,org-todo-keyword-1)
                                    (todo ,org-todo-keyword-2))
                               ((org-ql-block-header "This Week's task"))))
         ((org-agenda-overriding-header "This Week's Task")
          (org-overriding-columns-format "%26ITEM(Task) %Effort(Effort){:} %CLOCKSUM_T(Today){:} %CLOCKSUM(Total)")
          (org-agenda-view-columns-initially t)
          (org-agenda-sorting-strategy '(todo-state-up priority-down deadline-up))))
        ("ed" "done task"
         ((org-ql-search-block `(or (todo ,org-done-keyword-0)
                                    (todo ,org-done-keyword-1)
                                    (todo ,org-done-keyword-2))
                               ((org-ql-block-header "Done task"))))
         ((org-agenda-overriding-header "Done Task")
          (org-overriding-columns-format "%26ITEM(Task) %Effort(Effort){:} %CLOCKSUM(Total){:}")
          (org-agenda-view-columns-initially t)
          (org-agenda-sorting-strategy '(todo-state-up priority-down deadline-up))))
        ("i" "Today's agenda"
         ((todo "Today's agenda"
                ((org-agenda-sorting-strategy '(priority-up))))
          (org-ql-search-block `(heading ,(format-time-string "%Y-%m-%d %A"))
                               ((org-agenda-files `(,(org-record-file)))
                                (org-ql-block-header "Today's tree node")))
          (org-ql-search-block `(or (todo ,org-warning-keyword-0)
                                    (todo ,org-todo-keyword-2))
                               ((org-ql-block-header "Today's task")))
          (org-ql-search-block `(and (planning :on today)
                                     (not (todo ,org-todo-keyword-2
                                                ,org-done-keyword-0
                                                ,org-done-keyword-1
                                                ,org-done-keyword-2))
                                     (not (tags "web"))
                                     (not (habit)))
                               ((org-ql-block-header "Scheduled/Deadlined on today")))
          (org-ql-search-block `(and (habit)
                                     (todo ,org-todo-keyword-0)
                                     (scheduled :to today)
                                     (not (tags "ac_impulse"))
                                     (not (tags-inherited "ARCHIVE")))
                               ((org-ql-block-header "Habits to take")))
          (org-ql-search-block `(and (ts-active :on today)
                                     (not (or (todo ,org-todo-keyword-2)
                                              (habit) (done))))
                               ((org-ql-block-header "Today's common event")))
          (org-ql-search-block '(and (done)
                                     (closed :on today))
                               ((org-ql-block-header "Completed tasks on today")))))
        ("g" "This Week's agenda"
         ((org-ql-search-block `(heading ,(format-time-string "%G-W%V"))
                               ((org-agenda-files `(,(org-record-file)))
                                (org-ql-block-header "This week's tree")))
          (org-ql-search-block `(or (todo ,org-todo-keyword-1))
                               ((org-ql-block-header "This week's tasks")))
          (org-ql-search-block `(or (todo ,org-warning-keyword-0)
                                    (todo ,org-todo-keyword-2))
                               ((org-ql-block-header "Today's tasks")))
          (org-ql-search-block `(and (planning :from 0 :to 6)
                                     (not (todo ,org-todo-keyword-2
                                                ,org-done-keyword-0
                                                ,org-done-keyword-1
                                                ,org-done-keyword-2))
                                     (not (tags "web"))
                                     (not (habit)))
                               ((org-ql-block-header "Scheduled/Deadlined this week")))
          (org-ql-search-block `(and (and (ts-active :from 0 :to 6)
                                          (not (deadline))
                                          (not (scheduled))
                                          (not (closed)))
                                     (not (or (todo ,org-todo-keyword-1
                                                    ,org-todo-keyword-2)
                                              (done))))
                               ((org-ql-block-header "This week's common event"))))
         ((org-agenda-sorting-strategy
           '(todo-state-up priority-down deadline-up))))
        ("c" "actionable thing list"
         ((org-ql-search-block `(and (todo ,org-todo-keyword-3)
                                     (not (deadline :to -1)))
                               ((org-ql-block-header "Doable things"))))
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
         ((org-ql-search-block `(todo ,org-warning-keyword-0)
                               ((org-ql-block-header "Urgent task")))
          (org-ql-search-block `(todo ,org-todo-keyword-2)
                               ((org-ql-block-header "Today's task")))
          (org-ql-search-block `(todo ,org-todo-keyword-1)
                               ((org-ql-block-header "This week's task")))
          (org-ql-search-block `(todo ,org-todo-keyword-4)
                               ((org-ql-block-header "Someday's entries on agenda files")))
          (org-ql-search-block `(and (todo ,org-todo-keyword-0)
                                     (not (habit)))
                               ((org-ql-block-header "Remaining task")))))
        ("d" "Done tasks"
         ((org-ql-search-block '(done)
                               ((org-ql-block-header "Done/Canceled/Pending task")))))
        ("#" "stuck project"
         ((org-ql-search-block `(and (tags "project")
                                     ;; exclude projects
                                     (not (children (todo ,org-todo-keyword-0
                                                          ,org-todo-keyword-1
                                                          ,org-todo-keyword-2
                                                          ,org-todo-keyword-3))))
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
        ("H" "show all habits"
         ((org-ql-search-block '(habit)
                               ((org-ql-block-header "All Habits"))))
         ((org-agenda-sorting-strategy '(scheduled-up))))))

(defvar org-capture-todo-file (concat env-org-dir "/priv_a/life.org"))

(defun org-goto-clocking-or-today ()
  "Go to currently clocking entry.

If no entry is clocked or CATEGORY on clocking entry is \"Cyclic\",
go to today's entry in record file."
  (if (and (org-clocking-p)
           (save-excursion
             (with-current-buffer (org-clocking-buffer)
               (org-clock-jump-to-current-clock)
               (org-back-to-heading)
               (not (string=
                     (org-entry-get (point) "CATEGORY" t)
                     "Cyclic")))))
      (org-clock-goto)
    (let* ((now (decode-time (current-time)))
           (day (nth 3 now))
           (month (nth 4 now))
           (year (nth 5 now))
           (org-refile-targets
            `((,org-record-file :regexp . ,(format "%04d-%02d-%02d" year month day)))))
      (find-file org-record-file)
      (org-datetree-find-iso-week-create `(,month ,day ,year) nil))))

(use-package org-capture
  :after org
  :init
  (setq org-capture-templates
        `(("t" "Task"
           entry (id "adcd63ea-f81a-4909-b659-6e5794052fcc")
           ,(format "* %s %%?\nADDED: %%U\n"
                    org-todo-keyword-0))
          ("p" "Project"
           entry (id "adcd63ea-f81a-4909-b659-6e5794052fcc")
           "* %? [/] :project:\nADDED: %U\n  - [ ] insert ID property if necessary"
           :prepend t :jump-to-captured t)
          ("m" "Memo"
           entry (file+datetree ,org-record-file)
           "* %? %^g\nADDED: %U\n" :tree-type week)
          ("s" "Someday memo")
          ("ss" "any"
           entry (file+datetree ,org-record-file)
           ,(format "* %s %%?\nADDED: %%U\n  %%a"
                    org-todo-keyword-4)
           :tree-type week)
          ("sr" "purchase book"
           entry (file+datetree ,org-record-file)
           ,(format "* %s %%? :ac_purchase:book:\n  ADDED: %%U\n  %%a"
                    org-todo-keyword-4)
           :tree-type week)
          ("sR" "read (register to whisper as kindle)"
           entry (file+datetree ,org-record-file)
           ,(format "* %s %%? :ac_purchase:book:ap_whisper:%%^{WP_URL1_FORMAT}p%%^{WP_URL1}p%%^{WP_ALERT}p\n  ADDED: %%U\n  - [ ] insert ID property\n  %%a"
                    org-todo-keyword-4)
           :tree-type week)
          ("sc" "cook"
           entry (file+datetree ,org-record-file)
           ,(format "* %s %%? :ac_cook:\n  ADDED: %%U\n  %%a"
                    org-todo-keyword-4)
           :tree-type week)
          ("sp" "purchase"
           entry (file+datetree ,org-record-file)
           ,(format "* %s %%? :ac_purchase:\n  ADDED: %%U\n  %%a"
                    org-todo-keyword-4)
           :tree-type week)
          ("sm" "make"
           entry (file+datetree ,org-record-file)
           ,(format "* %s %%? :ac_make:\n  ADDED: %%U\n  %%a"
                    org-todo-keyword-4)
           :tree-type week)
          ("D" "Drill")
          ("Dd" "Drill entry in currently clocking or today's entry."
           entry (function org-goto-clocking-or-today)
           "* %i :drill:\n[%?]")
          ("De" "English drill entry in currently clocking or today's entry."
           entry (function org-goto-clocking-or-today)
           "* %i :drill:fd_en:\n[%^C%?]\n- %a")
          ("M" "Append memo to clocking task"
           item (clock)
           "- %i%?")))
  :hook
  (org-capture-mode . (lambda ()
                        (skk-mode 1)
                        (delete-other-windows)))
  :custom
  (org-capture-bookmark nil))

(setq org-refile-targets
      `((org-agenda-files :tag . "project")
        (,(file-expand-wildcards (concat env-org-dir "/**/*.org")) :tag . "refile")))

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

(org-ql-defpred category-inherited (category)
  "Return non-nil if current heading has CATEGORY.
Ancestors are looked up If current heading has no CATEGORY."
  :body (string= (org-entry-get (point) "CATEGORY" t)
                 category))

(defun org-weekly-review-archive-candidates (due-date)
  "List candidate entries for archiving in weekly review ends with DUE-DATE."
  (interactive (list (org-read-date nil nil nil "Due date: ")))
  (let ((files (org-agenda-files)))
    (org-ql-search files `(or
                           ;; DONE tasks
                           (todo ,org-todo-keyword-4)
                           (and (not (habit))
                                (not (tags "project"))
                                (planning :to ,due-date))
                           ;; outdated web actionable
                           (and (tags "web")
                                (deadline :to ,due-date))
                           ;; past events
                           (and (not (todo))
                                (ts-active :to ,due-date)
                                (category-inherited "Event")))
      :sort '(date))))
