(setq org-agenda-start-on-weekday 1)
(setq org-deadline-warning-days 60)
(setq org-agenda-include-diary t)
(defvar org-agenda-files-archive
  (append (sort (file-expand-wildcards (format "%s/archive/*_archive.org" env-doc-dir)) 'string<)
          org-agenda-files-default)
  "agenda files plus archive files")
(setq org-agenda-custom-commands
      '(("r" . "Search for all archive files")
        ("rs" "Entries containing search words entry or headline."
         search ""
         ((org-agenda-files org-agenda-files-archive)
          (org-agenda-sorting-strategy '(time-down))))
        ("rm" "Match a TAGS/PROP/TODO query in archive file"
         tags ""
         ((org-agenda-files org-agenda-files-archive)
          (org-agenda-sorting-strategy '(time-down))))
        ("s" . "Someday entries")
        ("sa" "all" tags "TODO=\"SD\""
         ((org-agenda-files org-agenda-files-archive)
          (org-agenda-sorting-strategy '(priority-up time-down))))
        ("sr" "read" tags "TODO=\"SD\"+ac_read"
         ((org-agenda-files org-agenda-files-archive)
          (org-agenda-sorting-strategy '(priority-up time-down))))
        ("sc" "cook" tags "TODO=\"SD\"+ac_cook"
         ((org-agenda-files org-agenda-files-archive)
          (org-agenda-sorting-strategy '(priority-up time-down))))
        ("sp" "purchase" tags "TODO=\"SD\"+ac_purchase"
         ((org-agenda-files org-agenda-files-archive)
          (org-agenda-sorting-strategy '(priority-up time-down))))
        ("sm" "make" tags "TODO=\"SD\"+ac_make"
         ((org-agenda-files org-agenda-files-archive)
          (org-agenda-sorting-strategy '(priority-up time-down))))
        ("b" "tag match for current Buffer"
         tags ""
         ((org-agenda-files `(,buffer-file-name))))
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
        ("d" "daily log view"
         agenda ""
         ((org-agenda-prefix-format (eval (car (get 'org-agenda-prefix-format 'standard-value))))
          (org-agenda-span 'day)
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
         tags "+Effort=>\"0\"/UG|DI"
         ((org-agenda-overriding-header "Today's Task")
          (org-local-columns-format "%26ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM(Time){:}")
          (org-agenda-view-columns-initially t)
          (org-agenda-sorting-strategy '(todo-state-up priority-down deadline-up))))
        ("ew" "of will-do task"
         tags "+Effort=>\"0\"/UG|DI|WD"
         ((org-agenda-overriding-header "This Week's Task")
          (org-local-columns-format "%26ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM(Time){:}")
          (org-agenda-view-columns-initially t)
          (org-agenda-sorting-strategy '(todo-state-up priority-down deadline-up))))
        ("ea" "of all task"
         tags "+Effort=>\"0\"/UG|DI|WD|TD"
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
        ("Ei" "doing task" tags "+Effort<\"0:01\"/UG|DI")
        ("Ew" "will-do task" tags "+Effort<\"0:01\"/UG|DI|WD")
        ("Ea" "all task" tags "+Effort<\"0:01\"/UG|DI|WD|TD")
        ("t" . "TD entries")
        ("ta" "Master task list"
         tags "/UG|DI|WD|TD|OG|SD"
         ((org-agenda-sorting-strategy
           '(todo-state-up priority-down deadline-up))))
        ("ti" "doIng task"
         tags "/UG|DI"
         ((org-agenda-sorting-strategy
           '(todo-state-up priority-down deadline-up))))
        ("tw" "Will-do task"
         tags "-CONDITION/UG|DI|WD"
         ((org-agenda-sorting-strategy
           '(todo-state-up priority-down deadline-up))))
        ("tt" "Todo task"
         tags "-CONDITION/UG|DI|WD|TD"
         ((org-agenda-sorting-strategy
           '(todo-state-up priority-down deadline-up))))
        ("to" "on-going task"
         tags "/OG"
         ((org-agenda-sorting-strategy
           '(category-up priority-down deadline-up))))
        ("ts" "Someday list"
         todo "SD"
         ((org-agenda-archives-mode t)
          (org-agenda-sorting-strategy '(time-down))))
        ("tP" "Pending list"
         todo "PD"
         ((org-agenda-archives-mode t)
          (org-agenda-sorting-strategy '(time-down))))
        ("td" "Done list"
         todo "DN|CX"
         ((org-agenda-sorting-strategy
           '(todo-state-up priority-down deadline-up))))
        ("$" "Archiving candidates"
         ((tags "LEVEL=2+TODO={DN\\|CX\\|PD}")))
        ("p" "Projects" tags "+project")
        ("h" "HB entries" tags-todo "TODO=\"HB\"+SCHEDULED<\"<+1d>\""
         ((org-agenda-sorting-strategy
           '(scheduled-up))))
        ("H" "All Habits" tags "CATEGORY=\"Habit\"+LEVEL=>2+{ac_.+}")
        ("g" "aggregated task list"
         ((tags "SCHEDULED<=\"<today>\"|DEADLINE<=\"<today>\"")
          (todo "UG|DI|OG")
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
(defun org-archive-file (&optional year)
  "Return a path of archive file.
If optional argument 'YEAR passed, a file which contains the year's tree is used instead of this year's one.."
  (let* ((archive-year (if year year (ts-year (ts-now))))
         (archive-file (format "%s/archive/%s_archive.org" env-doc-dir archive-year)))
    (if (or (file-exists-p archive-file)
            (file-symlink-p archive-file))
        archive-file
      nil)))
(defvar org-archive-file (org-archive-file))
(defun org-goto-clocking-or-today ()
  "Go to currently clocking entry.

If no entry is clocked or CATEGORY on clocking entry is Habit,
go to today's entry in archive file."
  (if (and (org-clocking-p)
           (save-excursion
             (with-current-buffer (org-clocking-buffer)
               (org-clock-jump-to-current-clock)
               (org-back-to-heading)
               (not (string=
                     (org-entry-get (point) "CATEGORY" t)
                     "Habit")))))
      (org-clock-goto)
    (let* ((now (decode-time (current-time)))
           (day (nth 3 now))
           (month (nth 4 now))
           (year (nth 5 now))
           (org-refile-targets
            `((,org-archive-file :regexp . ,(format "%04d-%02d-%02d" year month day)))))
      (find-file org-archive-file)
      (org-datetree-find-iso-week-create `(,month ,day ,year) nil))))
(setq org-capture-templates
      `(("t" "Task"
         entry (id "adcd63ea-f81a-4909-b659-6e5794052fcc")
         "* TD %?\n  ADDED: %U\n")
        ("p" "Project"
         entry (id "adcd63ea-f81a-4909-b659-6e5794052fcc")
         "* %? :project:\n  ADDED: %U\n  - [ ] insert REF_ID property if necessary"
         :prepend t :jump-to-captured t)
        ("m" "Memo"
         entry (file+datetree ,org-archive-file)
         "* %? %^g\n  ADDED: %U\n" :tree-type week)
        ("d" "Diary"
         entry (file+datetree ,org-archive-file)
         "* %? :mm_diary:\n  %U\n"
         :tree-type week :time-prompt t)
        ("s" "Someday memo")
        ("ss" "any"
         entry (file+datetree ,org-archive-file)
         "* SD %?\n  ADDED: %U\n  %a"
         :tree-type week)
        ("sr" "read"
         entry (file+datetree ,org-archive-file)
         "* SD %? :ac_purchase:fd_book:\n  ADDED: %U\n  %a"
         :tree-type week)
        ("sR" "read (register to whisper as kindle)"
         entry (file+datetree ,org-archive-file)
         "* SD %? :ac_purchase:fd_book:ap_whisper:%^{WP_URL1_FORMAT}p%^{WP_URL1}p%^{WP_ALERT}p\n  ADDED: %U\n  - [ ] insert ID property\n  %a"
         :tree-type week)
        ("sc" "cook"
         entry (file+datetree ,org-archive-file)
         "* SD %? :ac_cook:\n  ADDED: %U\n  %a"
         :tree-type week)
        ("sp" "purchase"
         entry (file+datetree ,org-archive-file)
         "* SD %? :ac_purchase:\n  ADDED: %U\n  %a"
         :tree-type week)
        ("sm" "make"
         entry (file+datetree ,org-archive-file)
         "* SD %? :ac_make:\n  ADDED: %U\n  %a"
         :tree-type week)
        ("D" "Drill")
        ("Dd" "Drill entry in currently clocking or today's entry."
         entry (function org-goto-clocking-or-today)
         "* %i :drill:\n  [%?]")
        ("De" "English drill entry in currently clocking or today's entry."
         entry (function org-goto-clocking-or-today)
         "* %i :drill:fd_eng:\n[%?]")
        ("M" "Memo to the clocked"
         item (clock)
         "- %i%?")
        ;; for auto refiling
        ("r" "note from region"
         entry (file+datetree ,org-archive-file)
         "* %i\n  %U\n" :immediate-finish t :tree-type week)
        ("0" "note"
         entry (file ,auto-org-capture-file)
         "* %?\n  ADDED: %U")
        ("i" "TODO for auto refiling"
         entry (id "e6ee5322-dfb3-407b-846f-87a6ddd4705c")
         "%i" :immediate-finish t :prepend t)
        ("n" "memo for auto refiling"
         entry (file+datetree ,org-archive-file)
         "%i" :immediate-finish t :prepend t :tree-type week)))
;; for auto refiling capture
(defun auto-org-capture (arg)
  (interactive "p")
  (cl-case arg
    (16 (find-file org-archive-file)
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
           (if (string-match (concat org-ts-regexp "\\|\\* \\(UG\\|DI\\|WD\\|TD\\)")
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
(setq org-stuck-projects
      '("project-SCHEDULED>\"<today>\"-DEADLINE>\"<today>\"/-TD-DN-CX"
        ("UG" "DI" "WD" "TD" "OG") nil ""))

(defun org-tags-view-archive (&optional todo-only match)
  "Invoke `org-tags-view' using predetermined agenda files plus archive files.
The prefix args TODO-ONLY and MATCH are passed to 'org-tags-view.

If region is active, use the word in region for matching instead."
  (interactive)
  (let* ((archive-cands (file-expand-wildcards (format "%s/archive/*_archive.org" env-doc-dir)))
         (archive-files (last archive-cands (safe-length archive-cands)))
         (org-agenda-files (append org-agenda-files-default
                                   (sort archive-files 'string<)))
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
  "Show all entries which refers this entry."
  (interactive)
  (let ((id (org-id-get)))
    (when id
      (org-tags-view-archive nil (format "+%s=\"%s\"" org-project-property id)))))
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

(defun org-archive-to-archive-file ()
  "Archive current subtree to archive file using latest timestamp."
  (interactive)
  (let* ((ts (car (sort (org-timestamps-in-entry t) #'ts>)))
         (year (ts-year ts)))
    (org-refile-to-datetree-using-ts-in-entry 'latest (org-archive-file year) t)
    (org-save-all-org-buffers)
    (setq this-command 'org-archive-to-archive-file)))
(defun org-agenda-archive-to-archive-file ()
  "Archive the entry or subtree belonging to the current agenda entry."
  (interactive)
  (org-agenda-archive-with 'org-archive-to-archive-file))
(bind-keys :map org-agenda-mode-map
           ("$" . org-agenda-archive-to-archive-file) ;
           ("&" . org-agenda-ref-id-tieup-tree))
