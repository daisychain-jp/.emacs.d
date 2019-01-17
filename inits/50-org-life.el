(defun org-life ()
  "settings for self management"
  (setq org-agenda-start-on-weekday 0)
  (setq org-deadline-warning-days 60)
  (setq org-agenda-include-diary t)
  (defvar org-agenda-files-archive
    (append org-agenda-files-default
            (sort (file-expand-wildcards (format "%s/archive/*_archive.org" env-doc-dir)) 'string<))
    "agenda files plus archive files")
  (setq org-agenda-custom-commands
        '(("r" "Match a TAGS/PROP/TODO query in archive file"
           tags ""
           ((org-agenda-files org-agenda-files-archive)
            (org-agenda-sorting-strategy '(time-down))))
          ("R" "Match a TAGS/PROP/TODO query only for TODO entries in archive file"
           tags-todo ""
           ((org-agenda-files org-agenda-files-archive)
            (org-agenda-sorting-strategy '(time-down))))
          ("o" . "Someday entries")
          ("oo" "all" tags "st_somd"
           ((org-agenda-files org-agenda-files-archive)
            (org-agenda-sorting-strategy '(time-down))))
          ("or" "read" tags "st_somd+ac_read"
           ((org-agenda-files org-agenda-files-archive)
            (org-agenda-sorting-strategy '(time-down))))
          ("oc" "cook" tags "st_somd+ac_cook"
           ((org-agenda-files org-agenda-files-archive)
            (org-agenda-sorting-strategy '(time-down))))
          ("ob" "buy" tags "st_somd+ac_buy"
           ((org-agenda-files org-agenda-files-archive)
            (org-agenda-sorting-strategy '(time-down))))
          ("ow" "watch" tags "st_somd+ac_watch"
           ((org-agenda-files org-agenda-files-archive)))
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
          ("L" "Log entries in a day"
           agenda ""
           ((org-agenda-span 'day)
            (org-agenda-start-with-log-mode t)
            (org-agenda-include-inactive-timestamps nil)
            (org-agenda-include-diary t)
            (org-agenda-sorting-strategy
             '(time-up
               deadline-up
               todo-state-up
               priority-down))))
          ("e" . "Effort table")
          ("et" "of Today's task"
           tags "+Effort=>\"0\"/URGE|TDAY"
           ((org-agenda-overriding-header "Today's Task")
            (org-agenda-overriding-columns-format "%26ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM(Time){:}")
            (org-agenda-view-columns-initially t)
            (org-agenda-sorting-strategy '(todo-state-up priority-down deadline-up))))
          ("ew" "of this Week's task"
           tags "+Effort=>\"0\"/URGE|TDAY|WEEK"
           ((org-agenda-overriding-header "This Week's Task")
            (org-agenda-overriding-columns-format "%26ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM(Time){:}")
            (org-agenda-view-columns-initially t)
            (org-agenda-sorting-strategy '(todo-state-up priority-down deadline-up))))
          ("ea" "of All Task"
           tags "+Effort=>\"0\"/URGE|TDAY|WEEK|TODO"
           ((org-agenda-overriding-header "This Week's Task")
            (org-agenda-overriding-columns-format "%26ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM(Time){:}")
            (org-agenda-view-columns-initially t)
            (org-agenda-sorting-strategy '(todo-state-up priority-down deadline-up))))
          ("ed" "of Done task"
           tags "+Effort=>\"0\"/DONE|CXL"
           ((org-agenda-overriding-header "Done task")
            (org-agenda-overriding-columns-format "%26ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM(Time){:}")
            (org-agenda-view-columns-initially t)
            (org-agenda-sorting-strategy '(todo-state-up priority-down deadline-up))))
          ("E" . "tasks without Effort")
          ("Et" "Today's task" tags "+Effort<\"0:01\"/URGE|TDAY")
          ("Ew" "this Week's task" tags "+Effort<\"0:01\"/URGE|TDAY|WEEK")
          ("Ea" "All Task" tags "+Effort<\"0:01\"/URGE|TDAY|WEEK|TODO")
          ("t" . "TODO entries")
          ("tm" "Master task list"
           alltodo ""
           ((org-agenda-sorting-strategy
             '(habit-down todo-state-up priority-down deadline-up))))
          ("tt" "Today's task list"
           tags "-CONDITION/URGE|TDAY"
           ((org-agenda-sorting-strategy
             '(todo-state-up priority-down deadline-up))))
          ("tw" "this Week's task list"
           tags "-CONDITION/URGE|TDAY|WEEK"
           ((org-agenda-sorting-strategy
             '(todo-state-up priority-down deadline-up))))
          ("ta" "task list of All"
           tags "-CONDITION/URGE|TDAY|WEEK|TODO"
           ((org-agenda-sorting-strategy
             '(todo-state-up priority-down deadline-up))))
          ("tp" "Pending list"
           todo "PEND"
           ((org-agenda-archives-mode t)
            (org-agenda-sorting-strategy
             '(todo-state-up priority-down))))
          ("td" "Done list"
           todo "DONE|CXL"
           ((org-agenda-sorting-strategy
             '(todo-state-up priority-down deadline-up))))
          ("$" "Archiving candidates"
           ((tags "-CATEGORY=\"Project\"+TODO={DONE\\|CXL\\|PEND}")
            (tags "st_somd|st_done")))
          ("te" "Easy tasks" tags-todo "t_easy")
          ("tc" "Commit tasks" tags-todo "t_commit")
          ("p" "Projects" tags "+CATEGORY={Project}+LEVEL=2")
          ("h" "HBT entries" tags-todo "TODO=\"HBT\"+SCHEDULED<\"<+1d>\""
           ((org-agenda-sorting-strategy
             '(scheduled-up))))
          ("H" "All Habits"
           tags "CATEGORY={Habit}+LEVEL=>2")
          ("c" "Clockable entries" tags "+CATEGORY={Action\\|Habit\\|Task\\|Project}")
          ("g" "Trigger" tags "+trigger")
          ("@" . "place-related tasks")
          ("@@" "All place-related tasks" tags "PLACE")
          ("d" "Daily task list"
           ((tags "SCHEDULED<=\"<today>\"|DEADLINE<=\"<today>\"")
            (todo "URGE|TDAY")
            (tags "CATEGORY={Action}+LEVEL=>2+st_ongo")
            (agenda ""))
           ((org-agenda-sorting-strategy
             '(time-up
               todo-state-up
               priority-down
               deadline-up))
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
  (defvar org-capture-memo-file (concat env-doc-dir "/archive/2019_archive.org"))
  (setq org-capture-templates
        `(("t" "Task"
           entry (id "e6ee5322-dfb3-407b-846f-87a6ddd4705c")
           "* TODO %?\n  ADDED: %U\n")
          ("p" "Project"
           entry (id "adcd63ea-f81a-4909-b659-6e5794052fcc")
           "* %?\n  ADDED: %U" :jump-to-captured t)
          ("m" "Memo"
           entry (file+datetree ,org-capture-memo-file)
           "* %? %^g\n  ADDED: %U\n" :tree-type week)
          ("d" "Diary"
           entry (file+datetree ,org-capture-memo-file)
           "* %t :mm_diary:\n  %U\n%?"
           :tree-type week :time-prompt t)
          ("o" "Someday memo")
          ("oo" "All"
           entry (file+datetree ,org-capture-memo-file)
           "* %? :st_somd:\n  ADDED: %U\n  %a"
           :tree-type week)
          ("or" "Reading"
           entry (file+datetree ,org-capture-memo-file)
           "* %? :ac_read:st_somd:\n  ADDED: %U\n  %a"
           :tree-type week)
          ("oR" "Reading with kindle book"
           entry (file+datetree ,org-capture-memo-file)
           "* %? :ac_read:st_somd:ap_whisper:%^{WP_URL1_FORMAT}p%^{WP_URL1}p%^{WP_ALERT}p\n  ADDED: %U\n  - [ ] insert ID property\n  %a"
           :tree-type week)
          ("oc" "Cooking"
           entry (file+datetree ,org-capture-memo-file)
           "* %? :ac_cook:st_somd:\n  ADDED: %U\n  %a"
           :tree-type week)
          ("ob" "Buying"
           entry (file+datetree ,org-capture-memo-file)
           "* %? :ac_buy:st_somd:\n  ADDED: %U\n  %a"
           :tree-type week)
          ("D" "Drill entry to the clocked"
           entry (clock)
           "* %i%? :drill:" :immediate-finish t)
          ("M" "Memo to the clocked"
           item (clock)
           "- %i%?")
          ;; for auto refiling
          ("r" "note from region"
           entry (file+datetree ,org-capture-memo-file)
           "* %i\n  %U\n" :immediate-finish t :tree-type week)
          ("0" "note"
           entry (file ,auto-org-capture-file)
           "* %?\n  ADDED: %U")
          ("i" "TODO for auto refiling"
           entry (id "e6ee5322-dfb3-407b-846f-87a6ddd4705c")
           "%i" :immediate-finish t :prepend t)
          ("n" "memo for auto refiling"
           entry (file+datetree ,org-capture-memo-file)
           "%i" :immediate-finish t :prepend t :tree-type week)))
  ;; for auto refiling capture
  (defun auto-org-capture (arg)
    (interactive "p")
    (cl-case arg
      (16 (find-file org-capture-memo-file)
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
             (if (string-match (concat org-ts-regexp "\\|\\* \\(URGE\\|TDAY\\|WEEK\\|TODO\\)")
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
  (bind-keys ("C-c C" . auto-org-capture))

  (setq org-refile-targets
        `((org-agenda-files :regexp . "Projects\\|Tasks")
          (org-agenda-files :level . 2)
          (,(file-expand-wildcards (concat env-doc-dir "/**/*.org")) :tag . "refile")))
  (setq org-stuck-projects
        '("+CATEGORY=\"Project\"+LEVEL=2-SCHEDULED>\"<today>\"-DEADLINE>\"<today>\"/-TODO-DONE-CXL"
          ("URGE" "TDAY" "WEEK" "TODO") nil ""))

  (defun org-tags-view-archive (&optional todo-only match)
    "Execute org-tags-view with archive file (xxx-archive.org) as org-agenda-files.

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

  (defun org-id-view-refs ()
    "Show all entries which refers this entry by having REF_ID property."
    (interactive)
    (let* ((id-prop (org-entry-properties nil "ID"))
           (id (if (listp id-prop) (cdar id-prop) nil)))
      (when id
        (org-tags-view-archive nil (format "REF_ID=\"%s\"" id)))))

  ;; switch state HBT to DONE when clock is out
  (setq org-clock-out-switch-to-state
        (lambda (state)
          (when (string= state "HBT")
            "DONE")))

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
                    (alert "Time for the task is over" :style 'alarm)
                  (alert "Time for the task is over" :buffer (org-clocking-buffer))))))
  )
