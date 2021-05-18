(require 'thingatpt)
(require 'whitespace)

(with-eval-after-load "org-life"
  (mapc (lambda (template)
          (add-to-list 'org-capture-templates
                       template))
        '(("x" "actionable entry for manual input"
           entry
           (function org-capture-derived-target-function)
           "* %(org-capture-derived-insert-todo)%? %(org-set-tags org-capture-derived-tags)\n%(org-capture-derived-insert-deadline)")
          ("X" "Actionable entry for automated input"
           entry
           (function org-capture-derived-target-function)
           "* %(org-capture-derived-insert-todo)%a %(org-set-tags org-capture-derived-tags)\n%(org-capture-derived-insert-deadline)"
           :immediate-finish t))))

(defvar org-capture-derived-todo nil
  "Todo state to be set to an org entry which org-capture-derived creates.

User should set this value locally, otherwise unexpected results occur.")
(defvar org-capture-derived-tags nil
  "Tags represented with colon(:) to be set to an org entry which org-capture-derived creates.

User should set this value locally, otherwise unexpected results occur.")
(defvar org-capture-derived-target nil "docstring")
(defvar org-capture-derived-deadline-in-day nil "docstring")

(defun org-capture-derived-target-function ()
  ""
  (cond
   ((listp org-capture-derived-target)
    (set-buffer (org-capture-target-buffer (nth 1 org-capture-derived-target)))
    (org-capture-put-target-region-and-position)
    (widen)
    (goto-char (nth 3 org-capture-derived-target)))
   ((and (stringp org-capture-derived-target)
         (string-match-p thing-at-point-uuid-regexp org-capture-derived-target))
    (org-id-goto org-capture-derived-target))))

(defun org-capture-derived (&optional arg)
  "C-u user is asked for all arguments
C-u C-u create manually"
  (interactive "P")
  ;; insert ID property unless existing
  (cond
   ((and (eq major-mode 'org-mode)
         (not (equal arg '(16))))
    (org-id-get nil t)))
  (let* ((todo-in-org
          (when (derived-mode-p 'org-mode)
            (org-entry-get (point) "DERIVED_TODO" t)))
         (org-capture-derived-todo
          (or todo-in-org
              org-capture-derived-todo))
         (tags-in-org
          (when (derived-mode-p 'org-mode)
            (org-entry-get (point) "DERIVED_TAGS" t)))
         (org-capture-derived-tags
          (or (when (not (equal arg '(4)))
                (or tags-in-org
                    org-capture-derived-tags))
              (read-string "Tags: " tags-in-org)))
         (target-in-org
          (when (derived-mode-p 'org-mode)
            (org-entry-get (point) "DERIVED_TARGET" t)))
         (org-capture-derived-target
          (or (when (not (equal arg '(4)))
                (or target-in-org
                    org-capture-derived-target))
              (org-refile-get-location)))
         (deadline-str-in-org
          (when (derived-mode-p 'org-mode)
            (org-entry-get (point) "DERIVED_DEADLINE_IN_DAY" t)))
         (deadline-num-in-org
          (if (stringp deadline-str-in-org) (string-to-number deadline-str-in-org) nil))
         (org-capture-derived-deadline-in-day
          (if (equal arg '(4))
              (read-number "Deadline in day: "
                           deadline-num-in-org)
            (or deadline-num-in-org
                org-capture-derived-deadline-in-day))))
    (if (equal arg '(16))
        (org-capture nil "x")
      (org-capture nil "X"))))

(defun org-capture-derived-insert-todo ()
  ""
  (if (stringp org-capture-derived-todo)
      (if (string-match-p whitespace-trailing-regexp
                          org-capture-derived-todo)
          org-capture-derived-todo
        (format "%s " org-capture-derived-todo))
    ""))

(defun org-capture-derived-insert-deadline ()
  ""
  (if (numberp org-capture-derived-deadline-in-day)
      (let ((week-ahead (encode-time
                         (seq-map-indexed (lambda (element index)
                                            (if (= index 3)
                                                (+ element org-capture-derived-deadline-in-day)
                                              element))
                                          (decode-time (current-time))))))
        (format-time-string "DEADLINE: <%Y-%m-%d %a>" week-ahead))
    ""))
