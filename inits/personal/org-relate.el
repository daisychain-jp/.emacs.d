(require 'thingatpt)
(require 'org-ql-search)

(defconst org-relate-property "PARENT"
  "Property name for child nodes to look up parent node.")
(defcustom org-relate-parent-tag-list
  '("project" "refile")
  "A list of tags all parent nodes must have one of them.")
(defcustom org-relate-target-files
  (file-expand-wildcards (concat env-org-dir "/**/*.org"))
  "A list of target files to search relation nodes.")

(with-eval-after-load 'org
  (bind-key "C-c %" #'org-relate-search
            org-mode-map))

(with-eval-after-load 'org-keys
  (setf (map-elt org-speed-commands "%") #'org-relate-search)
  (setf (map-elt org-speed-commands "&") #'org-relate-interrelate)
  (push '("Org-Relate Operation") org-speed-commands))

(with-eval-after-load 'org-agenda
  (bind-keys :map org-agenda-mode-map
             ("&" . org-agenda-relation-interrelate)))

(defun org-relate-search ()
  "Search for entries related to the entry at point.

User who would like to search for parent and children of that, namely siblings,
 must press 'p'.
User who would like to search for children of this entry must press 'c'.
User who would like to search for friends either pointing to this entry
 or pointed from this entry must be press 'f'.
User who would like to search for all listed above must press 'a'."
  (interactive)
  (let* ((org-super-agenda-properties-inherit nil)
         (char (read-char-from-minibuffer "Target: [a]ll [p]arent [c]hild [f]riend"))
         (queries (seq-remove 'null
                              (cond
                               ((equal char ?a) (list (org-relate--search-parent-org-ql-query)
                                                      (org-relate--search-children-org-ql-query)
                                                      (org-relate--search-friends-refer-org-ql-query)
                                                      (org-relate--search-friends-referred-org-ql-query)
                                                      (org-relate--search-siblings-org-ql-query)))
                               ((equal char ?p) (list (org-relate--search-parent-org-ql-query)
                                                      (org-relate--search-siblings-org-ql-query)))
                               ((equal char ?c) (list (org-relate--search-children-org-ql-query)))
                               ((equal char ?f) (list (org-relate--search-friends-refer-org-ql-query)
                                                      (org-relate--search-friends-referred-org-ql-query)))))))
    (if queries
        (org-ql-search org-relate-target-files
          (seq-reduce (lambda (accum elem)
                        (push elem (cdr (last accum)))
                        accum)
                      (mapcar 'car queries)
                      '(or))
          :super-groups (mapcar 'cdr queries))
      (message "No query found"))))

(defun org-relate--search-children-org-ql-query ()
  (when-let* ((id (org-id-get))
              (rel-prop-link (format "[[id:%s]]" id)))
    `((or (property ,org-relate-property ,id)
          (property ,org-relate-property ,rel-prop-link)) .
          (:name "Children" :property (,org-relate-property ,id)))))

(defun org-relate--search-parent-org-ql-query ()
  (if-let* ((prop-ref (org-entry-get (point) org-relate-property))
            (index (string-match (format "\\(%s\\)" thing-at-point-uuid-regexp) prop-ref))
            (parent-id (match-string 0 prop-ref)))
      `((property "ID" ,parent-id) .
        (:name "Parent" :property ("ID" ,parent-id)))))

(defun org-relate--search-siblings-org-ql-query ()
  (if-let* ((prop-ref (org-entry-get (point) org-relate-property))
            (index (string-match (format "\\(%s\\)" thing-at-point-uuid-regexp) prop-ref))
            (parent-id (match-string 0 prop-ref))
            (parent-id-link (format "[[id:%s]]" parent-id)))
      `((or (property ,org-relate-property ,parent-id)
            (property ,org-relate-property ,parent-id-link)) .
            (:name "Siblings" :property (,org-relate-property ,parent-id)))))

(defun org-relate--search-friends-refer-org-ql-query ()
  (when-let* ((id (org-id-get))
              (ref-re (format "\\[\\[id:%s\\].*\\]" id)))
    `((regexp ,ref-re) .
      (:name "Friends pointing to this entry" :regexp ,ref-re))))

(defun org-relate--search-friends-referred-org-ql-query ()
  (let* ((id-link-re (format "\\[\\[id:\\(%s\\)\\].*\\]" thing-at-point-uuid-regexp))
         (end (or (save-excursion
                    (outline-next-heading))
                  (point-max))))
    (when (save-excursion
            (re-search-forward id-link-re end t))
      `((property "ID" ,(match-string 1)) .
        (:name "Friends to whom this entry is pointing" :property ("ID" ,(match-string 1)))))))

(defun org-relate-interrelate ()
  "Make parent-child relationship by inserting property.

This command must be called in parent node which should have one of `org-relate-parent-tag-list'."
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (if (some (lambda (parent-tag)
                (member parent-tag (org-get-tags)))
              org-relate-parent-tag-list)
        (let ((ref-id (org-id-get-create)))
          (and (org-goto-first-child)
               (cl-labels ((set-ref-id-to-siblings
                            (ref-link)
                            (org-set-property org-relate-property ref-link)
                            (and (org-goto-sibling)
                                 (set-ref-id-to-siblings ref-link))))
                 (set-ref-id-to-siblings ref-id)))
          (message "Interrelated has done."))
      (message "This entry does not compliant with 'org-relate-parent-tag-list"))))

(defun org-agenda-relation-interrelate ()
  "Invoke `org-relate-interrelate' in agenda view."
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
        (call-interactively 'org-relate-interrelate)
        (end-of-line 1)
        (setq newhead (org-get-heading))))))
