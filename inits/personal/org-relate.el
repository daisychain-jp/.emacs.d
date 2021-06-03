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

(with-eval-after-load 'org-keys
  (setf (map-elt org-speed-commands "%") #'org-relate-look)
  (setf (map-elt org-speed-commands "&") #'org-relate-interrelate)
  (push '("Org-Relate Operation") org-speed-commands))

(with-eval-after-load 'org-agenda
  (bind-keys :map org-agenda-mode-map
             ("&" . org-agenda-relation-interrelate)))

(defun org-relate-look ()
  "Entry command for org-relate-look-up, org-relate-look-down
and org-relate-look-around."
  (interactive)
  (let ((char (read-char-from-minibuffer "Select look-[u]p/[d]own/a[r]ound")))
    (cond
     ((equal char ?u) (org-relate-look-up))
     ((equal char ?d) (org-relate-look-down))
     ((equal char ?r) (org-relate-look-around)))))

(defun org-relate-look-down ()
  "Show all child nodes of this entry."
  (interactive)
  (if-let* ((id (org-id-get))
            (rel-prop-link (format "[[id:%s]]" id)))
      (org-ql-search
        org-relate-target-files
        `(or (property ,org-relate-property ,id)
             (property ,org-relate-property ,rel-prop-link)))
    (message "This entry has no 'ID' property")))

(defun org-relate-look-up ()
  "Show parent node and all child nodes of it (siblings)."
  (interactive)
  (let ((org-super-agenda-properties-inherit nil))
    (if-let* ((prop-ref (org-entry-get (point) org-relate-property))
              (index (string-match (format "\\(%s\\)" thing-at-point-uuid-regexp) prop-ref))
              (parent-id (match-string 0 prop-ref))
              (parent-id-link (format "[[id:%s]]" parent-id)))
        (org-ql-search
          org-relate-target-files
          `(or (property "ID" ,parent-id)
               (property ,org-relate-property ,parent-id)
               (property ,org-relate-property ,parent-id-link)) ; for backward compatibility)
          :super-groups `((:name "Parent" :property ("ID" ,parent-id))))
      (message "This entry has no parent"))))

(defun org-relate-look-around ()
  "Show all entries refer to this node by having an org-link which contain this node's ID."
  (interactive)
  (if-let* ((id (org-id-get))
            (ref-re (format "\\[\\[id:%s\\].*\\]" id)))
      (org-ql-search
        org-relate-target-files
        `(regexp ,ref-re))
    (message "This entry has no 'ID' property")))

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
