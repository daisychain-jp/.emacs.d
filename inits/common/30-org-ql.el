(use-package org-ql
  :straight t
  :custom
  (org-ql-search-directories-files-recursive t)
  :config
  (require 'org-ql-search))

(org-ql-defpred (tags-expanded expanded-tags tags-x xtags) (&rest tags)
  "Return non-nil if current heading has one or more of TAGS.
If TAGS contains a group tag, all tags in the group is used to match.
Both inherited and local tags is tested."
  :normalizers ((`(,predicate-names . ,tags)
                 `(tags-expanded ,@tags)))
  :body (apply #'org-ql--predicate-tags
               (seq-uniq (--mapcat (org-tags-expand it t)
                                   tags))))

(org-ql-defpred (category-inherited) (&rest categories)
  "Return non-nil if current heading has CATEGORY.
Ancestors are looked up If current heading has no CATEGORY."
  :body (when-let ((category (or (org-get-category (point))
                                 (org-entry-get (point) "CATEGORY" t))))
          (cl-typecase categories
            (null t)
            (otherwise (member category categories)))))
