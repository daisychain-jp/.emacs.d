(use-package elfeed
  :straight t
  :delight
  (elfeed-show-mode   " EF")
  (elfeed-search-mode " EF")
  :commands (elfeed)
  :custom
  (elfeed-db-directory (concat env-var-dir "/lib/elfeed/db"))
  (elfeed-search-date-format '("%m%d" 4 :left))
  (elfeed-search-title-min-width 34)
  (elfeed-search-trailing-width 16)
  (elfeed-show-refresh-function
   (lambda ()
     (interactive)
     (buffer-face-set 'variable-pitch)
     (elfeed-show-refresh--mail-style)))
  (elfeed-search-header-function
   (lambda ()
     (interactive)
     (buffer-face-set 'visible)
     (setq-local line-spacing 0.1)))
  (elfeed-sort-order 'ascending)
  :config
  (defalias 'elfeed-search-tag-all-unchecked
    (elfeed-expose #'elfeed-search-tag-all 'unchecked)
    "Add the `unchecked' tag to all selected entries.")
  (defalias 'elfeed-search-untag-all-unchecked
    (elfeed-expose #'elfeed-search-untag-all 'unchecked)
    "Remove the `unchecked' tag from all selected entries.")
  (defalias 'elfeed-search-tag-all-checked
    (elfeed-expose #'elfeed-search-tag-all 'checked)
    "Add the `checked' tag to all selected entries.")
  (defalias 'elfeed-search-untag-all-checked
    (elfeed-expose #'elfeed-search-untag-all 'checked)
    "Remove the `checked' tag from all selected entries.")
  (bind-keys :map elfeed-search-mode-map
             ("C-j" . elfeed-search-show-entry)
             ("f" . scroll-up-line)
             ("e" . scroll-down-line)
             ("q" . quit-window)
             ("Q" . elfeed-kill-buffer)
             ("c" . (lambda () (interactive)
                      (elfeed-search-untag-all-unread)
                      (unless (use-region-p) (forward-line -1))
                      (elfeed-search-tag-all-unchecked)))
             ("C" . elfeed-search-untag-all-unchecked)
             ("r" . (lambda () (interactive)
                      (elfeed-search-untag-all-unread)
                      (unless (use-region-p) (forward-line -1))
                      (elfeed-search-untag-all-unchecked)
                      (unless (use-region-p) (forward-line -1))
                      (elfeed-search-tag-all-checked)))
             ("R" . elfeed-search-untag-all-checked)
             ("d" . (lambda () (interactive)
                      (elfeed-search-untag-all-unread)
                      (unless (use-region-p) (forward-line -1))
                      (elfeed-search-untag-all-unchecked))))
  (defface elfeed-search-unchecked-title-face
    '((t :weight bold))
    "Face used in search mode for unchecked entry titles."
    :group 'elfeed)
  (add-to-list 'elfeed-search-face-alist '(unchecked elfeed-search-unchecked-title-face))
  (defface elfeed-search-checked-title-face
    '((t :weight bold))
    "Face used in search mode for checked entry titles."
    :group 'elfeed)
  (add-to-list 'elfeed-search-face-alist '(checked elfeed-search-checked-title-face)))

(use-package elfeed-org
  :straight t
  :after (elfeed)
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list (format "%s/library/feed.org" env-doc-dir)))
  (setq rmh-elfeed-org-ignore-tag "ARCHIVE")
  (setq rmh-elfeed-org-auto-ignore-invalid-feeds t))
