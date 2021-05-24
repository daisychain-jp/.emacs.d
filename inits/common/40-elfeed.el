(use-package elfeed
  :straight t
  :delight
  (elfeed-show-mode   " EF")
  (elfeed-search-mode " EF")
  :commands (elfeed)
  :init
  (defface elfeed-search-buffer `((t . (:font "fontset-default"
                                              :height ,(my-adjust-font-size 800))))
    "Default face for elfeed search buffer."
    :group 'elfeed)
  (defface elfeed-show-buffer `((t . (:font "fontset-variable"
                                            :height ,(my-adjust-font-size 835))))
    "Default face for elfeed show buffer."
    :group 'elfeed)
  (defface elfeed-search-unchecked-title-face
    nil
    "Face used in search mode for unchecked entry titles."
    :group 'elfeed)
  (defface elfeed-search-checked-title-face
    nil
    "Face used in search mode for checked entry titles."
    :group 'elfeed)
  :hook
  (elfeed-search-update . (lambda ()
                            (buffer-face-set 'elfeed-search-buffer)
                            (setq-local line-spacing 0.1)))
  :custom
  (elfeed-use-curl t)
  (elfeed-curl-program-name (expand-file-name "curl" "~/usr/bin"))
  (elfeed-db-directory (concat env-var-dir "/lib/elfeed/db"))
  (elfeed-search-date-format '("%Y%m%d" 8 :left))
  (elfeed-search-title-min-width 100)
  (elfeed-search-title-max-width 120)
  (elfeed-search-trailing-width 160)
  (elfeed-show-refresh-function
   (lambda ()
     (interactive)
     (buffer-face-set 'elfeed-show-buffer)
     (elfeed-show-refresh--mail-style)))
  (elfeed-sort-order 'ascending)
  (elfeed-enclosure-default-dir "~/Downloads/")
  (elfeed-save-multiple-enclosures-without-asking t)
  :config
  (setf elfeed-search-print-entry-function
        'my/elfeed-search-print-entry)
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
             ("o" . elfeed-search-open-url)
             ("C-c C-o" . elfeed-search-open-url)
             ("f" . scroll-up-line)
             ("e" . scroll-down-line)
             ("q" . elfeed-kill-buffer)
             ("Q" . quit-window)
             ("x" . elfeed-search-update--force)
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
                      (elfeed-search-tag-all-checked)
                      (unless (use-region-p) (forward-line -1))
                      (elfeed-search-tag-all (intern (format-time-string "%Y%m%d")))))
             ("R" . elfeed-search-untag-all-checked)
             ("d" . elfeed-search-untag-all-unread)
             ("Dv" . elfeed-search-download-video)
             ("Da" . elfeed-search-download-audio)
             ("D=" . elfeed-search-show-media-duration)
             ("X" . elfeed-search-org-capture-derived)
             :map elfeed-show-mode-map
             ("C-i" . shr-next-link))
  (add-to-list 'elfeed-search-face-alist '(unchecked elfeed-search-unchecked-title-face))
  (add-to-list 'elfeed-search-face-alist '(checked elfeed-search-checked-title-face)))

(use-package elfeed-org
  :straight t
  :after (elfeed)
  :custom
  (rmh-elfeed-org-files (list (format "%s/index/feed.org" env-org-dir)))
  (rmh-elfeed-org-ignore-tag "ignore")
  (rmh-elfeed-org-auto-ignore-invalid-feeds t)
  :config
  (elfeed-org))

(defun my/elfeed-search-print-entry (entry)
  "Print ENTRY to the buffer with my style."
  (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
         (title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
         (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
         (feed (elfeed-entry-feed entry))
         (feed-title
          (when feed
            (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
         (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
         (tags-str (mapconcat
                    (lambda (s) (propertize s 'face 'elfeed-search-tag-face))
                    tags ","))
         (title-width (- (window-width) 10 elfeed-search-trailing-width))
         (title-column (elfeed-format-column
                        title (elfeed-clamp
                               elfeed-search-title-min-width
                               title-width
                               elfeed-search-title-max-width)
                        :left)))
    (when feed-title
      (insert (propertize (elfeed-format-column feed-title 6 :left)
                          'face 'elfeed-search-feed-face) " "))
    (insert (propertize title-column 'face title-faces 'kbd-help title) " ")
    (insert (propertize date 'face 'elfeed-search-date-face) " ")
    (when tags
      (insert "(" tags-str ")"))))

(defun elfeed-search-open-url ()
  "Visit the current entry in your browser using 'eww-browse-url'."
  (interactive)
  (let ((browse-url-browser-function 'open-url))
    (elfeed-search-untag-all-unchecked)
    (unless (use-region-p) (forward-line -1))
    (elfeed-search-tag-all-checked)
    (unless (use-region-p) (forward-line -1))
    (elfeed-search-tag-all (intern (format-time-string "%Y%m%d")))
    (unless (use-region-p) (forward-line -1))
    (elfeed-search-browse-url)))

(defun elfeed-search-download-video ()
  "Downlaod video file."
  (interactive)
  (let ((entries (elfeed-search-selected)))
    (cl-loop for entry in entries
             when (or (caar (elfeed-entry-enclosures entry))
                      (elfeed-entry-link entry))
             do (let ((title (elfeed-entry-title entry)))
                  (download-video it title)))
    (mapc #'elfeed-search-update-entry entries)
    (unless (or elfeed-search-remain-on-entry (use-region-p))
      (forward-line))))

(defun elfeed-search-download-audio ()
  "Download audio file."
  (interactive)
  (let ((entries (elfeed-search-selected)))
    (cl-loop for entry in entries
             when (or (caar (elfeed-entry-enclosures entry))
                      (elfeed-entry-link entry))
             do (let ((title (elfeed-entry-title entry)))
                  (download-audio it title)))
    (mapc #'elfeed-search-update-entry entries)
    (unless (or elfeed-search-remain-on-entry (use-region-p))
      (forward-line))))

(defun elfeed-search-show-media-duration ()
  "Show duration of media attached to current entry."
  (interactive)
  (let* ((entry (elfeed-search-selected :single))
         (url (or (caar (elfeed-entry-enclosures entry))
                  (elfeed-entry-link entry))))
    (show-media-duration url)))

(defun elfeed-search-org-capture-derived (&optional arg)
  "Create actionable org task derived from current elfeed entry.

If called with C-u prefix, ask user to specify tags and target."
  (interactive "P")
  (let ((entries (elfeed-search-selected)))
    (save-window-excursion
      (cl-loop for entry in entries
               do (progn
                    (elfeed-show-entry entry)
                    (let ((org-capture-derived-todo org-todo-keyword-3)
                          (org-capture-derived-tags (format ":web:%s"
                                                            (cond
                                                             ((or (member 'video (elfeed-entry-tags entry))
                                                                  (member 'audio (elfeed-entry-tags entry)))
                                                              "ac_watch:")
                                                             (t "ac_read:"))))
                          (org-capture-derived-target "68d74115-1f70-448d-a76e-738e32b272d8")
                          (org-capture-derived-deadline-in-day 7))
                      (call-interactively #'org-capture-derived))
                    (elfeed-kill-buffer))))
    (elfeed-search-untag-all-unchecked)
    (unless (use-region-p) (forward-line -1))
    (elfeed-search-tag-all-checked)
    (mapc #'elfeed-search-update-entry entries)))
