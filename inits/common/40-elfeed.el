(use-package elfeed
  :straight t
  :delight
  (elfeed-show-mode   " EF")
  (elfeed-search-mode " EF")
  :commands (elfeed)
  :hook
  (elfeed-search-update . (lambda ()
                            (buffer-face-set 'visible)
                            (setq-local line-spacing 0.1)))
  :custom
  (elfeed-db-directory (concat env-var-dir "/lib/elfeed/db"))
  (elfeed-search-date-format '("%y%m%d" 6 :left))
  (elfeed-search-title-min-width 100)
  (elfeed-search-title-max-width 120)
  (elfeed-search-trailing-width 160)
  (elfeed-show-refresh-function
   (lambda ()
     (interactive)
     (buffer-face-set 'variable-pitch)
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
             ("C-o" . elfeed-search-open-url)
             ("f" . scroll-up-line)
             ("e" . scroll-down-line)
             ("q" . quit-window)
             ("Q" . elfeed-kill-buffer)
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
                      (elfeed-search-tag-all-checked)))
             ("R" . elfeed-search-untag-all-checked)
             ("d" . elfeed-search-untag-all-unread)
             ("V" . my/elfeed-search-download-video)
             ("A" . my/elfeed-search-download-audio)
             :map elfeed-show-mode-map
             ("C-i" . shr-next-link))
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
  :custom
  (rmh-elfeed-org-files (list (format "%s/library/feed.org" env-doc-dir)))
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
      (insert (propertize (elfeed-format-column feed-title 4 :left)
                          'face 'elfeed-search-feed-face) " "))
    (insert (propertize title-column 'face title-faces 'kbd-help title) " ")
    (insert (propertize date 'face 'elfeed-search-date-face) " ")
    (when tags
      (insert "(" tags-str ")"))))

(defun elfeed-search-open-url ()
  "Visit the current entry in your browser using 'eww-browse-url'."
  (interactive)
  (let ((browse-url-browser-function 'open-url))
    (elfeed-search-browse-url)))

(defun my/elfeed-search-download-video ()
  "Downlaod video file."
  (interactive)
  (let* ((entry (elfeed-search-selected :single))
         (title (replace-regexp-in-string "[/:|]" "" (elfeed-entry-title entry)))
         (link (elfeed-entry-link entry))
         (match-index (string-match "https?://www.youtube.com.+" link))
         (yt-url (match-string 0 link)))
    (princ (format "Donwloading %s" title))
    (start-process-shell-command "ytdl" nil (format "cd ~/Videos && youtube-dl -f \"%1$s\" \"%2$s\"" (youtube-dl-format) yt-url))))

(defun my/elfeed-search-download-audio ()
  "Download audio file."
  (interactive)
  (let* ((entry (elfeed-search-selected :single))
         (title (elfeed-entry-title entry))
         (link (elfeed-entry-link entry))
         (enc-url (caar (elfeed-entry-enclosures entry)))
         (yt-url (when (string-match "https?://www\\.youtube\\.com.+" link)
                   (match-string 0 link)))
         (work-dir "~/Music/")
         (temp-fname (make-temp-name work-dir)))
    (when (or enc-url yt-url)
      (princ (format "Donwloading %s" title))
      (setq title (replace-regexp-in-string "[\\?/:|’]" "" title))
      (start-process-shell-command "ytdl" nil
                                   (format "%s; cd %s && ffmpeg -i %s %s -vn \"%s.mp3\"; rm -f %s"
                                           (if enc-url
                                               (format "curl -LJs \"%s\" -o %s" enc-url temp-fname)
                                             (format "youtube-dl --extract-audio --audio-format mp3 \"%1$s\" -o %2$s.mp3; mv -f %2$s.mp3 %2$s" yt-url temp-fname))
                                           work-dir
                                           temp-fname
                                           (if (equal '(undecided) (find-coding-systems-string title))
                                               "-filter:a \"atempo=1.1\""
                                             "-filter:a \"atempo=1.7\"")
                                           title
                                           temp-fname)))))
