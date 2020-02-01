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
  (elfeed-enclosure-default-dir "~/Downloads/")
  (elfeed-save-multiple-enclosures-without-asking t)
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
             ("C-o" . elfeed-search-show-entry)
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

(defun my/elfeed-search-download-video ()
  "Downlaod video file."
  (interactive)
  (if-let* ((entry (elfeed-search-selected :single))
            (title (replace-regexp-in-string "[/:|]" "" (elfeed-entry-title entry)))
            (link (elfeed-entry-link entry))
            (match-index (string-match "https?://www.youtube.com.+" link))
            (yt-url (match-string 0 link)))
      (start-process-shell-command "ytdl" nil (format "cd ~/Videos && youtube-dl -f \"bestvideo[height<=?720]+bestaudio/best\" \"%s\"" yt-url))))

(defun my/elfeed-search-download-audio ()
  "Download audio file."
  (interactive)
  (let* ((entry (elfeed-search-selected :single))
         (title (elfeed-entry-title entry))
         (link (elfeed-entry-link entry))
         (enc-url (caar (elfeed-entry-enclosures entry)))
         (yt-url (when (string-match "https?://www\\.youtube\\.com.+" link)
                   (match-string 0 link)))
         url)
    (when (setq url (or enc-url yt-url))
      (princ (format "Donwloading from %s" url))
      (setq title (replace-regexp-in-string "[/:|]" "" title))
      (start-process-shell-command "ytdl" nil
                                   (format "cd ~/Music && youtube-dl \"%s\" -o - | ffmpeg -i - %s -vn \"%s\".mp3"
                                           url
                                           (if (equal '(undecided) (find-coding-systems-string title))
                                               "-filter:a \"atempo=1.1\""
                                             "-filter:a \"atempo=1.6\"")
                                           title)))))
