(defcustom my/view-file--temp-dir "~/var/tmp/view-file"
  "Directory in which we will work for uncompresssing, decrypting and htmlize.")
(defvar my/view-file--open-file nil
  "Target file which we will actually open.
User should not change this value.")
(defvar my/view-file--remove-files nil
  "List of files which we will remove afterward.
User should not change this value.")

;; TODO: support line/search args to jump to the desired position
(defun my/view-file (file &optional system)
  "Open FILE in my preferable way.

if SYSTEM is non-nii open FILE using preferable application in system."
  (setq my/view-file--open-file (expand-file-name file))
  (make-directory my/view-file--temp-dir t)

  ;; decrypt file if it is encrypted
  (when-let* ((match (string-match "\\(.+\\)\.gpg$" my/view-file--open-file))
              (decrypt-file (expand-file-name (file-name-base my/view-file--open-file)
                                              (expand-file-name my/view-file--temp-dir)))
              (context (epg-make-context epa-protocol)))
    (epg-context-set-passphrase-callback context
                                         #'epa-passphrase-callback-function)
    (epg-decrypt-file context my/view-file--open-file decrypt-file)
    (setq my/view-file--open-file decrypt-file)
    (add-to-list 'my/view-file--remove-files decrypt-file))

  ;; uncompress file if it is tarball
  (let* (uncompress-file
         (uncompress-command
          (cond
           ((string-match "\\(.+\\)\\.t\\(ar\\.\\)?gz$" my/view-file--open-file)
            (setq uncompress-file (expand-file-name (file-name-base (match-string 1 my/view-file--open-file))
                                                    my/view-file--temp-dir))
            (format "gzip -dc %s | tar -xf - -C %s"
                    (shell-quote-argument my/view-file--open-file)
                    (shell-quote-argument uncompress-file)))
           ((string-match "\\(.+\\)\\.t\\(ar\\.\\)?zst$" my/view-file--open-file)
            (setq uncompress-file (expand-file-name (file-name-base (match-string 1 my/view-file--open-file))
                                                    my/view-file--temp-dir))
            (format "unzstd -c %s | tar -xf - -C %s"
                    (shell-quote-argument my/view-file--open-file)
                    (shell-quote-argument uncompress-file)))
           ((string-match "\\(.+\\)\\.zip$" my/view-file--open-file)
            (setq uncompress-file (expand-file-name (file-name-base (match-string 1 my/view-file--open-file))
                                                    my/view-file--temp-dir))
            (format "unzip -o -d %s %s"
                    (shell-quote-argument uncompress-file)
                    (shell-quote-argument my/view-file--open-file))))))
    (when (and (stringp uncompress-file)
               (stringp uncompress-command))
      (make-directory uncompress-file t)
      (call-process-shell-command
       uncompress-command)
      (add-to-list 'my/view-file--remove-files uncompress-file)
      (setq my/view-file--open-file uncompress-file)))

  ;; convert pdf/epub to html
  (let* ((dirname-html
          (expand-file-name (file-relative-name my/view-file--open-file "~")
                            (expand-file-name my/view-file--temp-dir)))
         (filename-html
          (concat (file-name-as-directory dirname-html)
                  "index.html"))
         (command
          (cond
           ((= (call-process-shell-command (format "filetype-cli check --type pdf %s" (shell-quote-argument my/view-file--open-file))) 0)
            (format "cat %s 2>/dev/null | pdftohtml -p -s -noframes -nomerge -nodrm - %s"
                    (shell-quote-argument my/view-file--open-file)
                    (shell-quote-argument filename-html)))
           ((= (call-process-shell-command (format "filetype-cli check --type epub %s" (shell-quote-argument my/view-file--open-file))) 0)
            (format "cat %s 2>/dev/null | pandoc --from=epub --to=html --extract-media=%s - | strip_ruby > %s"
                    (shell-quote-argument my/view-file--open-file)
                    (shell-quote-argument dirname-html)
                    (shell-quote-argument filename-html))))))
    (when (and (null system)
               (stringp command))
      (unless (file-exists-p filename-html)
        (make-directory dirname-html t)
        (call-process-shell-command command)
        ;; reuse html only if pdf/epub file is not ecrypted
        (when (member my/view-file--open-file my/view-file--remove-files)
          (add-to-list 'my/view-file--remove-files dirname-html)))
      (setq my/view-file--open-file filename-html)))

  ;; open file
  (cond
   ((or system
        (string-match-p "\\.midi?$" my/view-file--open-file)
        (= (call-process-shell-command (format "filetype-cli check --type pdf %s" (shell-quote-argument my/view-file--open-file))) 0)
        (= (call-process-shell-command (format "filetype-cli check --type epub %s" (shell-quote-argument my/view-file--open-file))) 0))
    (let ((process-connection-type nil))
      (call-process-shell-command (format "xdg-open %s" (shell-quote-argument my/view-file--open-file)))
      (sleep-for 3)
      (funcall 'my/view-file--finalize-function)))
   ((or (= (call-process-shell-command (format "filetype-cli check --type playable %s" (shell-quote-argument my/view-file--open-file))) 0)
        (seq-some (lambda (suffix)
                    (string-suffix-p suffix my/view-file--open-file t))
                  '(".m3u" ".mts" ".m2ts")))
    (when-let ((mpv-proc (start-process-shell-command
                          "mpv" nil
                          (format "nohup mpv --force-window %s >/dev/null 2>&1" (shell-quote-argument my/view-file--open-file)))))
      (set-process-sentinel
       mpv-proc
       (lambda (process desc)
         (when (eq (process-status process) 'exit)
           (funcall 'my/view-file--finalize-function))))))
   ((= (call-process-shell-command
        (format "filetype-cli check --type html %s"
                (shell-quote-argument my/view-file--open-file))) 0)
    (add-hook 'eww-after-render-hook 'my/view-file--eww-finalize-function)
    (eww-open-file my/view-file--open-file))
   ((file-directory-p my/view-file--open-file)
    (dired my/view-file--open-file)
    (funcall 'my/view-file--finalize-function))
   (t
    (find-file my/view-file--open-file)
    (funcall 'my/view-file--finalize-function))))

(defun my/view-file--finalize-function ()
  "Remove files and initialize variables."
  (mapc (lambda (file)
          (when (stringp file)
            (cond
             ((file-directory-p file)
              (delete-directory file t nil))
             ((file-exists-p file)
              (let ((delete-by-moving-to-trash t))
                (delete-file file))))))
        my/view-file--remove-files)
  (setq my/view-file--open-file nil
        my/view-file--remove-files nil))

(defun my/view-file--eww-finalize-function ()
  "Finalize function for `EWW'."
  (remove-hook 'eww-after-render-hook 'my/view-file--eww-finalize-function)
  (funcall 'my/view-file--finalize-function))

(defun my/view-file-filter-org-link-store-props (&rest plist-orig)
  "Filter PLIST-ORIG to original file name if value of `eww-current-url'
 is in `my/view-file--temp-dir'."
  (let* ((plist-filtered (car plist-orig)))
    (plist-put plist-filtered
               :link
               (replace-regexp-in-string (format "%s\\(.*\\)/index.html"
                                                 (expand-file-name my/view-file--temp-dir))
                                         (format "%s\\1"
                                                 (expand-file-name "~"))
                                         (plist-get plist-filtered
                                                    :link)))))

(advice-add #'org-link-store-props :filter-args
            #'my/view-file-filter-org-link-store-props)

(provide 'my/view-file)
