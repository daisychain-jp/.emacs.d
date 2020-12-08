;; get a face variable name relevant with the pinted word
(defun describe-face-at-point ()
  (interactive)
  (message "%s" (get-char-property (point) 'face)))

(defun copy-line-number ()
  "Copy the line number of current point into the kill ring."
  (interactive)
  (let ((line-number (number-to-string (line-number-at-pos))))
    (message "Line number: %s" line-number)
    (kill-new line-number)))
(bind-keys ("C-M-S-w" . copy-line-number))

(defun open-thing-at-point ()
  "Open thing with appropriate manner."
  (interactive)
  (let* ((thing nil)
         (region-thing (when (use-region-p)
                         (buffer-substring (region-beginning) (region-end)))))
    (cond
     ((setq thing
            (if region-thing
                (when (string-match thing-at-point-email-regexp
                                    region-thing)
                  (match-string 0 region-thing))
              (thing-at-point 'email t)))
      (mu4e~compose-mail thing)
      t)
     ((setq thing (car (eww-links-at-point)))
      (cl-case (prefix-numeric-value current-prefix-arg)
        (16 (browse-url-default-browser thing))
        (4 (eww-browse-url thing))
        (t (open-url thing)))
      t)
     ((setq thing
            (if region-thing
                (when (string-match browse-url-button-regexp
                                    region-thing)
                  (match-string 0 region-thing))
              (thing-at-point 'url t)))
      (let ((url-pos (split-location-uri thing)))
        (cl-case (prefix-numeric-value current-prefix-arg)
          (16 (browse-url-default-browser (car url-pos)))
          (4 (eww-browse-url (car url-pos)))
          (t (open-url (car url-pos) (cadr url-pos))))
        t))
     ((and (setq thing
                 (if region-thing
                     (when (string-match (format "[%s].+" thing-at-point-file-name-chars)
                                         region-thing)
                       (match-string 0 region-thing))
                   (thing-at-point 'filename t)))
           (file-exists-p thing))
      (let ((url-pos (split-location-uri thing)))
        (cl-case (prefix-numeric-value current-prefix-arg)
          (16 (open-file-external (car url-pos)))
          (4 (find-file (car url-pos)))
          (t (open-file (car url-pos))
             (goto-pos (cadr url-pos))))
        t))
     ;; this clause must be last in cond
     ((setq thing (button-at (point)))
      (push-button)
      t))))

(defvar open-file-work-dir "~/var/tmp/exfile")

(defun open-file (file)
  "Open file `FILE' with appropriate application."
  (let ((ex-file (expand-file-name file))
        (rm-file nil))
    (let ((is-tar (string-suffix-p ".tar.gpg" ex-file))
          (is-gzip (string-match-p "\\.t\\(ar\\.\\)?gz\\.gpg$" ex-file)))
      (when (or is-tar is-gzip)
        (call-process-shell-command
         (format "mkdir -p %s; gpg -d %s | tar -x %s -C %s"
                 (setf rm-file (shell-quote-argument
                                (expand-file-name (make-temp-name
                                                   (file-name-as-directory open-file-work-dir)))))
                 ex-file
                 (cond (is-gzip "-z")
                       (t ""))
                 rm-file))
        (setf ex-file rm-file)))
    (when (string-suffix-p ".mp4.gpg" ex-file)
      (call-process-shell-command
       (format "gpg -o %s -d %s"
               (setf rm-file (expand-file-name (concat (make-temp-name
                                                        (file-name-as-directory open-file-work-dir))
                                                       ".mp4")))
               (shell-quote-argument ex-file)))
      (setf ex-file rm-file))
    (cond
     ((string-match-p "\\.midi?$" ex-file)
      (call-process-shell-command (format "xdg-open %s" (shell-quote-argument ex-file))))
     ((or (= (call-process-shell-command (format "filetype-cli check --type playable %s" (shell-quote-argument ex-file))) 0)
          (string-suffix-p ".m3u" ex-file))
      (start-process-shell-command "mpv" nil
                                   (concat (format "nohup mpv --force-window %s >/dev/null 2>&1;" (shell-quote-argument ex-file))
                                           (if rm-file (format "rm -rf %s" (shell-quote-argument rm-file)) nil))))
     ((= (call-process-shell-command (format "filetype-cli check --type html %s" (shell-quote-argument ex-file))) 0)
      (eww-open-file ex-file))
     ((or (= (call-process-shell-command (format "filetype-cli check --type pdf %s" (shell-quote-argument ex-file))) 0)
          (= (call-process-shell-command (format "filetype-cli check --type epub %s" (shell-quote-argument ex-file))) 0))
      (open-uri-htmlize ex-file))
     ((file-directory-p ex-file) (dired ex-file))
     (t (find-file ex-file)))))

(defun open-file-external (file)
  "Open file `FILE' in external application.
Generally preferance application is used."
  (let ((ex-file (expand-file-name file))
        (process-connection-type nil))
    (start-process-shell-command "xdg-open" nil (format "xdg-open %s" (shell-quote-argument ex-file)))))

(defun split-location-uri (location-uri)
  "Split LOCATION-URI into normal uri and location specifier.

Location specifier is either line number or string.
If splitting is successful, this function returns a list containing uri and location specifier."
  (cond
   ((string-match "\\(.+\\)::\\([[:digit:]]+\\)$" location-uri)
    (list (match-string 1 location-uri) (string-to-number (match-string 2 location-uri))))
   ((string-match "\\(.+\\)::\\([[:graph:][:blank:]]+\\)$" location-uri)
    (list (match-string 1 location-uri) (match-string 2 location-uri)))
   (t (list location-uri))))

(defun goto-pos (pos)
  "Move the cursor to the location where `POS' is pointing.

if `POS' is a number assumed to be a line number.
If `POS' is a string assumed to be a searching word."
  (interactive)
  (when (cond
         ((numberp pos)
          (forward-line (- pos (line-number-at-pos)))
          t)
         ((stringp pos)
          (let ((pos-num (string-to-number pos)))
            (cl-case pos-num
              (0 (goto-char
                  (re-search-forward
                   (string-join (split-string pos "" t "[ \t\r\n]*") "[ \t\r\n]*") nil nil)))
              (t (forward-line (- pos-num (line-number-at-pos))))))
          t))
    (beginning-of-line)
    (recenter-top-bottom 0)))

(defun open-url (url &optional pos)
  "Open URL in an appropriate manner and jump to POS.

If URL points to a multimedia contents such as youtube video and mp3 audio file,
play it in media player."
  (cond
   ((or (string-suffix-p ".pdf" url)
        (string-suffix-p ".epub" url))
    (open-uri-htmlize url)
    (when pos (goto-pos pos)))
   ((string-match-p "^https?://www.youtube.com.*" url)
    ;; convert greedily watch url to playlist url if possible
    (let ((url (if (string-match "&list=\\([[:graph:]]+\\)" url)
                   (format "https://www.youtube.com/playlist?list=%s" (match-string 1 url))
                 url))
          (ytdl-opts
           (remove nil
                   (list (when (numberp pos)
                           (format "--ytdl-raw-options=playlist-start=%d" pos))
                         (format "--ytdl-format=%s" (shell-quote-argument (youtube-dl-format)))))))
      (start-process-shell-command "mpv" nil (format "nohup mpv --force-window %s \"%s\" >/dev/null 2>&1" (mapconcat 'identity ytdl-opts " ") url))))
   ((seq-some (lambda (suffix)
                (string-suffix-p suffix url))
              '(".mp3" ".m4a" ".mp4" ".mkv" ".web" ".|m3" ".|jp" ".|jp" ".png"))
    (start-process-shell-command "mpv" nil (format "nohup mpv --force-window \"%s\" >/dev/null 2>&1" url)))
   (t (browse-web url)
      (lexical-let ((position pos))
        (add-hook 'eww-after-render-hook
                  (lambda ()
                    (when position (goto-pos position))
                    (setq-local eww-after-render-hook nil))
                  t t)))))

(defun open-uri-htmlize (uri)
  "Open html converted from URI in EWW."
  (let ((html (shell-command-to-string
               (mapconcat #'shell-quote-argument
                          (list "orgafile" "htmlize" uri)
                          " "))))
    (eww-open-file html)))

(defun open-uri-orgnize (uri)
  "Open contents on URI as a org file."
  (let ((org (shell-command-to-string
              (mapconcat #'identity
                         (list "orgafile" "orgnize" uri)
                         " "))))
    (find-file org)))

(defun youtube-dl-format ()
  "Return appropriate format option value for youtube-dl command."
  (let ((ipv4-addr (shell-command-to-string "hostname -I | cut -f1 -d' ' | tr -d '\n'"))
        (wifi-ssid (shell-command-to-string "iwgetid -r | tr -d '\n'")))
    (cond
     ;; using mobile Wi-Fi
     ((string-prefix-p "192.168.179." ipv4-addr)
      "bestvideo[height<=?240]+worstaudio/best")
     ;; using wired network or broad wifi
     ((or (string= ipv4-addr "192.168.100.126")
          (member wifi-ssid
                  '("saint_laurent_ap")))
      "bestvideo[height<=?720]+bestaudio/best")
     ;; usin unknown network
     (t
      "worstvideo+worstaudio"))))

(defvar download-video-dir "~/Videos" "Directory where downloaded video locate.")
(defun download-video (url &optional filename)
  "Download video from `URL'.

If optional argument `FILENAME' is given use this as a filename."
  (when (string-match "https?://www.youtube.com.+" url)
    (lexical-let* ((url-orig (match-string 0 url))
                   (urlobj-orig (url-generic-parse-url url-orig))
                   (pq-orig (url-path-and-query urlobj-orig))
                   (q-str (url-build-query-string
                           (list (assoc "v" (url-parse-query-string (cdr pq-orig))))))
                   (yt-url (url-recreate-url
                            (url-parse-make-urlobj (url-type urlobj-orig)
                                                   (url-user urlobj-orig)
                                                   (url-password urlobj-orig)
                                                   (url-host urlobj-orig)
                                                   (url-portspec urlobj-orig)
                                                   (concat (car pq-orig) "?" q-str)
                                                   (url-target urlobj-orig)
                                                   (url-attributes urlobj-orig)
                                                   (url-fullness urlobj-orig))))
                   (url-lex url)
                   (canonical-filename (if filename
                                           (replace-regexp-in-string
                                            "[\\?\"/:|’]" "" filename)
                                         nil))
                   (filename-lex canonical-filename))
      (princ (format "Downloading: %s" (or canonical-filename url)))
      (set-process-sentinel
       (start-process-shell-command
        "download video" nil
        (format "cd %1$s && youtube-dl %2$s %3$s"
                download-video-dir
                (mapconcat #'identity
                           (list (format "--format \"%s\"" (youtube-dl-format))
                                 (when canonical-filename (format "--output %s" (shell-quote-argument canonical-filename))))
                           " ")
                yt-url))
       (lambda (process desc)
         (when (eq (process-status process) 'exit)
           (let ((exit-status (process-exit-status process)))
             (if (= exit-status 0)
                 (alert (format "Downloaded: %s" (or filename-lex url-lex)) :severity 'normal :style 'fringe)
               (alert (format "Failed(%d): %s" exit-status (or filename-lex url-lex)) :severity 'urgent :style 'fringe)))))))))

(defvar download-audio-dir "~/Audio" "Directory where downloaded audio locate.")
(defun download-audio (url &optional filename)
  "Download audio from `URL'.

If optional argument `FILENAME' is given use this as a filename."
  (lexical-let* ((temp-fname (make-temp-name (if (directory-name-p download-audio-dir)
                                                 download-audio-dir
                                               (concat download-audio-dir "/"))))
                 (url-lex url)
                 (canonical-filename (if filename
                                         (replace-regexp-in-string
                                          "[\\?\"/:|’]" "" filename)
                                       nil))
                 (filename-lex canonical-filename))
    (princ (format "Downloading: %s" (or canonical-filename url)))
    (set-process-sentinel
     (start-process-shell-command
      "download audio" nil
      (format "%s; %s; %s"
              (if (string-match "https?://www.youtube.com.+" url)
                  (format "youtube-dl --extract-audio --audio-format mp3 %1$s -o %2$s.mp3; mv -f %2$s.mp3 %2$s" (match-string 0 url) temp-fname)
                (format "curl -LJs \"%s\" -o %s" url temp-fname))
              (format "cd %s && ffmpeg -n %s %s.mp3"
                      download-audio-dir
                      (mapconcat #'identity
                                 (list (format "-i %s" temp-fname)
                                       (if (equal '(undecided) (find-coding-systems-string (or canonical-filename url)))
                                           "-filter:a \"atempo=1.1\""
                                         "-filter:a \"atempo=1.7\"")
                                       "-vn")
                                 " ")
                      (if canonical-filename
                          (shell-quote-argument canonical-filename)
                        (file-name-base temp-fname)))
              (format "rm -f %s" temp-fname)))
     (lambda (process desc)
       (when (eq (process-status process) 'exit)
         (let ((exit-status (process-exit-status process)))
           (if (= exit-status 0)
               (alert (format "Downloaded: %s" (or filename-lex url-lex)) :severity 'normal :style 'fringe)
             (alert (format "Failed(%d): %s" exit-status (or filename-lex url-lex)) :severity 'urgent :style 'fringe))))))))

(defun show-media-duration (url)
  "Return a duration value for media located at URL."
  (when-let ((duration (cond
                        ((string-match "https?://www.youtube.com.+" url)
                         (car (split-string (shell-command-to-string
                                             (format "youtube-dl --get-duration %s" url)))))
                        ((string-match "https?://.+" url)
                         (car (split-string (shell-command-to-string
                                             (format "ffmpeg -i \"%s\" 2>&1 | grep Duration | awk -F '[ ]+' '{print substr($3, 0, length($3)-1)}'" url))))))))
    (message "Duration: %s" (if (stringp duration)
                                duration "N/A"))))

(defun download-video-at-point ()
  "Download video file from url currently pointed."
  (interactive)
  (when-let ((url (thing-at-point-url-at-point)))
    (download-video url)))

(defun download-audio-at-point ()
  "Download audio file from url currently pointed."
  (interactive)
  (let ((url (thing-at-point-url-at-point)))
    (when url
      (download-audio url))))

(defun show-media-duration-at-point ()
  "Show duration of media file which cursor currently pointed."
  (interactive)
  (if-let* ((url (thing-at-point-url-at-point)))
      (show-media-duration url)))

(defun increment-number-at-point (&optional inc)
  "Increment number at point by one.

With numeric prefix arg INC, increment the number by INC amount."
  (interactive "p")
  (let ((inc (or inc 1))
        (n (thing-at-point 'number))
        (bounds (bounds-of-thing-at-point 'word)))
    (delete-region (car bounds) (cdr bounds))
    (insert (number-to-string (+ n inc)))))
(defun decrement-number-at-point (&optional dec)
  "Decrement number at point by one.

With numeric prefix arg DEC, decrement the number by DEC amount."
  (interactive "p")
  (increment-number-at-point (- (or dec 1))))
(global-set-key (kbd "C-M-<up>")   #'increment-number-at-point)
(global-set-key (kbd "C-M-<down>") #'decrement-number-at-point)

(defun yank-and-indent ()
  (interactive)
  (yank)
  (indent-region (region-beginning) (region-end)))

(defun insert-time-stamp ()
  "Insert date string formatted YYYYMMDD style."
  (interactive)
  (insert (format-time-string "%Y%m%d")))
(bind-key "C-c ." #'insert-time-stamp
          global-map)

(defun refresh-buffer-and-display ()
  "Revert buffer and redraw display in one command."
  (interactive)
  (progn (revert-buffer t t)
         (redraw-display)))
(bind-keys ("s-r" . refresh-buffer-and-display))

;; for rubikichi mail magazine
(defun emm:goto (idx) (progn(push-mark)(search-forward idx)(recenter 0)))

(defun create-fontset-for-unicode (fontset-name font-family font-size &optional weight slant)
  "Create fontset based on FONT-FAMILY, FONT-SIZE, WEIGHT AND ALANT.

The name of created fontset, specified FONTSET-NAME with prefix 'fontset-' will be returned."
  (let* ((fontset-prefix "fontset-")
         (weight (if weight weight "normal"))
         (weight-symbol (make-symbol weight))
         (slant (if slant slant "normal"))
         (slant-symbol (make-symbol slant))
         (font-str (format "%s-%d:weight=%s:slant=%s" font-family font-size weight slant))
         (unicode-fontspec (font-spec :family font-family :size font-size :weight weight-symbol :slant slant-symbol))
         (fontset-name-str (create-fontset-from-ascii-font font-str nil fontset-name)))
    (set-fontset-font fontset-name-str 'unicode unicode-fontspec)
    (concat fontset-prefix fontset-name)))

(defvar utl-x-display-default-pixel-width 1920.0 "Default display width in pixel.")
(defmacro utl-x-display-adjust-font-size (original)
  "Adjust original font size to fit in display."
  `(truncate (* ,original (/ (x-display-pixel-width) ,utl-x-display-default-pixel-width))))

(defun utl-say ()
  "read out using open-jtalk"
  (interactive)
  (let* ((read-text-raw (buffer-substring (region-beginning) (region-end)))
         (read-text (replace-regexp-in-string "[ \t\r\n\v\f]+" "" read-text-raw))
         (say-cmd (if (string-match
                       (format "\\`[%s]+\\'" "[:ascii:]’“”–")
                       read-text)
                      "esay" "jsay")))
    (call-process-shell-command (format "echo %s | %s &" read-text say-cmd))))

(defmacro add-to-hook-delay (hook body)
  `(add-hook ,hook
             (lambda () ,body)))
