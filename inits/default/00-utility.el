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

(defun apply-in-indirect-buffer (function &rest arguments)
  "Call FUNCTION in a indirect buffer with our remaining args, using ARGUMENTS."
  (interactive "P")
  (let ((ibuf (switch-to-buffer (org-get-indirect-buffer))))
    (condition-case _
        (apply function arguments)
      (quit (kill-buffer ibuf)))))

(defun open-thing-at-point ()
  "Open thing."
  (interactive)
  (let ((button (button-at (point)))
        (email (thing-at-point 'email))
        (links (eww-links-at-point))
        (url (thing-at-point-url-at-point))
        (filename (thing-at-point 'filename)))
    (cond
     (button
      (push-button)
      t)
     (email
      (mu4e~compose-mail email)
      t)
     (links
      (cl-case (prefix-numeric-value current-prefix-arg)
        (16 (browse-url-default-browser (car links)))
        (4 (eww-browse-url (car links)))
        (t (open-url (car links))))
      t)
     (url (let ((url-pos (split-location-uri url)))
            (cl-case (prefix-numeric-value current-prefix-arg)
              (16 (browse-url-default-browser (car url-pos)))
              (4 (eww-browse-url (car url-pos)))
              (t (open-url (car url-pos) (cadr url-pos))))
            t))
     ((and (stringp filename)
           (file-exists-p filename))
      (let ((url-pos (split-location-uri filename)))
        (cl-case (prefix-numeric-value current-prefix-arg)
          (16 (open-file-external (car url-pos)))
          (4 (find-file (car url-pos)))
          (t (open-file (car url-pos))
             (goto-pos (cadr url-pos))))
        t)))))

(defvar open-file-work-dir "~/var/tmp/exfile")

(defun open-file (file)
  "Open file `FILE' with appropriate application."
  (let ((ex-file (expand-file-name file))
        (rm-file nil))
    (cond
     ((string-suffix-p ".mp4.gpg" ex-file)
      (call-process-shell-command
       (format "gpg -o \"%s\" -d \"%s\""
               (setf rm-file (expand-file-name (concat (make-temp-name
                                                        (file-name-as-directory open-file-work-dir))
                                                       ".mp4")))
               ex-file))
      (setf ex-file rm-file))
     ((string-suffix-p ".tar.gpg" ex-file)
      (call-process-shell-command
       (format "mkdir -p \"%s\"; gpg -d %s | tar xv -C %s"
               (setf rm-file (expand-file-name (make-temp-name
                                                (file-name-as-directory open-file-work-dir))))
               ex-file
               rm-file))
      (setf ex-file rm-file)))
    (cond
     ((or (= (call-process-shell-command (format "filetype-cli check --type playable \"%s\"" ex-file)) 0)
          (string-suffix-p ".m3u" ex-file))
      (start-process-shell-command "mpv" nil
                                   (concat (format "nohup mpv --force-window \"%s\" >/dev/null 2>&1;" ex-file)
                                           (if rm-file (format "rm -rf \"%s\"" rm-file) nil))))
     ((or (= (call-process-shell-command (format "filetype-cli check --type pdf \"%s\"" ex-file)) 0)
          (= (call-process-shell-command (format "filetype-cli check --type epub \"%s\"" ex-file)) 0))
      (open-uri-htmlize ex-file))
     ((file-directory-p ex-file) (dired ex-file))
     (t (find-file ex-file)))))

(defun open-file-external (file)
  "Open file `FILE' in external application.
Generally preferance application is used."
  (let ((ex-file (expand-file-name file))
        (process-connection-type nil))
    (start-process-shell-command "xdg-open" nil (format "xdg-open \"%s\"" ex-file))))

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
                         (if (not (string-prefix-p "192.168.179." (shell-command-to-string "hostname -I | cut -f1 -d' ' | tr -d '\n'")))
                             "--ytdl-format=\"bestvideo[height<=?720]+bestaudio/best\""
                           "--ytdl-format=\"worstvideo+worstaudio\"")))))
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
