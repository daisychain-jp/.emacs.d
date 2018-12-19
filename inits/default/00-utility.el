;; get a face variable name relevant with the pinted word
(defun describe-face-at-point ()
  (interactive)
  (message "%s" (get-char-property (point) 'face)))

(defun kill-line-number ()
  "Add line number to kill ring."
  (interactive)
  (kill-new (number-to-string (line-number-at-pos))))

(defun apply-in-indirect-buffer (function &rest arguments)
  "Call FUNCTION in a indirect buffer with our remaining args, using ARGUMENTS."
  (interactive "P")
  (let ((ibuf (switch-to-buffer (org-get-indirect-buffer))))
    (condition-case _
        (apply function arguments)
      (quit (kill-buffer ibuf)))))

(defun open-thing-at-point (&optional arg)
  "Open thing.

The optional prefix argument ARG is passed to lower function."
  (interactive "P")
  (let ((button (button-at (point)))
        (email (thing-at-point 'email))
        (links (eww-links-at-point))
        (url (thing-at-point-url-at-point))
        (filename (thing-at-point 'filename)))
    (cond
     (button (push-button))
     (email (mu4e~compose-mail email))
     (links (cl-case (car arg)
              (16 (browse-url-default-browser (car links)))
              (4 (eww-browse-url (car links)))
              (t (open-url-switch-application (car links)))))
     (url (let ((url-pos (split-positioned-uri url)))
            (cl-case (car arg)
              (16 (browse-url-default-browser (car url-pos)))
              (4 (eww-browse-url (car url-pos)))
              (t (open-url-switch-application (car url-pos) (cadr url-pos))))))
     (filename (let ((filename-pos (split-positioned-uri (expand-file-name filename))))
                 (cl-case (car arg)
                   (16 (browse-url-default-browser (car filename-pos)))
                   (4 (find-file-at-point (car filename-pos)))
                   (t (open-url-switch-application (car filename-pos) (cadr filename-pos)))))))))

(defun split-positioned-uri (positioned-uri)
  "Split POSITIONED-URI into uri and position specifier."
  (cond
   ((string-match "\\(.+\\)::\\([[:digit:]]+\\)$" positioned-uri)
    (list (match-string 1 positioned-uri) (string-to-number (match-string 2 positioned-uri))))
   ((string-match "\\(.+\\)::\\([[:graph:][:blank:]]+\\)$" positioned-uri)
    (list (match-string 1 positioned-uri) (match-string 2 positioned-uri)))
   (t (list positioned-uri))))

(defun goto-pos (pos)
  "Go to POS.

if POS is numeric, go to line using 'forward-line.
If POS is string, search it forward and set point to occurence."
  (interactive)
  (cond
   ((numberp pos)
    (forward-line (- pos (line-number-at-pos))))
   ((stringp pos)
    (goto-char (search-forward pos nil t)))))

(defun open-url-switch-application (url &optional pos)
  "Open URL in an appropriate manner and jump to POS.

If URL points to a multi media contents like youtube video, mp3 audio file,
play that with media player."
  (cond
   ((or (s-ends-with? ".pdf" url)
        (s-ends-with? ".epub" url))
    (open-uri-htmlize url)
    (when pos (goto-pos pos)))
   ((eql (call-process-shell-command (format "filetype-cli check --type playable \"%s\"" url)) 0)
    (utl-play-media url pos))
   (t
    (browse-web url)
    (lexical-let ((position pos))
      (add-hook 'eww-after-render-hook
                (lambda ()
                  (when position (goto-pos position)) ;
                  (setq-local eww-after-render-hook nil))
                t t)))))

(defun open-uri-htmlize (uri)
  "Open html converted from URI in EWW."
  (let ((html (shell-command-to-string
               (mapconcat #'shell-quote-argument
                          (list "orgafile" "htmlize" uri)
                          " "))))
    (eww-open-file html)))

(defun utl-play-media (file &optional start)
  "Play a media file `FILE' at START point."
  (cond
   ;; play youtube video with mpv
   ((string-match-p "^https?://www.youtube.com.*" file)
    ;; convert greedily watch url to playlist url if possible
    (let ((file (if (string-match "&list=\\([[:graph:]]+\\)" file)
                    (format "https://www.youtube.com/playlist?list=%s" (match-string 1 file))
                  file))
          (ytdl-opts
           (remove nil
                   (list (when (numberp start)
                           (format "--ytdl-raw-options=playlist-start=%d" start))
                         (if (string= "192.168.100.126" (shell-command-to-string "hostname -I | cut -f1 -d' ' | tr -d '\n'"))
                             "--ytdl-format=\"bestvideo[height<=?720]+bestaudio/best\""
                           "--ytdl-format=\"worstvideo+worstaudio\"")))))
      (start-process-shell-command "mpv" nil (format "mpv --force-window %s \"%s\"" (mapconcat 'identity ytdl-opts " ") file))))
   (t (start-process-shell-command "mpv" nil (format "mpv --force-window \"%s\"" file)))))

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

(defun create-unicode-fontset (fontset-name ascii-font ascii-size unicode-font unicode-size &optional weight slant)
  "docstring"
  (let* ((fsn-prefix "fontset-")
         (weight (if weight
                     weight "normal"))
         (weight-symbol (make-symbol weight))
         (slant (if slant
                    slant "normal"))
         (slant-symbol (make-symbol slant))
         (font (format "%s-%d:weight=%s:slant=%s" ascii-font ascii-size weight slant))
         (fontspec (font-spec :family ascii-font))
         (unicode-fontspec (font-spec :family unicode-font :size unicode-size :weight weight-symbol :slant slant-symbol))
         (fsn (create-fontset-from-ascii-font font nil fontset-name)))
    (set-fontset-font fsn 'unicode unicode-fontspec)
    (concat fsn-prefix fontset-name)))

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
