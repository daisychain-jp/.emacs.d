;; (el-get-bundle org-mode)
(straight-use-package 'org)
(use-package org
  :straight t
  :delight
  (org-mode         " OG")
  (org-agenda-mode  " OG")
  (org-capture-mode " OG")
  :diminish ((org-src-mode . "os"))
  :mode (("\\.org$" . org-mode))
  :interpreter (("org" . org-mode))
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c j" . org-clock-goto)
         ("C-c s" . org-store-link))
  :init
  (add-to-list 'load-path (format "%s/straight/repos/org/contrib/lisp"
                                  user-emacs-directory))
  :custom
  (org-directory env-doc-dir)
  :config
  (require 'org-agenda)
  (require 'org-capture)
  (require 'org-habit)
  (require 'org-eww)
  (require 'org-eshell)
  (require 'org-mu4e)
  (require 'org-drill)

  ;; local key bindings
  (bind-keys :map org-mode-map
             ("M-j"     . org-meta-return)
             ("C-o"     . org-open-at-point)
             ("C-c /"   . org-sparse-tree-indirect-buffer)
             ("C-c \\"  . org-match-sparse-tree-indirect-buffer)
             ("C-c z"   . org-narrow-to-element-indirect-buffer)
             ("C-,"     . nil)
             ("C-x C-a s" . org-afile-store)
             ("C-x C-a d" . org-afile-delete)
             ("C-c o" . org-mu4e-compose-org-mode)
             :map org-agenda-mode-map
             ("C-j" . org-agenda-switch-to))

  ;; list
  (setq org-list-demote-modify-bullet
        '(("-" . "+") ("+" . "-") ("*" . "-")
          ("1." . "1)") ("1)" . "1.") ("*" . "1.")))
  (setq org-cycle-include-plain-lists 'integrate)

  ;; link
  (setq org-confirm-elisp-link-function nil) ; do not confirm when execute elisp
  (defadvice org-open-at-point (around switch-browser activate)
    (let ((link-str (org-link-unescape (car (org-link-at-point)))))
      (if (and (stringp link-str)
               (string-match-p "^https?://.+" link-str))
          (let ((url-pos (split-location-uri link-str)))
            (cl-case (car arg)
              (16 (browse-url-default-browser (car url-pos)))
              (4 (eww-browse-url (car url-pos)))
              (t (open-url-switch-application (car url-pos) (cadr url-pos)))))
        ad-do-it)))
  (setq org-file-apps
        '((auto-mode . emacs)
          ("\\.mm\\'" . default)
          ("\\.x?html?\\'" .
           (lambda (file-path link-string)
             (eww-open-file file-path)
             (goto-pos (cadr (split-location-uri link-string)))))
          ("org.gpg" . emacs)
          ("tar.gpg" .
           (lambda (file-path link-string)
             (lexical-let (file)
               (deferred:$
                 (deferred:process "orgafile" "play" file-path)))))
          ("\\(?:pdf\\|epub\\)\\'" .
           (lambda (file-path link-string)
             (open-uri-htmlize file-path)
             (goto-pos (cadr (split-location-uri link-string)))))
          ("\\(?:mp3\\|m4a\\|mp4\\|mkv\\|jpg\\|jpeg\\|png\\)\\'" .
           (lambda (file-path link-string)
             (deferred:$
               (deferred:process "orgafile" "play" file-path))))
          (directory . (lambda (file-path link-string)
                         (if (= 0 (call-process-shell-command (format "filetype-cli check --type playable %s" file-path)))
                             (start-process-shell-command "mpv" nil (format "mpv --force-window \"%s\"" file-path))
                           (dired file-path))))))
  (org-add-link-type
   "sudo"
   (lambda (cmd)
     "Run CMD with sudo."
     (shell-command
      (concat "echo " (shell-quote-argument (read-passwd "Password? "))
              " | sudo -S " cmd))))

  ;; afile
  (defun org-afile-expand (tag)
    "expands 'tag' to absolute path corresponds with rfile root directory."
    (let ((loc (shell-command-to-string "orgafile location")))
      (format "%s/%s" loc tag)))
  (defun org-afile-store (arg)
    "Stores file to the specified directory.

If a single prefix 'ARG' is passed, it encrypts rfile with .tar.gpg extension."
    (interactive "P")
    (let* ((file (read-file-name "File: " "~/"))
           (base (shell-command-to-string "orgafile location"))
           (dest-dir (read-directory-name "Dir: " base))
           (opt (if (equal arg '(4)) "--encrypt" ""))
           (store-loc (shell-command-to-string (format "orgafile store %s %s %s" opt (shell-quote-argument file) (shell-quote-argument dest-dir)))))
      (minibuffer-message "Stored to %s" store-loc)
      (kill-new store-loc)))
  (defun org-afile-delete (arg)
    "Delete afile currently pointed.

If 'ARG' is passed, shred afile instead delete."
    (interactive "P")
    (let* ((alink (org-link-at-point))
           (lstr (car alink))
           (cmd-opt (if (equal arg '(4)) "--shred" ""))
           (do-str (if (equal arg '(4)) "Shred" "Delete")))
      (when (and (stringp lstr)
                 (string-match "^afile:\\(.+\\)$" lstr))
        (let* ((ex-lstr (org-afile-expand (match-string 1 lstr))))
          (if (file-exists-p ex-lstr)
              (when (string= (downcase (read-string (format "%s? y/n: " do-str))) "y")
                (call-process-shell-command (format "orgafile delete %s %s" cmd-opt ex-lstr))
                (minibuffer-message (format "%s DONE!" do-str)))
            (minibuffer-message "NOT EXIST"))))))

  ;; speed command
  (setq org-use-speed-commands t)
  (setq org-speed-commands-user
        '(("$" org-archive-to-archive-file)
          ("a" org-attach)
          ("A" org-toggle-archive-tag)
          ("W" org-copy)
          ("Q" org-clock-cancel)
          ("/" org-sparse-tree-indirect-buffer)
          ("m" org-match-sparse-tree-indirect-buffer)
          ("z" org-narrow-to-element-indirect-buffer)
          ("!" org-readable)
          ("S" org-send-mail)
          ("k" nil)
          ("K" org-entry-kill-property)
          ("&" org-id-view-refs)
          ("N" org-add-note)
          ("T" org-set-tags-command)
          ("P" call-interactively 'org-set-property)
          ("s" call-interactively 'org-schedule)
          ("d" call-interactively 'org-deadline)))

  ;; basic
  (setq org-hide-leading-stars t)
  (setq org-comment-string "####")
  (setq org-pretty-entities t)
  (setq org-use-sub-superscripts '{}) ; use _{}/^{} for sub/super script

  ;; display
  (add-hook 'org-mode-hook
            (lambda ()
              (whitespace-mode 1)
              (buffer-face-set 'outline)
              (setq-local line-spacing 0.1)
              (smart-newline-mode 1)
              (reveal-mode 1)))
  (add-hook 'org-agenda-mode-hook
            (lambda ()
              (buffer-face-set 'outline)
              (setq-local line-spacing 0.1)
              (delete-other-windows)
              (org-agenda-to-appt t '((category "appt")))))

  ;; tag/property
  (setq org-use-tag-inheritance "^ac_.+")
  (setq org-tags-column -48)
  (setq org-global-properties
        '(("Effort_ALL". "0 0:10 0:30 1:00 2:00 4:00 6:00 8:00")))
  (setq org-use-property-inheritance "TIMELIMIT.*")
  (setq org-highest-priority ?A)
  (setq org-lowest-priority ?Z)
  (setq org-default-priority ?Z)

  ;; id
  (setq org-id-files (file-expand-wildcards (concat env-doc-dir "/**/*.org")))
  (setq org-id-locations-file (concat env-doc-dir "/.org-id-locations"))
  (setq org-id-track-globally t)
  (setq org-id-link-to-org-use-id 'create-if-interactive)

  ;; todo
  (setq org-todo-keywords
        '((sequence "HBT(h)" "SMD(y/!)" "URGE(u/!)" "TDAY(T/!)" "WEEK(w/!)" "TODO(t/!)" "ONGO(o/!)" "|" "DONE(d/@)" "CXL(x@/@)" "PEND(p@/@)")))
  (setq org-todo-keyword-faces
        '(("HBT"  . ((org-todo    (:foreground "OliveDrab1"))))
          ("SMD"  . ((org-todo    (:foreground "SpringGreen"))))
          ("URGE" . ((org-warning (:foreground "red1"))))
          ("TDAY" . ((org-todo    (:foreground "DodgerBlue1"))))
          ("WEEK" . ((org-todo    (:foreground "DeepPink1"))))
          ("TODO" . ((org-todo    (:foreground "green1"))))
          ("ONGO" . ((org-todo    (:foreground "chocolate"))))
          ("PEND" . ((org-todo    (:foreground "sea green"))))
          ("DONE" . ((org-done    (:foreground "gray30"))))
          ("CXL"  . ((org-done    (:foreground "dark gray"))))))
  (add-hook  'org-after-todo-state-change-hook
             (lambda ()
               (interactive)
               (save-excursion
                 (org-back-to-heading t)
                 (let* ((element (org-element-at-point))
                        (todo-state (org-get-todo-state))
                        (priority (org-element-property :priority element))
                        (category (org-entry-get (point) "CATEGORY"))
                        (style (org-entry-get (point) "STYLE")))
                   ;; remove priority when the todo state moves to DONE|CXL|PND
                   (when (and
                          (s-matches? "DONE\\|CXL\\|PND" todo-state)
                          (bound-and-true-p priority))
                     (org-priority ? ))
                   ;; strip DONE state if CATEGORY of the entry is "Habit"
                   (when (and (string= category "Habit")
                              (string= todo-state "DONE")
                              (not (string= style "habit")))
                     (org-todo ""))))))

  ;; clock
  (setq org-clock-into-drawer "CLOCKLOG")
  (setq org-clock-persist t)
  (setq org-clock-persist-query-resume nil)
  (org-clock-persistence-insinuate)
  (setq org-clock-string-limit 20)
  (add-hook 'org-clock-in-hook
            (lambda () (save-buffer)))
  (add-hook 'org-clock-out-hook
            (lambda () (save-buffer)))
  (add-hook 'org-clock-cancel-hook
            (lambda () (save-buffer)))
  (defconst org-clock-ts-line-re
    (concat "^[ \t]*" org-clock-string "[ \t]*" org-tsr-regexp-both)
    "Matches a line with clock time stamp.")

  ;; time
  (setq org-duration-format
        '(("d" . nil)
          (special . h:mm)))

  ;; time-stamp
  (setq org-time-stamp-custom-formats
        '("<%m-%d %a>" . "<%H:%M>"))
  (defun org-display-iso-week-at-point ()
    "Display ISO 8601 week number correspoinds to time stamp at point."
    (interactive)
    (let ((ts (org-timestamp-at-point)))
      (when ts
        (message "W%s" (format-time-string "%V" (org-read-date nil t ts nil))))))
  (defun org-timestamp-at-point ()
    "Return time stamp string at point."
    (let ((ts-bound (org-in-regexp (org-re-timestamp 'all))))
      (when ts-bound
        (buffer-substring (car ts-bound) (cdr ts-bound)))))

  ;; logging
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-log-states-order-reversed nil)
  (setq org-reverse-note-order t)
  (setq org-log-note-headings
        '((done .  "CLOSING NOTE %t")
          (state . "")
          (note .  "")
          (reschedule .  "Rescheduled from %S on %t")
          (delschedule .  "Not scheduled, was %S on %t")
          (redeadline .  "New deadline from %S on %t")
          (deldeadline .  "Removed deadline, was %S on %t")
          (refile . "Refiled from %s to %S on %t")
          (clock-out . "")))
  (add-hook 'org-log-buffer-setup-hook
            (lambda ()
              (setq skk-dcomp-activate 'eolp)
              (setq skk-dcomp-multiple-activate nil)))

  ;; org-agenda
  (defvar org-agenda-files-default (append
                                    (file-expand-wildcards (concat env-doc-dir "/*_a"))
                                    (file-expand-wildcards (concat env-doc-dir "/**/*_a.org")))
    "Default org-agenda-files.")
  (setq org-agenda-files org-agenda-files-default)
  (setq org-agenda-prefix-format
        '((agenda   . "%?-12t% s")
          (timeline . "  %s")
          (todo     . " ")
          (tags     . " ")
          (search   . " ")))

  ;; org-capture
  (setq org-capture-bookmark nil)
  (setq org-bookmark-names-plist nil)

  ;; drill
  (setq org-drill-scope 'tree)
  (setq org-drill-cram-hours 0.5)

  ;; clock table
  ;;; work around for the bug in emacs 25
  (defalias 'calendar-absolute-from-iso 'calendar-iso-to-absolute)

  ;; image
  (setq org-image-actual-width 100)

  ;; export
  (setq org-export-with-sub-superscripts nil)
  (setq org-export-dispatch-use-expert-ui nil)
  (setq org-export-with-creator nil)

  ;; org-babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R        . t)
     (shell    . t)
     (ruby     . t)
     (python   . t)
     (org      . t)
     (gnuplot  . t)
     (dot      . t)
     (plantuml . t)))
  (setq org-confirm-babel-evaluate nil)
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
  (cond
   ((string= system-type "gnu/linux")
    (custom-set-variables '(org-plantuml-jar-path (format "%s/lib/plantuml/plantuml.jar" env-var-dir))))
   ((string= system-type "darwin")
    (custom-set-variables '(org-plantuml-jar-path "/usr/local/Cellar/plantuml/8041/plantuml.8041.jar"))))

  ;; latex
  (require 'ox-latex)
  (setq org-latex-pdf-process '("platex %f"
                                "platex %f"
                                "bibtex %b"
                                "platex %f"
                                "platex %f"
                                "dvipdfmx %b.dvi"))
  (add-to-list 'org-latex-classes
               '("jsarticle"
                 "\\documentclass[dvipdfmx,12pt]{jsarticle}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes
               `("beamer"
                 "\\documentclass[presentation,dvipdfmx,18pt]{beamer}\n"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
  (setq org-latex-default-class "jsarticle")

  ;; src
  (setq org-src-window-setup 'current-window)

  ;; save all org files every hour
  (run-at-time 10 300 'org-save-all-org-buffers))

(use-package helm-org-rifle
  :straight t
  :bind (("M-s g o" . helm-org-rifle-org-directory))
  :after (org helm))

(defun org-sparse-tree-indirect-buffer (&optional arg type)
  "Create a sparse tree, prompt for the details.
This command can create sparse trees.
You first need to select the type same in org-sparse-tree."
  (interactive "P")
  (apply-in-indirect-buffer 'org-sparse-tree arg type))
(defun org-match-sparse-tree-indirect-buffer (&optional todo-only match)
  "Create a sparse tree in a indirect buffer.
If optional argument TODO-ONLY is non-nil,
 only select lines that are also TODO tasks.
The sparse tree is according to tags string MATCH."
  (interactive)
  (apply-in-indirect-buffer 'org-match-sparse-tree todo-only match))
(defun org-narrow-to-element-indirect-buffer ()
  "Narrow to current element in indirect buffer."
  (interactive)
  (apply-in-indirect-buffer 'org-narrow-to-element))

(defcustom org-readable-directory "~/var/lib/readable"
  "Directory where all html file for org-readable is located.")

(defun org-readable ()
  "View the main readable of current org subtree in EWW."
  (interactive)
  (org-copy-subtree)
  (save-excursion
    (with-temp-buffer
      (org-paste-subtree)
      (save-excursion
        (let* ((beg (org-entry-beginning-position))
               (end (org-end-of-subtree)))
          (goto-char beg)
          (cl-loop while (re-search-forward org-bracket-link-regexp end t)
                   do (save-excursion
                        (goto-char (match-beginning 0))
                        (let* ((link (org-element-link-parser))
                               (list-begin (plist-get (cadr link) :begin))
                               (list-end (plist-get (cadr link) :end))
                               (raw-link (plist-get (cadr link) :raw-link))
                               (type (plist-get (cadr link) :type)))
                          ;; delete link with location-uri since export parser could not handled
                          (when (or (string= type "fuzzy")
                                    (cdr (split-location-uri raw-link)))
                            (delete-region list-begin list-end)))))))
      (let ((org-export-with-author nil)
            (org-export-show-temporary-export-buffer nil))
        (org-html-export-as-html nil t)))
    (let* ((id (org-id-get))
           (uuid (downcase (if id id (org-id-uuid))))
           (org-export-buffer-name "*Org HTML Export*")
           (org-readable-file (format "%s/%s.html" org-readable-directory uuid)))
      (with-current-buffer org-export-buffer-name
        (write-file org-readable-file)
        (eww-open-file org-readable-file)
        (kill-buffer (format "%s.html" uuid))))))

(defun org-send-mail ()
  "Send a mail of org contents."
  (interactive)
  (let ((heading (org-get-heading t t t t))
        (org-export-show-temporary-export-buffer nil))
    (org-ascii-export-as-ascii nil t nil)
    (let* ((export-string
            (with-current-buffer "*Org ASCII Export*"
              (buffer-string)))
           (mail-string (format "To: tinamo@yahoo.co.jp\nFrom: t.inamori@daisychain.jp\nSubject: %s\n\n%s\n" heading export-string)))
      (call-process-shell-command
       (format "echo \"%s\" | msmtp -C ~/.msmtprc -a default tinamo@yahoo.co.jp" mail-string))
      (kill-buffer "*Org ASCII Export*"))))

(defun org-entry-kill-property ()
  "Append property value to the kill ring by selecting key."
  (interactive)
  (let* ((properties (org-entry-properties))
         (selected-key
          (completing-read "Property name: "
                           (mapcar #'(lambda (var) (car var)) properties))))
    (when (stringp selected-key)
      (kill-new (cdr (assoc selected-key properties))))))

(defun org-end-of-subtree ()
  (interactive)
  (org-end-of-subtree))

(defun org-link-at-point ()
  "Return org link at point as a cons."
  (let ((link (org-in-regexp org-bracket-link-regexp 1)))
    (if link
        (let* ((full-str (buffer-substring (car link) (cdr link)))
               (match-index (string-match org-bracket-link-regexp full-str)))
          (if match-index
              (cons (match-string 1 full-str) (match-string 2 full-str))
            nil))
      nil)))

(defun org-scratch (arg)
  "Create and visit temporal org file."
  (interactive "p")
  (find-file
   (make-temp-file "auto-org-capture" nil ".org"))
  (skk-mode 1))

(defun org-clock-remove-old-timestamps (old-time)
  "Remove old clock timestamps earlier than 'OLD-TIME' in the current subtree."
  (save-excursion
    (org-back-to-heading t)
    (org-show-all)
    (org-map-entries
     (lambda ()
       (let ((drawer (org-clock-drawer-name))
             (case-fold-search t))
         (when drawer
           (let* ((re (format "^[ \t]*:%s:[ \t]*$" (regexp-quote drawer)))
                  (tree-end (save-excursion
                              (org-end-of-subtree)))
                  (drawer-start (save-excursion
                                  (re-search-forward re tree-end t)))
                  (drawer-end (save-excursion
                                (re-search-forward re tree-end t)
                                (re-search-forward "^[ \t]*:END:[ \t]*$" tree-end t))))
             (when (and drawer-start drawer-end)
               (goto-char drawer-start)
               (while (re-search-forward org-tsr-regexp-both drawer-end t)
                 (when (time-less-p (apply 'encode-time (parse-time-string (match-string 3)))
                                    old-time)
                   (kill-whole-line)
                   (setq drawer-end (save-excursion
                                      (re-search-forward "^[ \t]*:END:[ \t]*$" tree-end t)))))
               (org-remove-empty-drawer-at (point)))
             (setq org-map-continue-from (org-entry-end-position))))))
     nil 'tree)))

(defun org-get-latest-clock-log-time (pom)
  "Get the latest clock log time stamp in org entry at POM as a time object.

If entry at POM has no clock log time stamp, this function returns 0."
  (org-with-point-at pom
    (save-excursion
      (setq end-of-subtree (org-end-of-subtree))
      (setq latest-time 0)
      (org-back-to-heading t)
      (org-show-all)
      (while (re-search-forward org-drawer-regexp end-of-subtree t)
        (when (string= (match-string 1) (org-clock-drawer-name))
          (while (progn
                   (forward-line 1)
                   (when (org-match-line org-clock-ts-line-re)
                     (setq match-ts
                           (if (match-string 3)
                               (match-string 3) (match-string 1)))
                     (when (time-less-p latest-time
                                        (apply 'encode-time (parse-time-string match-ts)))
                       (setq latest-time (apply 'encode-time (parse-time-string match-ts)))))
                   (not (org-match-line org-clock-drawer-end-re))))))))
  latest-time)

(defun org-agenda-cmp-latest-clock-log (a b)
  "Compare two org entry A and B in terms of clock log.

This function can be used as `org-agenda-cmp-user-defined' in `org-agenda-sorting-strategy'."
  (let* ((marker-a (get-text-property 1 'org-marker a))
         (time-a (org-get-latest-clock-log-time marker-a))
         (marker-b (get-text-property 1 'org-marker b))
         (time-b (org-get-latest-clock-log-time marker-b)))
    (if (time-less-p time-a time-b) -1 +1)))

(defun org-gc-subtree ()
  "Do garbage collection for the current subtree."
  ;; TODO: This function could be refactored with
  ;;       org-clock-ts-line-re and marker feature
  (interactive)
  (let* ((current (decode-time (current-time)))
         (month-ago (encode-time (nth 0 current)
                                 (nth 1 current)
                                 (nth 2 current)
                                 (nth 3 current)
                                 (- (nth 4 current) 1)
                                 (nth 5 current))))
    (org-clock-remove-old-timestamps month-ago)))

(add-to-list 'load-path (format "%s/site-lisp/ts.el/" user-emacs-directory))

;;;###autoload
(defun org-refile-to-datetree-using-ts-in-entry (which-ts file &optional subtree-p)
  "Refile current entry to datetree in FILE using timestamp found in entry.
WHICH should be `earliest' or `latest'. If SUBTREE-P is non-nil,
search whole subtree."
  (interactive (list (intern (completing-read "Which timestamp? " '(earliest latest)))
                     (read-file-name "File: " (concat org-directory "/") nil 'mustmatch nil
                                     (lambda (filename)
                                       (string-suffix-p ".org" filename)))
                     current-prefix-arg))
  (require 'ts)
  (let* ((sorter (pcase which-ts
                   ('earliest #'ts<)
                   ('latest #'ts>)))
         (tss (org-timestamps-in-entry subtree-p))
         (ts (car (sort tss sorter)))
         (date (list (ts-month ts) (ts-day ts) (ts-year ts))))
    (org-refile-to-datetree file :date date)))

;;;###autoload
(defun org-timestamps-in-entry (&optional subtree-p)
  "Return timestamp objects for all Org timestamps in entry.
 If SUBTREE-P is non-nil (interactively, with prefix), search
 whole subtree."
  (interactive (list current-prefix-arg))
  (save-excursion
    (let* ((beg (org-entry-beginning-position))
           (end (if subtree-p
                    (org-end-of-subtree)
                  (org-entry-end-position))))
      (goto-char beg)
      (cl-loop while (re-search-forward org-tsr-regexp-both end t)
               for ts = (save-excursion
                          (goto-char (match-beginning 0))
                          (org-element-timestamp-parser))
               collect (ts-parse-org ts)))))

;;;###autoload
(cl-defun org-refile-to-datetree (file &key (date (calendar-current-date)) entry)
  "Refile ENTRY or current node to entry for DATE in datetree in FILE."
  (interactive (list (read-file-name "File: " (concat org-directory "/") nil 'mustmatch nil
                                     (lambda (filename)
                                       (string-suffix-p ".org" filename)))))
  ;; If org-datetree isn't loaded, it will cut the tree but not file
  ;; it anywhere, losing data. I don't know why
  ;; org-datetree-file-entry-under is in a separate package, not
  ;; loaded with the rest of org-mode.
  (require 'org-datetree)
  (unless entry
    (org-cut-subtree))
  ;; Using a condition-case to be extra careful. In case the refile
  ;; fails in any way, put cut subtree back.
  (condition-case err
      (with-current-buffer (or (org-find-base-buffer-visiting file)
                               (find-file-noselect file))
        (org-datetree-find-iso-week-create date)
        (let ((level (org-get-valid-level (funcall outline-level) 1)))
          (org-end-of-subtree t t)
          (org-back-over-empty-lines)
          (org-paste-subtree level (or entry (car kill-ring))))
        (save-buffer))
    (error (unless entry
             (org-paste-subtree))
           (message "Unable to refile! %s" err))))

(use-package org-web-tools
  :straight t
  :bind (("C-c C-;" . org-web-tools-insert-link-for-url)))
