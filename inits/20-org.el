(el-get-bundle org-mode)
(el-get-bundle helm-org-rifle)
(use-package org
  :mode (("\\.org$" . org-mode))
  :interpreter (("org" . org-mode))
  :bind (("C-c a" . org-agenda)
         ("C-c C" . org-capture)
         ("C-c j" . org-clock-goto)
         ("C-c s" . org-store-link)
         ("M-s o" . helm-org-rifle-org-directory))
  :config
  (require 'org-agenda)
  (require 'org-capture)
  (require 'org-habit)
  (require 'org-id)
  (require 'org-eww)
  (require 'org-mew)
  (require 'org-mu4e)
  (require 'org-drill)
  (require 'helm-org-rifle)

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
    (let ((link-str (car (org-link-at-point))))
      (if (and
           (stringp link-str)
           (string-match-p "^https?://.+" link-str))
          (let ((url-pos
                 (cond
                  ((string-match "^\\(https?://.+\\)::\\([[:digit:]]+\\)$" link-str)
                   (cons (match-string 1 link-str) (string-to-number (match-string 2 link-str))))
                  ((string-match "^\\(https?://.+\\)::\\([[:graph:][:blank:]]+\\)$" link-str)
                   (cons (match-string 1 link-str) (match-string 2 link-str)))
                  (t
                   (cons link-str nil)))))
            (cl-case (car (ad-get-arg 0))
              (16 (browse-url-default-browser (car url-pos)))
              (4 (eww-browse-url (car url-pos)))
              (t (open-url-switch-application (car url-pos) (cdr url-pos)))))
        ad-do-it)))
  (append-to-list
   org-file-apps
   '((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" .
      (lambda (file-path link-string)
        (eww-open-file file-path)
        (cond
         ((string-match ".+::\\([[:digit:]]+\\)$" link-string)
          (goto-line (string-to-number (match-string 1 link-string))))
         ((string-match ".+::\\([[:graph:][:blank:]]+\\)$" link-string)
          (goto-char (search-forward (match-string 1 link-string) nil t))))))
     ("org.gpg" . emacs)
     ("tar.gpg" .
      (lambda (file-path link-string)
        (lexical-let (file)
          (deferred:$
            (deferred:process "orgafile" "play" file-path)))))
     ("\\(?:pdf\\|epub\\)\\'" .
      (lambda (file-path link-string)
        (deferred:$
          (deferred:process "orgafile" "convert" file-path)
          (deferred:nextc it
            (lambda (conv-file)
              (when (s-ends-with-p "\.html" conv-file)
                (eww-open-file conv-file)))))))
     ("\\(?:mp3\\|m4a\\|mp4\\|mkv\\|jpg\\|jpeg\\|png\\)\\'" .
      (lambda (file-path link-string)
        (deferred:$
          (deferred:process "orgafile" "play" file-path))))
     (directory .
                (lambda (file-path link-string)
                  (deferred:$
                    (deferred:process "orgafile" "play" file-path))))))
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
        '(("$" org-archive-subtree)
          ("a" org-attach)
          ("A" org-toggle-archive-tag)
          ("W" org-copy)
          ("Q" org-clock-cancel)
          ("/" org-sparse-tree-indirect-buffer)
          ("m" org-match-sparse-tree-indirect-buffer)
          ("z" org-narrow-to-element-indirect-buffer)
          ("!" org-readable)
          ("K" org-entry-kill-property)
          ("&" org-id-view-refs)
          ("N" org-add-note)
          ("T" org-set-tags-command)
          ("P" call-interactively 'org-set-property)
          ("s" call-interactively 'org-schedule)
          ("d" call-interactively 'org-deadline)))

  ;; display
  (add-hook 'org-mode-hook
            (lambda ()
              (whitespace-mode 1)
              (buffer-face-set 'outline)
              (setq-local line-spacing 0.1)
              (smart-newline-mode 1)))
  (add-hook 'org-agenda-mode-hook
            (lambda ()
              (buffer-face-set 'outline)
              (setq-local line-spacing 0.1)))
  (setq org-hide-leading-stars t)
  (setq org-pretty-entities t)
  (setq org-use-sub-superscripts '{}) ; use _{}/^{} for sub/super script

  ;; tag/property
  (setq org-use-tag-inheritance "^_.+")
  (setq org-tags-column -46)
  (setq org-global-properties
        '(("Effort_ALL". "0 0:10 0:30 1:00 2:00 4:00 6:00 8:00")))

  ;; priority
  (setq org-highest-priority ?A)
  (setq org-lowest-priority ?Z)
  (setq org-default-priority ?Z)
  (setq org-use-property-inheritance nil)

  ;; id
  (setq org-id-files (file-expand-wildcards (concat env-doc-dir "/**/*.org")))
  (setq org-id-locations-file (concat env-doc-dir "/.org-id-locations"))
  (setq org-id-track-globally t)
  (setq org-id-link-to-org-use-id 'create-if-interactive)

  ;; todo
  (setq org-todo-keywords
        '((sequence "HBT(h)" "URGE(u/!)" "TDAY(T/!)" "WEEK(w/!)" "TODO(t/!)" "PEND(p/!)" "|" "DONE(d/@)" "CXL(x@/@)")))
  (setq org-todo-keyword-faces
        '(("HBT"  . ((org-todo    (:foreground "OliveDrab1"))))
          ("URGE" . ((org-warning (:foreground "red1"))))
          ("TDAY" . ((org-todo    (:foreground "DodgerBlue1"))))
          ("WEEK" . ((org-todo    (:foreground "DeepPink1"))))
          ("TODO" . ((org-todo    (:foreground "green1"))))
          ("PEND" . ((org-todo    (:foreground "sea green"))))
          ("DONE" . ((org-done    (:foreground "gray30"))))
          ("CXL"  . ((org-done    (:foreground "dark gray"))))))
  ;; remove priority when the todo state moves to DONE|CXL|PND
  (add-hook  'org-after-todo-state-change-hook
             (lambda ()
               (interactive)
               (save-excursion
                 (org-back-to-heading t)
                 (let* ((element (org-element-at-point))
                        (todo-state (org-get-todo-state))
                        (priority (org-element-property :priority element)))
                   (when (and
                          (s-matches? "DONE\\|CXL\\|PND" todo-state)
                          (bound-and-true-p priority))
                     (org-priority ? ))))))

  ;; clock
  (setq org-clock-persist t)
  (setq org-clock-persist-query-resume nil)
  (org-clock-persistence-insinuate)
  (add-hook 'org-clock-in-hook
            (lambda () (save-buffer)))
  (add-hook 'org-clock-out-hook
            (lambda () (save-buffer)))
  (add-hook 'org-clock-cancel-hook
            (lambda () (save-buffer)))

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
  (setq org-clock-into-drawer "CLOCKLOG")
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

  ;; org-agenda
  (defvar org-agenda-files-default (append
                                    (file-expand-wildcards (concat env-doc-dir "/*_a"))
                                    (file-expand-wildcards (concat env-doc-dir "/**/*_a.org")))
    "Default org-agenda-files.")
  (setq org-agenda-files org-agenda-files-default)
  (setq org-agenda-prefix-format
        '((agenda   . " %?-12t%s")
          (timeline . "  %s")
          (todo     . " ")
          (tags     . " ")
          (search   . " ")))

  ;; org-capture
  (setq org-capture-bookmark nil)

  ;; clock table
  ; work around for the bug in emacs 25
  (defalias 'calendar-absolute-from-iso 'calendar-iso-to-absolute)

  ;; export
  (setq org-export-with-sub-superscripts nil)
  (setq org-comment-string "####")

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
    (custom-set-variables '(org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar")))
   ((string= system-type "darwin")
    (custom-set-variables '(org-plantuml-jar-path "/usr/local/Cellar/plantuml/8041/plantuml.8041.jar"))))

  ;; src
  (setq org-src-window-setup 'current-window)

  ;; org mode hook
  (add-hook 'org-mode-hook
            (lambda ()
              (yas-minor-mode 1)
              (reveal-mode 1)))

  ;; save all org files every hour
  (run-at-time 10 300 'org-save-all-org-buffers)

  (with-eval-after-load "50-org-life.el" (org-life)))

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

(defun org-readable ()
  "Show org subtree in eww by creating html file."
  (interactive)
  (org-copy-subtree)
  (let* ((id (org-id-get))
         (uuid (downcase (if id id (org-id-uuid))))
         (file (format "~/var/lib/readable/%s.html" uuid)))
    (call-process-shell-command (format "echo %s | pandoc -f org+hard_line_breaks+east_asian_line_breaks -t html -o %s" (shell-quote-argument org-subtree-clip) file))
    (eww-open-file file)))

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
