(let ((straight-current-profile 'pinned))
  (straight-use-package 'org)
  (add-to-list 'straight-x-pinned-packages
               '("org" . "d70db54dbc32feb2034f5a07a346fb8bb75160d1")))
(use-package org
  ;; you need to make in org directory in advance
  ;; $ make clean all
  :straight t
  :defer t
  :diminish ((org-src-mode . "os"))
  :mode (("\\.org$" . org-mode))
  :interpreter (("org" . org-mode))
  :init
  (defvar org-agenda-files-default
    (file-expand-wildcards (concat env-org-dir "/agenda/*.org"))
    "Default org-agenda-files.")
  (add-to-list 'load-path (concat (file-name-as-directory user-emacs-directory) "straight/repos/org/lisp"))
  (add-to-list 'load-path (concat (file-name-as-directory user-emacs-directory) "straight/repos/org/contrib/lisp"))
  :hook
  (org-load . (lambda ()
                (mapc (lambda (dir)
                        (dolist (f (file-expand-wildcards
                                    (concat (expand-file-name dir env-org-dir) "/*.org")))
                          (org-babel-lob-ingest f)))
                      '("agenda" "index" "wiki"))))
  :custom
  (org-directory env-org-dir)
  (org-special-ctrl-a/e t)
  (org-adapt-indentation nil)
  ;; do not split line at point
  (org-M-RET-may-split-line '((default . nil)))
  :config
  (require 'org-agenda)
  (require 'org-capture)
  (require 'org-habit)
  (require 'org-mu4e)

  ;; local key bindings
  (bind-keys :map org-mode-map
             ("C-j"     . org-return)
             ("C-m"     . org-return-indent)
             ("M-j"     . org-meta-return)
             ("C-S-p"   . org-previous-item)
             ("C-S-n"   . org-next-item)
             ("C-S-u"   . org-up-element)
             ("C-$"     . org-down-element)
             ("C-<"     . org-previous-link)
             ("C->"     . org-next-link)
             ("C-c &"   . org-mark-ring-goto)
             ("C-c V"   . org-download-video-link-at-point)
             ("C-c A"   . org-download-audio-link-at-point)
             ("C-c D"   . org-show-media-duration-at-point)
             ("C-c C-a" . nil)
             ("C-,"     . nil)
             ("M-h"     . nil)
             ("C-M-m"   . hydra-lazy-control/body)
             :map org-agenda-mode-map
             ("C-j" . org-agenda-switch-to)
             ("T" . counsel-org-tag-agenda)
             ("M" . org-agenda-month-view))

  ;; list
  (setq org-list-demote-modify-bullet
        '(("-" . "+") ("+" . "-") ("*" . "-")
          ("1." . "1)") ("1)" . "1.") ("*" . "1.")))
  (setq org-cycle-include-plain-lists 'integrate)

  ;; attach
  (setq org-attach-preferred-new-method nil)

  ;; link
  (setq org-confirm-elisp-link-function nil) ; do not confirm when execute elisp
  (org-link-set-parameters
   "src" :follow #'org-babel-ref-resolve)
  (defun org-open-at-point-link ()
    "This function is responsible for org links when user calls `org-open-at-point'."
    (let* ((context (org-element-lineage
                     (org-element-context)
                     '(link)
                     t))
           (type (org-element-property :type context))
           (path (org-element-property :path context))
           (app (org-element-property :application context))
           (search-option (org-element-property :search-option context)))
      (if (stringp type)
          (cond
           ((string-match-p "https?" type)
            (let ((url-pos (split-location-uri (org-link-unescape (concat type ":" path)))))
              (cl-case (prefix-numeric-value current-prefix-arg)
                (16 (browse-url-default-browser (car url-pos)))
                (4 (eww-browse-url (car url-pos)))
                (t (open-url (car url-pos) (cadr url-pos))))
              t))
           ((string= type "file")
            (let ((line-search (cond ((not search-option) nil)
                                     ((string-match-p "\\`[0-9]+\\'" search-option)
                                      (list (string-to-number search-option)))
                                     (t (list nil search-option)))))
              (apply #'my/org-open-file
                     path
                     (cond
                      ((equal app "sys") 'system)
                      ((equal app "emacs") 'emacs)
                      (t nil))
                     line-search))
            t))
        (open-thing-at-point))))
  (add-to-list 'org-open-at-point-functions 'org-open-at-point-link)
  (setq org-file-apps
        '((t . (lambda (file-path link-string)
                 (my/view-file file-path)))))
  (org-add-link-type
   "sudo"
   (lambda (cmd)
     "Run CMD with sudo."
     (shell-command
      (concat "echo " (shell-quote-argument (read-passwd "Password? "))
              " | sudo -S " cmd))))

  ;; speed command
  (setq org-use-speed-commands
        (lambda () (and (looking-at org-outline-regexp) (looking-back "^\**"))))
  (setq org-speed-commands-user
        '(("$" org-record-subtree)
          ("W" org-copy)
          ("Q" org-clock-cancel)
          ("!" org-readable)
          ("k" nil)
          ("c" org-property-copy-as-kill)
          ("%" org-reference-find-referrers)
          ("&" org-reference-refer-parent)
          ("N" org-add-note)
          ("T" counsel-org-tag)
          ("P" call-interactively 'org-set-property)
          ("s" call-interactively 'org-schedule)
          ("z" org-toggle-narrow-to-subtree)
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
              (setq-local truncate-lines t)))
  (add-hook 'org-agenda-mode-hook
            (lambda ()
              (buffer-face-set 'outline)
              (setq-local line-spacing 0.1)
              (delete-other-windows)
              (org-agenda-to-appt t '((category "appt")))))

  ;; tag/property
  (setq org-use-tag-inheritance "ARCHIVE")
  (setq org-tags-column -48)
  (setq org-global-properties
        '(("Effort_ALL". "0 0:10 0:20 0:30 1:00 1:30 2:00 3:00 4:00 6:00 8:00")))
  (setq org-use-property-inheritance "TIMELIMIT.*")
  (setq org-highest-priority ?A)
  (setq org-lowest-priority ?Z)
  (setq org-default-priority ?E)

  ;; todo
  (defvar org-warning-keyword-0 "UG" "TODO keyword acronym standing for 'UrGent'")
  (defvar org-todo-keyword-0 "TD" "TODO keyword acronym standing for 'To Do'")
  (defvar org-todo-keyword-1 "GO" "TODO keyword acronym standing for 'Get On'")
  (defvar org-todo-keyword-2 "IP" "TODO keyword acronym standing for 'In Progressn'")
  (defvar org-todo-keyword-3 "AB" "TODO keyword acronym standing for 'doABle'")
  (defvar org-todo-keyword-4 "SD" "TODO keyword acronym standing for 'SomeDay'")
  (defvar org-done-keyword-0 "DN" "TODO keyword acronym standing for 'DoNe'")
  (defvar org-done-keyword-1 "CX" "TODO keyword acronym standing for 'Cancel'")
  (defvar org-done-keyword-2 "PD" "TODO keyword acronym standing for 'PenDing'")
  (setq org-enforce-todo-dependencies t)
  (setq org-todo-keywords
        `((sequence ,(format "%s(t/!)" org-todo-keyword-0)
                    ,(format "%s(u/!)" org-warning-keyword-0)
                    ,(format "%s(g/!)" org-todo-keyword-1)
                    ,(format "%s(i/!)" org-todo-keyword-2)
                    "|"
                    ,(format "%s(d/@)" org-done-keyword-0)
                    ,(format "%s(x/@)" org-done-keyword-1)
                    ,(format "%s(p/@)" org-done-keyword-2))
          (sequence ,(format "%s(a/!)" org-todo-keyword-3)
                    "|"
                    ,(format "%s(d/@)" org-done-keyword-0)
                    ,(format "%s(x/@)" org-done-keyword-1)
                    ,(format "%s(p/@)" org-done-keyword-2))
          (sequence ,(format "%s(t/!)" org-todo-keyword-0)
                    ,(format "%s(s/!)" org-todo-keyword-4)
                    "|"
                    ,(format "%s(x/@)" org-done-keyword-1)
                    ,(format "%s(p/@)" org-done-keyword-2))))
  (setq org-todo-keyword-faces
        `((,org-warning-keyword-0 . "red1")
          (,org-todo-keyword-0    . "green1")
          (,org-todo-keyword-1    . "DeepPink1")
          (,org-todo-keyword-2    . "DodgerBlue1")
          (,org-todo-keyword-3    . "chocolate")
          (,org-todo-keyword-4    . "SpringGreen")
          (,org-done-keyword-0    . "gray30")
          (,org-done-keyword-1    . "dark gray")
          (,org-done-keyword-2    . "sea green")))
  (add-hook  'org-after-todo-state-change-hook
             (lambda ()
               (save-excursion
                 (let* ((element (org-element-at-point))
                        (todo-state (org-get-todo-state))
                        (tags (org-get-tags))
                        (priority (org-element-property :priority element))
                        (category (org-entry-get (point) "CATEGORY"))
                        (style (org-entry-get (point) "STYLE")))
                   ;; remove priority level when the to-do state is changed to DN|CX|PD
                   (when (and
                          (s-matches? (format "%1$s\\|%2$s\\|%3$s"
                                              org-done-keyword-0
                                              org-done-keyword-1
                                              org-done-keyword-2)
                                      todo-state)
                          (bound-and-true-p priority))
                     (org-priority ? ))
                   ;; remove DN state if CATEGORY of the entry is "Cyclic"
                   (when (and (string= category "Cyclic")
                              (string= todo-state org-done-keyword-0)
                              (not (member "episode" tags))
                              (not (string= style "habit")))
                     (org-todo ""))))))
  (add-hook 'org-after-todo-statistics-hook
            (lambda (n-done n-not-done)
              "Switch project entry to DONE when all subentries are done, to empty otherwise."
              (when (member "project" (org-get-tags))
                (org-todo (if (= n-not-done 0)
                              (prog1 org-done-keyword-0
                                (org-add-planning-info 'closed "now"))
                            "")))))

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
  (setq org-log-states-order-reversed t)
  (setq org-reverse-note-order nil)
  (add-hook 'org-log-buffer-setup-hook
            (lambda ()
              (setq skk-dcomp-activate 'eolp)
              (setq skk-dcomp-multiple-activate nil)))

  ;; org-agenda
  (setq org-agenda-files org-agenda-files-default)
  (setq org-agenda-prefix-format
        '((agenda   . "%?-12t% s")
          (timeline . "  %s")
          (todo     . " ")
          (tags     . " ")
          (search   . " ")))
  (dolist (dir '("archive" "index" "wiki"))
    (mapc (lambda (org-file)
            (add-to-list 'org-agenda-text-search-extra-files
                         org-file))
          (directory-files (expand-file-name dir env-org-dir)
                           t
                           (rx (one-or-more not-newline)
                               (or ".org" ".org.gpg")
                               line-end))))
  ;; these settings contribute to rapid building of agenda view
  (setq org-agenda-inhibit-startup t)
  (setq org-agenda-dim-blocked-tasks nil)
  (setq org-agenda-use-tag-inheritance nil)

  ;; org-capture
  (setq org-capture-bookmark nil)
  (setq org-bookmark-names-plist nil)

  ;; clock table
  ;;; work around for the bug in emacs 25
  (defalias 'calendar-absolute-from-iso 'calendar-iso-to-absolute)

  ;; image
  (setq org-image-actual-width 100)

  ;; src
  (setq org-src-window-setup 'current-window))

(use-package ob-core
  :after org
  :custom
  (org-confirm-babel-evaluate nil)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C        . t)
     (java     . t)
     (R        . t)
     (shell    . t)
     (ruby     . t)
     (python   . t)
     (org      . t)
     (gnuplot  . t)
     (dot      . t)
     (plantuml . t)
     (lilypond . t)))
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
  (cond
   ((string= system-type "gnu/linux")
    (custom-set-variables '(org-plantuml-jar-path (format "%s/lib/plantuml/plantuml.jar" env-var-dir))))
   ((string= system-type "darwin")
    (custom-set-variables '(org-plantuml-jar-path "/usr/local/Cellar/plantuml/8041/plantuml.8041.jar")))))

(use-package ob-async
  :straight t
  :after ob-core)

(use-package ox
  :defer t
  :after org
  :custom
  (org-export-with-smart-quotes t)
  (org-export-with-emphasize t)
  (org-export-with-special-strings t)
  (org-export-with-fixed-width t)
  (org-export-with-timestamps t)
  (org-export-preserve-breaks nil)
  (org-export-with-sub-superscripts nil)
  (org-export-with-archived-trees 'headline)
  (org-export-with-author nil)
  (org-export-with-broken-links 'mark)
  (org-export-with-clocks nil)
  (org-export-with-creator nil)
  (org-export-with-drawers '(not "LOGBOOK"))
  (org-export-with-date nil)
  (org-export-with-entities t)
  (org-export-with-email nil)
  (org-export-with-footnotes t)
  (org-export-headline-levels 5)
  (org-export-with-inlinetasks t)
  (org-export-with-section-numbers nil)
  (org-export-with-planning nil)
  (org-export-with-priority nil)
  (org-export-with-properties nil)
  (org-export-with-statistics-cookies t)
  (org-export-with-tags nil)
  (org-export-with-tasks t)
  (org-export-with-latex t)
  (org-export-time-stamp-file nil)
  (org-export-with-title t)
  (org-export-with-toc nil)
  (org-export-with-todo-keywords nil)
  (org-export-with-tables t)
  (org-export-default-language "ja")
  (org-export-dispatch-use-expert-ui nil))

(use-package ox-html
  :defer t
  :after ox
  :custom
  (org-html-preamble t)
  (org-html-postamble 'auto)
  (org-html-with-latex t)
  (org-html-container-element "div")
  (org-html-doctype "xhtml-strict"))

(use-package ox-latex
  :defer t
  :after ox
  :custom
  (org-latex-pdf-process '("platex %f"
                           "platex %f"
                           "bibtex %b"
                           "platex %f"
                           "platex %f"
                           "dvipdfmx %b.dvi"))
  (org-latex-default-class "jsarticle")
  :config
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
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

(use-package org-id
  :after org
  :custom
  (org-id-locations-file
   (expand-file-name ".org-id-locations" env-org-dir))
  (org-id-track-globally t)
  (org-id-extra-files
   (append org-agenda-text-search-extra-files))
  (org-id-link-to-org-use-id 'create-if-interactive))

(use-package org-timer
  :after org
  :hook
  (org-timer-done . (lambda ()
                      (when (and (org-clocking-p)
                                 org-clock-marker)
                        (let ((alert (org-entry-get org-clock-marker
                                                    "ALERT")))
                          (if (and (stringp alert)
                                   (string= alert "alarm"))
                              (alert "Timer Done!" :style 'alarm)
                            (alert "Timer Done!" :style 'fringe :mode 'org-mode :buffer (org-clocking-buffer) :severity 'trivial)))))))

(use-package org-clock
  :after org
  :hook
  ((org-clock-out org-clock-cancel) .
   (lambda () (and (boundp 'org-timer-countdown-timer)
                   org-timer-countdown-timer
                   (org-timer-stop))))
  ((org-clock-in org-clock-out org-clock-cancel) . save-buffer)
  (org-clock-in . (lambda ()
                    (let* ((opt '(4))
                           (attention-span (org-entry-get (point) "ATTENTION_SPAN" 'selective))
                           (effort (org-entry-get (point) "Effort" 'selective))
                           (org-timer-default-timer
                            (if (and (stringp attention-span)
                                     (< 0 (length attention-span)))
                                (progn
                                  (setq opt '(64))
                                  attention-span)
                              (default-value 'org-timer-default-timer)))
                           (time-default (decode-time (current-time))))
                      (when (or
                             ;; if "Effort" is less than 1:40 it's useless as timer value
                             (and (stringp effort)
                                  (apply #'time-less-p
                                         (mapcar (lambda (time)
                                                   (apply 'encode-time (mapcar* (lambda (x y) (or x y))
                                                                                (parse-time-string time)
                                                                                time-default)))
                                                 `(,effort "1:40"))))
                             attention-span)
                        (org-timer-set-timer opt)))))
  :custom
  (org-clock-out-when-done t)
  (org-clock-persist t)
  (org-clock-persist-query-resume nil)
  (org-clock-string-limit 20)
  (org-clock-continuously t)
  :config
  (org-clock-persistence-insinuate)
  (defconst org-clock-ts-line-re
    (concat "^[ \t]*" org-clock-string "[ \t]*" org-tsr-regexp-both)
    "Matches a line with clock time stamp."))

(defun org-clock-sum-all ()
  "Sum the times for all agenda files."
  (interactive)
  (save-excursion
    (mapc (lambda (file)
            (with-current-buffer (or (org-find-base-buffer-visiting file)
                                     (find-file-noselect file))
              (org-clock-sum)
              (org-clock-sum-today)))
          (org-agenda-files))))

(use-package org-archive
  :after org
  :custom
  (org-archive-default-command 'org-archive-set-tag))

(use-package org-ml
  :straight t
  :after org)

(use-package org-contacts
  :after org
  :custom
  (org-contacts-files `(,env-contacts-file)))

(use-package org-crypt
  :after org
  :custom
  (org-crypt-key "t.inamori@daisychain.jp")
  (org-tags-exclude-from-inheritance '("crypt"))
  (auto-save-default nil)
  :config
  (org-crypt-use-before-save-magic))

(use-package persist
  :straight t)
(use-package org-drill
  :after (org persist)
  :straight t
  :custom
  (org-drill-scope 'tree)
  (org-drill-cram-hours 0.5))

(use-package org-roam
  :straight (:host github
                   :repo "jethrokuan/org-roam"
                   :branch "master")
  :after org
  :diminish "or"
  :custom
  (org-roam-directory env-org-dir)
  (org-roam-db-location (expand-file-name "org-roam.db" env-org-dir))
  (org-roam-index-file (expand-file-name "index.org" env-org-dir))
  (org-roam-db-update-idle-seconds (* 10 60)))

(defcustom org-readable-directory "~/var/tmp/readable"
  "Directory where all html file for org-readable is located.")

(defun org-readable ()
  "Show current org subtree in EWW."
  (interactive)
  (let* ((heading (org-get-heading t t t t))
         (org-export-show-temporary-export-buffer nil)
         (export-buf-name "*Org HTML Export*")
         (org-export-with-broken-links 'mark)
         (id (org-id-get))
         (uuid (downcase (if id id (org-id-uuid))))
         (org-readable-file (format "%s/%s.html" org-readable-directory uuid)))
    (org-html-export-as-html nil t nil)
    (with-current-buffer export-buf-name
      (write-file org-readable-file)
      (eww-open-file org-readable-file))
    (kill-buffer export-buf-name)))

(defun org-mail-entry ()
  "Send a mail whose contents converted from current org entry.

Format of mail contents is plain text."
  (interactive)
  (let ((heading (org-get-heading t t t t))
        (org-export-show-temporary-export-buffer nil)
        (export-buf-name "*Org ASCII Export*")
        (org-export-with-toc nil)
        (org-export-with-author nil))
    (org-ascii-export-as-ascii nil t t t)
    (mail-simple-send (or (org-entry-get (point) "MAIL_TO" t)
                          (read-string "MAIL_TO: "))
                      heading
                      (with-current-buffer export-buf-name
                        (buffer-string)))
    (kill-buffer export-buf-name)))

(defun org-property-copy-as-kill ()
  "Prompt user to select property to append to the kill ring.

If property's value matches $(...) format, ... is interpreted as shell command and execute it."
  (interactive)
  (let* ((properties (org-entry-properties))
         (prop-key-to-copy
          (completing-read "Property name: "
                           (mapcar #'(lambda (var) (car var)) properties))))
    (when (stringp prop-key-to-copy)
      (let* ((prop-value (cdr (assoc prop-key-to-copy properties)))
             (str-to-copy
              (cond
               ((string-match "$(\\(.+\\))" prop-value)
                (shell-command-to-string (match-string 1 prop-value)))
               (t prop-value))))
        (kill-new str-to-copy)))))

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

;; WARN: This function does not work correctly
;;       Wait until library org-ml get mature
(defun org-gc-drawer-subtree ()
  "Remove all clocks and items in drawer of subtrees older than a month before."
  (interactive)
  (let* ((month-before (->> (decode-time (current-time))
                            (--map-indexed (if (= it-index 4)
                                               (- it 1) it))
                            (encode-time)))
         (config (list :log-into-drawer t
                       :clock-into-drawer t))
         (gc-clock-fun (lambda (headline)
                         (org-ml-update*
                           (org-ml-headline-map-logbook-clocks* config
                             (--filter
                              (time-less-p month-before
                                           (org-ml-time-to-unixtime
                                            (org-ml-timestamp-get-end-time
                                             (org-ml-get-property :value it))))
                              it)
                             it)
                           headline)))
         (gc-item-fun (lambda (headline)
                        (org-ml-update*
                          (org-ml-headline-map-logbook-items* config
                            (--filter
                             (time-less-p month-before
                                          (org-ml-logbook-item-get-timestamp it))
                             it)
                            it)
                          headline))))
    (->> (org-ml-parse-this-headline)
         (funcall gc-clock-fun))
    (->> (org-ml-parse-this-headline)
         (funcall gc-item-fun))
    (->> (org-ml-parse-this-subtree)
         (org-ml-headline-get-subheadlines)
         (-map gc-clock-fun))
    (->> (org-ml-parse-this-subtree)
         (org-ml-headline-get-subheadlines)
         (-map gc-item-fun))))

(add-hook 'org-cycle-hook #'org-cycle-hide-drawers t)
(add-hook 'org-cycle-hook #'org-cycle-hide-archived-subtrees t)
(add-hook 'org-cycle-hook #'org-cycle-show-empty-lines t)
(add-hook 'org-cycle-hook #'org-optimize-window-after-visibility-change t)

;;;###autoload
(defun org-refile-to-datetree-using-ts-in-entry (which-ts file)
  "Refile current entry to datetree in FILE using timestamp found in entry.
WHICH-TS should be `earliest' or `latest'."
  (interactive (list (intern (completing-read "Which timestamp? " '(earliest latest)))
                     (read-file-name "File: " (concat org-directory "/") nil 'mustmatch nil
                                     (lambda (filename)
                                       (string-suffix-p ".org" filename)))
                     current-prefix-arg))
  (let* ((sorter (pcase which-ts
                   ('earliest #'ts<)
                   ('latest #'ts>)))
         (tss (org-timestamps-in-entry))
         (ts (or (car (sort tss sorter))
                 (ts-now)))
         (date (list (ts-month ts) (ts-day ts) (ts-year ts))))
    (org-refile-to-datetree file :date date)))

;;;###autoload
(defun org-timestamps-in-entry ()
  "Return timestamp objects for all Org timestamps in entry."
  (interactive (list current-prefix-arg))
  (save-excursion
    (goto-char (org-entry-beginning-position))
    (org-show-entry)
    (org-narrow-to-element)
    (let* ((parsetree (org-element-parse-buffer))
           (ts-list nil))
      (org-element-map parsetree '(planning clock timestamp)
        (lambda (elm)
          (case (org-element-type elm)
            ('planning
             (add-to-list 'ts-list (ts-parse-org-element (or (org-element-property :closed elm)
                                                             (org-element-property :scheduled elm)
                                                             (org-element-property :deadline elm)))
                          t))
            ('clock
             (add-to-list 'ts-list (ts-parse-org-element (org-element-property :value elm)) t))
            ('timestamp
             (add-to-list 'ts-list (ts-parse-org-element elm) t)))))
      (widen)
      ts-list)))

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
        (let ((level (org-get-valid-level (funcall outline-level) 1))
              (heading (org-get-heading)))
          (org-end-of-subtree t t)
          (org-back-over-empty-lines)
          (org-paste-subtree level (or entry (car kill-ring)))
          (message "Refiled to %s" heading))
        (save-buffer))
    (error (unless entry
             (org-paste-subtree))
           (message "Unable to refile! %s" err))))

(use-package org-mime
  :straight t
  :after (org)
  :config
  (map-put org-speed-commands-user "M"
           (lambda (&optional arg)
             (interactive "P")
             (cond
              ((equal arg '(4))
               (call-interactively #'org-mail-entry))
              (t
               (let ((org-mime-use-property-inheritance t))
                 (call-interactively #'org-mime-org-subtree-htmlize)))))))

(use-package org-web-tools
  :straight t
  :demand t
  :bind (("C-c C-;" . org-web-tools-insert-link-for-url)))

(use-package org-clock-convenience
  :straight t
  :after (org)
  :bind (:map org-agenda-mode-map
              ("@"   . org-clock-convenience-fill-gap)
              ("C-@" . org-clock-convenience-fill-gap-both)))

(use-package org-clock-split
  :straight t
  :after (org))

(use-package org-ql
  :straight t
  :custom
  (org-ql-search-directories-files-recursive t)
  :config
  (require 'org-ql-search))

(defun my/org-open-file (path &optional in-emacs line search)
  "docstring"
  (case (prefix-numeric-value current-prefix-arg)
    (16 (my/view-file path t))
    (4 (org-open-file path t line search))
    (t (cond
        ((member in-emacs '((16) system))
         (my/view-file path t))
        ((member in-emacs '((4) emacs))
         (org-open-file path t line search))
        (t (my/view-file path)
           (when (or line search)
             (goto-pos (or line search))))))))

(defun org-download-video-link-at-point (&optional playlistp)
  "Download video file at point.
With `C-u' prefix arg, try to download all videos in playlist.

Video file is expected to appear in org-link."
  (interactive "P")
  (org-link-at-point-map (lambda (url title)
                           (download-video url title playlistp))))

(defun org-download-audio-link-at-point ()
  "Download audio file at point.

Audio file is expected to appear in org-link."
  (interactive)
  (org-link-at-point-map (lambda (url title)
                           (download-audio url title))))

(defun org-show-media-duration-at-point ()
  "Show duration of media at point."
  (interactive)
  (org-link-at-point-map (lambda (url title)
                           (show-media-duration url))))

(defun org-link-at-point-map (function)
  "Call `FUNCTION' with url and title obtained from org-link at point."
  (let* ((context (org-element-lineage
                   (org-element-context)
                   '(link)
                   t))
         (type (org-element-property :type context))
         (path (org-element-property :path context))
         (desc (when-let ((begin (org-element-property :contents-begin context))
                          (end (org-element-property :contents-end context)))
                 (buffer-substring begin end))))
    (cond
     ((string-match-p "https?" type)
      (funcall function (org-link-unescape (concat type ":" path)) desc))
     ((string-match-p "elfeed" type)
      (save-excursion
        (org-open-at-point)
        (when (eq major-mode 'elfeed-show-mode)
          (when-let ((url (or (caar (elfeed-entry-enclosures elfeed-show-entry))
                              (elfeed-entry-link elfeed-show-entry)))
                     (title (elfeed-entry-title elfeed-show-entry)))
            (funcall function url title))
          (quit-window)))))))
