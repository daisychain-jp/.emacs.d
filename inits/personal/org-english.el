(defun my/org-english-capture (word)
  "Capture an english WORD as a org-mode entry suitable for org-drill.

Capture URL or file currently visiting as well as a sentence at point."
  (interactive (list (if (use-region-p)
                         (buffer-substring (region-beginning) (region-end))
                       (read-string "Phrase: "))))
  (let* ((sentence-end-double-space nil)
         (org-english-word word)
         (match-entry (org-ql-select
                        (org-record-files)
                        `(and (heading ,(format "[%s]" word))
                              (tags "drill"))
                        :action 'element-with-markers)))
    (kill-new (replace-regexp-in-string word (format "[%s]" word)
                                        (sentence-at-point)))
    (cond
     (match-entry (org-capture nil "!1"))
     (t (org-capture nil "!0")))))

(with-eval-after-load 'org-ql
  (push '("English phrase list"
          :buffers-files org-record-files
          :query (and (tags "drill")
                      (tags "fd_en")))
        org-ql-views))

(with-eval-after-load "org-life"
  (add-to-list 'org-capture-templates
               '("!0" "drill entry for english word"
                 entry (function org-goto-clocking-or-today)
                 "* [%i] :drill:fd_en:
[%(my/org-english-insert-japanese-translation)]
%(my/org-english-insert-english-translation)
- %a
  %c"))
  (add-to-list 'org-capture-templates
               '("!1" "drill entry for english word"
                 item(function my/org-english-goto-match)
                 "- %a\n  %c")))

(defun my/org-english-goto-match ()
  "Goto function for org-capture-template designed to insert an edditional example
to existing english word entry."
  (org-goto-marker-or-bmk
   (plist-get (cadar match-entry) :org-marker)))

(defun my/org-english-insert-english-translation ()
  "Insert English translation into org capture entry."
  (let* ((url (format "http://wordnik.com/words/%s" (downcase org-english-word)))
         (buffer (url-retrieve-synchronously url t t)))
    (with-temp-buffer
      (url-insert-buffer-contents buffer url)
      (ivy-read "match" (split-string (funcall 'my/define-word--parse-wordnik) "\n")))))

(defun my/org-english-insert-japanese-translation ()
  "Insert Japanese translation into org capture entry."
  (let* ((url (format "https://ejje.weblio.jp/content/%s" (downcase org-english-word)))
         (buffer (url-retrieve-synchronously url t t)))
    (with-temp-buffer
      (url-insert-buffer-contents buffer url)
      (funcall 'my/define-word--parse-weblio))))

(defmacro with-org-drill-english-config (&rest body)
  "Evaluate BODY with config for Org-Drill for english words review."
  `(let ((org-drill-scope '("~/org/archive/archive_2021.org"
                            "~/org/archive/archive_2020.org"
                            "~/org/archive/archive_2019.org"))
         (org-drill-question-tag "drill")
         (org-drill-days-before-old 20)
         (org-drill-maximum-items-per-session 20)
         (truncate-lines nil))
     (global-visual-line-mode 1)
     ,@body
     (global-visual-line-mode 0)))

(defun my/org-english-drill ()
  "Invoke Org-Drill for English word review."
  (interactive)
  (with-org-drill-english-config (org-drill)))

(defun my/org-english-drill-resume ()
  "Resume Org-Drill session for english word review."
  (interactive)
  (with-org-drill-english-config (org-drill-resume)))

(defun my/org-agenda-bulk-heading-wrap-bracket ()
  "Make org heading wrapped by brackets."
  (interactive)
  (let ((marker (or (org-get-at-bol 'org-hd-marker)
                    (org-agenda-error))))
    (org-with-point-at marker
      (org-show-all)
      (org-back-to-heading)
      (org-end-of-line)
      (unless (eq (char-before) ?\])
        (insert ?\]))
      (org-beginning-of-line)
      (unless (eq (char-after) ?\[)
        (insert ?\[)))))
