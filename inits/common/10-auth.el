(use-package auth-source
  :custom
  (auth-source-gpg-encrypt-to `(,user-mail-address))
  (auth-sources '("~/.authinfo.gpg" "~/.netrc.gpg")))

(use-package auth-source-pass
  :config
  (auth-source-pass-enable))

(use-package password-store
  :straight t
  :custom
  (password-store-time-before-clipboard-restore 25))

(use-package pass
  :straight t
  :config
  (bind-keys :map pass-mode-map
             ("C-j" . pass-view)))


(defcustom my/org-password-store-property "PASSWORD_STORE"
  "Property used in my/password-store feature to get password-store entry name attached to current org entry.")

(defmacro with-password-store-entry (entry &rest body)
  "Eval BODY which can refer password-store ENTRY.."
  (declare (indent defun))
  `(let ((entry (or ,entry
                    (when (derived-mode-p 'org-mode) (org-entry-get (point) my/org-password-store-property))
                    (password-store--completing-read t))))
     ,@body))

(defmacro with-password-store-entry-field (entry field &rest body)
  "Eval BODY which can refer password-store ENTRY and FIELD..

Plus, OBDY can refer value of FIELD named field-value."
  (declare (indent defun))
  `(let* ((entry (or ,entry
                     (when (derived-mode-p 'org-mode) (org-entry-get (point) my/org-password-store-property))
                     (password-store--completing-read t)))
          (field (or ,field
                     (password-store-read-field entry)))
          (field-value (password-store-get-field entry field)))
     ,@body))

(defun my/password-store-copy ()
  "Add password for ENTRY into the kill ring.

`password-store-copy' is responsible for managing the kill ring."
  (interactive)
  (with-password-store-entry nil
    (password-store-copy entry)))

(defun my/password-store-copy-field ()
  "Add field for entry into the kill ring.

entry and field is determined by appropriate manner."
  (interactive)
  (with-password-store-entry-field nil nil
    (password-store-copy-field entry field)))

(defun my/password-store-url (&optional arg)
  "Browse url stored in entry by appropriate manner determined by ARG."
  (interactive "P")
  (with-password-store-entry-field nil "url"
    (cl-case (prefix-numeric-value arg)
      (16 (browse-url field-value))
      (4 (eww-browse-url field-value))
      (t (open-url field-value)))))

(defun my/password-store-show-field ()
  "Show a field value in the minibuffer.

Password-store entry and field used to derive this value are from org property
or selected by user."
  (interactive)
  (with-password-store-entry-field nil nil
    (message "%s: %s" field field-value)))

(defun my/password-store-show-username ()
  (interactive)
  (with-password-store-entry-field nil "username"
    (message "%s: %s" field field-value)))

(defun my/password-store-create ()
  (interactive)
  (let* ((input (read-string "Entry-name or URL: "))
         (domain (when (string-match-p browse-url-button-regexp input)
                   (string-trim-right
                    (shell-command-to-string (format "echo %s | awk -F[/:] '{print $4}'"
                                                     (shell-quote-argument input)))))))
    (with-password-store-entry (cond
                                ((> (length domain) 0) domain)
                                (input))
      (when (derived-mode-p 'org-mode)
        (org-entry-put (point) my/org-password-store-property entry))
      (password-store-edit entry))))

(defun my/password-store-edit ()
  (interactive)
  (with-password-store-entry nil
    (password-store-edit entry)))

;; TODO: make my/password-store-insert as soon after
;;     : password-store--run-insert has been implemented

(defun my/password-store-remove ()
  (interactive)
  (with-password-store-entry nil
    (password-store-remove entry)
    (when (derived-mode-p 'org-mode)
      (org-entry-delete (point) my/org-password-store-property))))

(defun my/password-store-web-login ()
  (interactive)
  (with-password-store-entry-field nil "username"
    (password-store-copy entry)
    (sleep-for 0.2) ; work around for overlapping message
    (if field-value
        (message "%s: %s" field field-value)
      (when-let ((second (nth 1 (password-store-parse-entry entry))))
        (message "%s: %s" (car second) (cdr second))))
    (sleep-for 3)
    (funcall-interactively #'my/password-store-url '(16))))

(with-eval-after-load 'hydra
  (defhydra hydra-password-store (global-map "C-o *"
                                             :color teal)
    "Password store"
    ("cc" my/password-store-copy)
    ("cf" my/password-store-copy-field)
    ("sf" my/password-store-show-field)
    ("su" my/password-store-show-username)
    ("w" my/password-store-url)
    ("l" my/password-store-web-login)
    ("n" my/password-store-create)
    ("e" my/password-store-edit)
    ("v" my/password-store-edit)
    ("k" my/password-store-remove)
    ("q" nil "quit")))
