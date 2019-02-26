;; MEW is not actively used
;; (use-package mew
;;   :straight t
;;   :commands (mew mew-send)
;;   :config
;;   ;; Optional setup (Read Mail menu):
;;   (setq read-mail-command 'mew)

;;   ;; Optional setup (e.g. C-xm for sending a message):
;;   (autoload 'mew-user-agent-compose "mew" nil t)
;;   (if (boundp 'mail-user-agent)
;;       (setq mail-user-agent 'mew-user-agent))
;;   (if (fboundp 'define-mail-user-agent)
;;       (define-mail-user-agent
;;         'mew-user-agent
;;         'mew-user-agent-compose
;;         'mew-draft-send-message
;;         'mew-draft-kill
;;         'mew-send-hook))

;;   (setq mew-config-file (concat user-emacs-directory "mew/init.el"))
;;   (setq mew-conf-path (concat env-var-dir "/lib/mew/Mail"))
;;   (setq mew-mail-path (concat env-var-dir "/lib/mew/Mail"))

;;   (bind-keys :map mew-summary-mode-map
;;              ("C-j" . mew-summary-scroll-up)
;;              ("C-M-j" . mew-summary-scroll-down)
;;              ("!" . zoom-another-window)
;;              :map mew-message-mode-map
;;              ("!" . zoom-another-window))

;;   (setq mew-rc-file mew-config-file)

;;   ;; use stunnel for SSL connection
;;   (setq mew-prog-ssl env-mew-prog-ssl)
;;   (setq mew-prog-est env-mew-prog-est)

;;   ;; auto-refile
;;   (setq mew-refile-guess-alist
;;         '(("To:" (".*" "%past"))))

;;   ;; biff
;;   (setq mew-use-biff t)
;;   (setq mew-use-biff-bell t)
;;   (setq mew-biff-interval 1)
;;   (setq mew-biff-function 'my-mew-biff-bark)

;;   ;; use Hyper Estraier for searching full-text
;;   (setq mew-search-method 'est)

;;   ;; insert signature in last part
;;   (setq mew-signature-as-lastpart t)
;;   (setq mew-signature-insert-last t)

;;   ;; use master password
;;   (setq mew-use-cached-passwd t)
;;   (setq mew-use-master-passwd t) ; requires gpg version 1

;;   ;; marking strategy
;;   (setq mew-use-unread-mark t)
;;   (setq mew-delete-unread-mark-by-mark nil)
;;   (setq mew-summary-form-mark-delete t)

;;   ;; mime
;;   (setq mew-use-text/html t)
;;   (setq mew-use-text/xml t)

;;   (setq mew-cite-strings-function
;;         'my-mew-cite-strings-function)

;;   (add-hook 'mew-message-mode-hook
;;             (lambda ()
;;               (buffer-face-set 'readable)
;;               (view-mode t)))
;;   (add-hook 'mew-draft-mode-hook
;;             (lambda ()
;;               (buffer-face-set 'visible)))
;;   (add-hook 'mew-summary-mode-hook
;;             (lambda ()
;;               (buffer-face-set 'visible)
;;               (setq-local line-spacing 0.1)))
;;   (run-at-time 1800 3600 #'mew-est-index-all))

;; ;; notify on mode-line if some mail has arrived
;; (defun my-mew-biff-bark (n)
;;   (if (= n 0)
;;       (setq mew-biff-string nil)
;;     (if (and mew-use-biff-bell (eq mew-biff-string nil))
;;         (beep))
;;     (setq mew-biff-string (format " M:%d" n))))

;; (defun my-mew-cite-strings-function ()
;;   (let* ((date (mew-cite-get-value mew-date:))
;;          (from (mew-cite-get-value mew-from:))
;;          (cite-date (cond
;;                      ((and date (string-match mew-time-rfc-regex date))
;;                       (format "%sth %s %s %s:%s"
;;                               (match-string 1 date)
;;                               (match-string 2 date)
;;                               (match-string 3 date)
;;                               (match-string 4 date)
;;                               (match-string 5 date)))
;;                      (t "Unknown")))
;;          (cite-from (or from "Unknown")))
;;     (format "\nOn %s, %s wrote:\n" cite-date cite-from)))
