(use-package mu4e
  :after (smtpmail)
  :delight
  (mu4e-main-mode    " MU")
  (mu4e-headers-mode " MU")
  (mu4e-view-mode    " MU")
  (mu4e-compose-mode " MU")
  :hook (after-init . (lambda () (mu4e t)))
  :init
  (defface mu4e-compose-buffer `((t . (:font "fontset-default"
                                             :height ,(my-adjust-font-size 630))))
    "Default face for mu4e compose buffer."
    :group 'mu4e-faces)
  (defface mu4e-view-buffer `((t . (:font "fontset-variable"
                                          :height ,(my-adjust-font-size 835))))
    "Default face for mu4e view buffer."
    :group 'mu4e-faces)
  :custom
  (mu4e-mu-binary (expand-file-name "mu" "~/usr/bin"))
  (mail-user-agent 'mu4e-user-agent)
  (mu4e-hide-index-messages t)
  (mu4e-context-policy 'pick-first)
  (mu4e-compose-context-policy 'ask)
  (mu4e-update-interval 300)
  (mu4e-split-view t)
  (mu4e-headers-date-format "%y%m%d")
  (mu4e-headers-time-format " %R")
  (mu4e-headers-fields
   '((:human-date . 6)
     (:flags      . 3)
     (:from       . 16)
     (:subject)))
  (mu4e-headers-results-limit 1000)
  (mu4e-headers-advance-after-mark t)
  (mu4e-change-filenames-when-moving t)
  (mu4e-view-show-images t)
  (mu4e-html2text-command (lambda (msg)
                            (plist-put msg :body-html
                                       (with-temp-buffer
                                         (insert (or (mu4e-message-field msg :body-html) ""))
                                         (shell-command-on-region (point-min) (point-max) "nkf -w -Lu" (current-buffer) t)
                                         (or (buffer-string) "")))
                            (mu4e-shr2text msg)))
  (message-send-mail-function 'smtpmail-send-it)
  (mm-sign-option nil)
  (mml-secure-openpgp-sign-with-sender t)
  (mml-default-sign-method 'pgpmime)
  :config
  (bind-keys :map mu4e-headers-mode-map
             ("C-j" . mu4e-headers-view-message)
             :map mu4e-view-mode-map
             ("C-M-m" . hydra-lazy-control/body)
             :map mu4e-compose-mode-map
             ("C-x C-o" . org-mu4e-compose-org-mode))
  (add-to-list 'mu4e-view-actions
               '("external browser" . mu4e-action-view-in-browser) t)
  (add-to-list 'mu4e-view-actions
               '("XWidget View" . mu4e-action-view-with-xwidget) t)
  ;; integrate with org-contacts
  (setq mu4e-org-contacts-file org-contacts-file)
  (add-to-list 'mu4e-headers-actions
               '("Contact to add" . mu4e-action-add-org-contact) t)
  (add-to-list 'mu4e-view-actions
               '("Contact to add" . mu4e-action-add-org-contact) t)
  (require 'gnus-dired)
  ;; make the `gnus-dired-mail-buffers' function also work on
  ;; message-mode derived modes, such as mu4e-compose-mode
  (defun gnus-dired-mail-buffers ()
    "Return a list of active message buffers."
    (let (buffers)
      (save-current-buffer
        (dolist (buffer (buffer-list t))
          (set-buffer buffer)
          (when (and (derived-mode-p 'message-mode)
                     (null message-sent-message-via))
            (push (buffer-name buffer) buffers))))
      (nreverse buffers)))
  (setq gnus-dired-mail-mode 'mu4e-user-agent)
  (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)
  (add-hook 'mu4e-view-mode-hook
            (lambda ()
              (buffer-face-set 'mu4e-view-buffer)
              (setq-local hlc/forward-paragraph-func
                          (lambda ()
                            (interactive)
                            (forward-paragraph 2)
                            (backward-paragraph 1)
                            (forward-line 1)
                            (recenter-top-bottom 0)))
              (setq-local hlc/backward-paragraph-func
                          (lambda ()
                            (interactive)
                            (backward-paragraph 2)
                            (forward-paragraph 1)
                            (backward-paragraph 1)
                            (forward-line 1)
                            (recenter-top-bottom 0)))
              (setq-local hlc/next-thing-func #'shr-next-link)
              (setq-local hlc/previous-thing-func #'shr-previous-link)
              (setq-local hlc/enter-func #'shr-browse-url)
              (setq-local hlc/quit-func 'mu4e~view-quit-buffer)
              (hydra-lazy-control/body)))
  (add-hook 'mu4e-compose-mode-hook
            (lambda ()
              (buffer-face-set 'mu4e-compose-buffer)))
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types)))

(use-package mu4e-alert
  :straight t
  :hook (after-init . (lambda ()
                        (mu4e-alert-enable-notifications)
                        (mu4e-alert-enable-mode-line-display)))
  :after (mu4e)
  :custom
  (mu4e-alert-modeline-formatter
   (lambda (count)
     (if (> count 0)
         (format " M:%d" count)
       "")))
  :config
  (mu4e-alert-set-default-style 'fringe))
