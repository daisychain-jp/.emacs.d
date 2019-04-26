(use-package mu4e
  :delight
  (mu4e-main-mode    " MU")
  (mu4e-headers-mode " MU")
  (mu4e-view-mode    " MU")
  (mu4e-compose-mode " MU")
  :hook (after-init . (lambda () (mu4e t)))
  :custom
  (mu4e-mu-binary env-mu4e-mu-binary)
  (mail-user-agent 'mu4e-user-agent)
  (auth-source-gpg-encrypt-to '("t.inamori@daisychain.jp"))
  (mu4e-get-mail-command "mbsync --quiet daisychain gmail yahoo")
  (mu4e-hide-index-messages t)
  (mu4e-maildir "~/var/mail")
  (mu4e-mu-home "~/var/lib/mu")
  (mu4e-context-policy 'pick-first)
  (mu4e-compose-context-policy 'ask)
  (mu4e-update-interval 300)
  (mu4e-maildir-shortcuts
   '(("/daisychain/Inbox"   . ?i)
     ("/daisychain/archive" . ?r)
     ("/daisychain/Sent"    . ?s)
     ("/gmail/Inbox"        . ?I)
     ("/gmail/past"         . ?R)))
  (mu4e-split-view t)
  (mu4e-headers-date-format "%y%m%d")
  (mu4e-headers-time-format " %R")
  (mu4e-headers-fields
   '((:human-date . 6)
     (:flags      . 3)
     (:from       . 16)
     (:subject)))
  (mu4e-headers-advance-after-mark t)
  (mu4e-change-filenames-when-moving t)
  (mu4e-view-show-images t)
  (mu4e-html2text-command #'mu4e-shr2text)
  :config
  (bind-keys :map mu4e-headers-mode-map
             ("C-j" . mu4e-headers-view-message)
             :map mu4e-view-mode-map
             ("C-M-m" . hydra-lazy-control/body)
             :map mu4e-compose-mode-map
             ("C-c o" . org-mu4e-compose-org-mode))
  (add-hook 'mu4e-headers-mode-hook
            (lambda ()
              (buffer-face-set 'selecting)))
  (add-hook 'mu4e-view-mode-hook
            (lambda ()
              (buffer-face-set 'variable-pitch)
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
              (buffer-face-set 'visible)))
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))
  (setq mu4e-contexts
        `(,(make-mu4e-context
            :name "Daisychain"
            :enter-func (lambda () (mu4e-message "Entering Daisychain context"))
            :leave-func (lambda () (mu4e-message "Leaving Daisychain context"))
            :match-func (lambda (msg)
                          (when msg
                            (string-match-p "^/daisychain" (mu4e-message-field msg :maildir))))
            :vars '((user-mail-address            . "t.inamori@daisychain.jp")
                    (user-full-name               . "Takayuki INAMORI")
                    (mu4e-sent-folder             . "/daisychain/Sent")
                    (mu4e-drafts-folder           . "/daisychain/Draft")
                    (mu4e-trash-folder            . "/daisychain/deleted")
                    (mu4e-refile-folder           . "/daisychain/archive")
                    (smtpmail-default-smtp-server . "smtp.daisychain.jp")
                    (smtpmail-local-domain        . nil)
                    (smtpmail-smtp-user           . "t.inamori")
                    (smtpmail-smtp-server         . "smtp.daisychain.jp")
                    (smtpmail-stream-type         . ssl)
                    (smtpmail-smtp-service        . 465)
                    (mu4e-compose-signature . (concat
                                               "稲守 孝之 (INAMORI Takayuki)\n"
                                               "t.inamori@daisychain.jp\n"))))
          ,(make-mu4e-context
            :name "Gmail"
            :enter-func (lambda () (mu4e-message "Entering Gmail context"))
            :leave-func (lambda () (mu4e-message "Leaving Gmail context"))
            :match-func (lambda (msg)
                          (when msg
                            (string-match-p "^/gmail" (mu4e-message-field msg :maildir))))
            :vars '((user-mail-address            . "takayuki.inamori@gmail.com")
                    (user-full-name               . "Takayuki INAMORI")
                    (mu4e-sent-folder             . "/gmail/Sent Messages")
                    (mu4e-drafts-folder           . "/gmail/Drafts")
                    (mu4e-trash-folder            . "/gmail/Deleted Messages")
                    (mu4e-refile-folder           . "/gmail/past")
                    (smtpmail-default-smtp-server . "smtp.gmail.com")
                    (smtpmail-local-domain        . "gmail.com")
                    (smtpmail-smtp-user           . "takayuki.inamori@gmail.com")
                    (smtpmail-smtp-server         . "smtp.gmail.com")
                    (smtpmail-stream-type         . ssl)
                    (smtpmail-smtp-service        . 465)
                    (mu4e-compose-signature . (concat
                                               "稲守 孝之 (INAMORI Takayuki)\n"
                                               "takayuki.inamori@gmail.com\n"))))
          ,(make-mu4e-context
            :name "Yahoo"
            :enter-func (lambda () (mu4e-message "Entering Yahoo context"))
            :leave-func (lambda () (mu4e-message "Leaving Yahoo context"))
            :match-func (lambda (msg)
                          (when msg
                            (string-match-p "^/yahoo" (mu4e-message-field msg :maildir))))
            :vars '((user-mail-address            . "tinamo@yahoo.co.jp")
                    (mu4e-sent-folder             . "/gmail/Sent")
                    (mu4e-drafts-folder           . "/yahoo/Draft")
                    (mu4e-trash-folder            . "/yahoo/Trash")
                    (smtpmail-local-domain        . "yahoo.co.jp")
                    (smtpmail-smtp-user           . "tinamo@yahoo.co.jp")
                    (smtpmail-smtp-server         . "smtp.mail.yahoo.co.jp")
                    (smtpmail-stream-type         . ssl)
                    (smtpmail-smtp-service        . 465))))))

(use-package mu4e-alert
  :straight t
  :hook (after-init . (lambda ()
                        (mu4e-alert-enable-notifications)
                        (mu4e-alert-enable-mode-line-display)))
  :after (mu4e)
  :custom
  (mu4e-alert-interesting-mail-query
   (mapconcat #'identity
              '("flag:unread AND NOT flag:trashed AND maildir:/daisychain/Inbox"
                "OR"
                "flag:unread AND NOT flag:trashed AND maildir:/daisychain/check"
                "OR"
                "flag:unread AND NOT flag:trashed AND maildir:/gmail/Inbox"
                "OR"
                "flag:unread AND NOT flag:trashed AND maildir:/yahoo/Inbox")
              " "))
  (mu4e-alert-modeline-formatter
   (lambda (count)
     (if (> count 0)
         (format " M:%d" count)
       "")))
  :config
  (mu4e-alert-set-default-style 'fringe))
