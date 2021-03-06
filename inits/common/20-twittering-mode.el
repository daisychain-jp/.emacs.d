(use-package twittering-mode
  :straight (twittering-mode
             :host github
             :repo "hayamiz/twittering-mode"
             :fork (:host github
                          :repo "daisychain-jp/twittering-mode"))
  :delight " TW"
  :after (hydra)
  :init
  (defface twit-buffer `((t . (:font "fontset-default"
                                     :height ,(my-adjust-font-size 1020))))
    "Default face for twittering-mode.")
  :custom
  (twittering-private-info-file
   (expand-file-name ".twittering-mode.gpg" user-emacs-directory))
  (twittering-user-id-db-file
   (expand-file-name ".twittering-mode-user-info.gz" user-emacs-directory))
  (twittering-use-master-password t)
  (twittering-use-ssl t)
  (twittering-number-of-tweets-on-retrieval 50)
  (twittering-status-format
   "%RT{%FACE[bold]{RT by %S\n}}%i %S, %@{}:\n%T %r%QT{\n+----\n%i %S, %@{}:\n%T %r%R\n+----}\n ")
  :config
  (twittering-icon-mode -1)
  (add-hook 'twittering-mode-hook
            (lambda ()
              (buffer-face-set 'twit-buffer)
              (twittering-lazy-control)))
  (bind-keys :map twittering-mode-map
             ("C-M-m" . twittering-lazy-control)))

(defun twittering-lazy-control ()
  "docstring"
  (interactive)
  (setq-local hlc/forward-paragraph-func
              (lambda ()
                (interactive)
                (twittering-goto-next-status)
                (recenter-top-bottom 0)))
  (setq-local hlc/backward-paragraph-func
              (lambda ()
                (interactive)
                (twittering-goto-previous-status)
                (recenter-top-bottom 0)))
  (setq-local hlc/next-thing-func
              'twittering-goto-next-thing)
  (setq-local hlc/previous-thing-func
              'twittering-goto-previous-thing)
  (setq-local hlc/quit-func
              'twittering-kill-buffer)
  (setq-local hlc/enter-func
              'twittering-enter)
  (hydra-lazy-control/body))

(defun twittering-goto-next-status-and-top ()
  (interactive)
  (twittering-goto-next-status)
  (recenter-top-bottom 0))
(defun twittering-goto-previous-status-and-top ()
  (interactive)
  (twittering-goto-previous-status)
  (recenter-top-bottom 0))
