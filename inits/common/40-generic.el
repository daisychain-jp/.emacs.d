(use-package generic-x
  :mode (("\\.conf$"     . apache-conf-generic-mode)
         ("\\.htaccess$" . apache-conf-generic-mode))
  :delight
  (default-generic-mode     " GE")
  (apache-conf-generic-mode " GE")
  (alias-generic-mode       " GE")
  :config
  (add-hook 'after-change-major-mode-hook
            (lambda ()
              (when (eq major-mode 'apache-conf-generic-mode)
                (setq-local helm-dash-docsets '("Apache_HTTP_Server"))))))
