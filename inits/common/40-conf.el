(use-package conf-mode
  :hook
  ((conf-space-mode
    conf-unix-mode
    conf-ppd-mode
    conf-colon-mode
    conf-xdefaults-mode
    conf-javaprop-mode
    conf-toml-mode
    conf-windows-mode
    conf-desktop-mode)
   . (lambda ()
       (toggle-truncate-lines 1))))

(use-package generic-x
  :mode (("\\.conf$"     . apache-conf-generic-mode)
         ("\\.htaccess$" . apache-conf-generic-mode))
  :delight
  (default-generic-mode     " GE")
  (apache-conf-generic-mode " GE")
  (alias-generic-mode       " GE")
  :config
  (toggle-truncate-lines 1)
  (add-hook 'after-change-major-mode-hook
            (lambda ()
              (when (eq major-mode 'apache-conf-generic-mode)
                (setq-local counsel-dash-docsets '("Apache_HTTP_Server"))))))
