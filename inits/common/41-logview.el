(use-package logview
  :delight
  (log-view-mode " LV")
  :mode ("\\.log$" . log-view-mode)
  :init
  (add-hook 'log-view-mode-hook
            'coding-mode-hook-function)
  (add-hook 'log-view-mode-hook
            (lambda ()
              (auto-revert-tail-mode 1)
              (setq-local truncate-lines t))))
(diminish 'auto-revert-tail-mode "tl")
