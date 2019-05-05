(use-package Info
  :delight " IF"
  :defer t
  :hook (Info-mode . (lambda ()
                       (buffer-face-set 'recognizable)
                       (setq-local truncate-lines nil))))
