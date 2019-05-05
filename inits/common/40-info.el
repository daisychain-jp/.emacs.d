(use-package Info
  :delight " IF"
  :defer t
  :hook (Info-mode . (lambda ()
                       (buffer-face-set 'recognizable)
                       (toggle-truncate-lines 0)
                       (adaptive-wrap-prefix-mode 1))))
