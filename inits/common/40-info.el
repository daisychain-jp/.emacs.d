(use-package Info
  :delight " IF"
  :defer t
  :bind (:map Info-mode-map
              ("C-o" . Info-follow-nearest-node))
  :hook (Info-mode . (lambda ()
                       (buffer-face-set 'recognizable)
                       (toggle-truncate-lines 0)
                       (adaptive-wrap-prefix-mode 1))))
