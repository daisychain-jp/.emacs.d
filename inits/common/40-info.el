(use-package Info
  :delight " IF"
  :defer t
  :bind (:map Info-mode-map
              ("C-o" . Info-follow-nearest-node)
              ("a"   . info-apropos))
  :hook (Info-mode . (lambda ()
                       (buffer-face-set 'recognizable)
                       (visual-line-mode 1)
                       (adaptive-wrap-prefix-mode 1))))
