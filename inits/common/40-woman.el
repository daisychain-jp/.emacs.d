(use-package woman
  :delight " WM"
  :after (hydra)
  :bind (:map woman-mode-map
              ("C-M-m" . hydra-lazy-control/body))
  :hook (woman-mode . (lambda ()
                        (buffer-face-set 'visible)
                        (visual-line-mode 1)
                        (adaptive-wrap-prefix-mode 1))))
