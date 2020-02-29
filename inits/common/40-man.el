(use-package man
  :delight " MAN"
  :after (hydra)
  :bind (:map Man-mode-map
              ("C-M-m" . hydra-lazy-control/body))
  :hook (Man-mode . (lambda ()
                      (buffer-face-set 'visible)
                      (visual-line-mode 1)
                      (adaptive-wrap-prefix-mode 1))))

(use-package woman
  :delight " WM")
