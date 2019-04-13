(use-package man
  :delight " MAN"
  :after (hydra)
  :bind (:map Man-mode-map
              ("C-M-m" . hydra-lazy-control/body))
  :hook (Man-mode . (lambda ()
                      (buffer-face-set 'visible)
                      (setq-local truncate-lines nil)
                      (adaptive-wrap-prefix-mode 1)
                      (hydra-lazy-control/body))))
