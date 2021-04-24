(use-package woman
  :delight " WM"
  :after (hydra)
  :bind (:map woman-mode-map
              ("C-M-m" . hydra-lazy-control/body))
  :init
  (defface woman-buffer `((t . (:font "fontset-default"
                                      :height ,(my-adjust-font-size 835))))
    "Default face in woman mode.")
  :hook (woman-mode . (lambda ()
                        (buffer-face-set 'woman-buffer)
                        (visual-line-mode 1)
                        (adaptive-wrap-prefix-mode 1))))
