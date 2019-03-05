(use-package help-mode
  :delight " HE"
  :bind (:map help-mode-map
              ("C-M-m" . hydra-lazy-control/body))
  :config
  (add-hook 'help-mode-hook
            (lambda ()
              (if (member (buffer-name) '("*Help*" "*Google Translate*"))
                  (progn
                    (setq-local truncate-lines nil)
                    (adaptive-wrap-prefix-mode 1)
                    (hydra-lazy-control/body))
                (setq-local truncate-lines t))
              (buffer-face-set 'recognizable))))
