(require 'generic-x)
(delight 'default-generic-mode " GE")

(delight 'fundamental-mode " FU")

(use-package help-mode
  :delight " HE"
  :bind (:map help-mode-map
              ("C-M-m" . hydra-lazy-control/body))
  :config
  (add-hook 'help-mode-hook
            (lambda ()
              (buffer-face-set 'recognizable)
              (adaptive-wrap-prefix-mode 1)
              (if (string= (buffer-name) "*Help*")
                  (progn
                    (setq-local truncate-lines nil)
                    (setq-local hlc/enter-func #'push-button)
                    (hydra-lazy-control/body))
                (setq-local truncate-lines t)))))

(add-hook 'Info-mode-hook
          (lambda ()
            (buffer-face-set 'recognizable)
            (setq-local truncate-lines t)
            (view-mode 1)))
