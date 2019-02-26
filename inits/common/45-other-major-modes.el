(require 'generic-x)
(delight 'default-generic-mode " GE")
(delight 'alias-generic-mode " GE")

(delight 'conf-unix-mode " CF")

(delight 'fundamental-mode " FU")

(use-package help-mode
  :delight " HE"
  :bind (:map help-mode-map
              ("C-M-m" . hydra-lazy-control/body))
  :config
  (add-hook 'help-mode-hook
            (lambda ()
              (unless (string= (buffer-name) "*Help*")
                (setq-local truncate-lines t))
              (buffer-face-set 'recognizable))))

(add-hook 'Info-mode-hook
          (lambda ()
            (buffer-face-set 'recognizable)
            (setq-local truncate-lines t)
            (view-mode 1)))
