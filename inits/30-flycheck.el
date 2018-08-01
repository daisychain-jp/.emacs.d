(el-get-bundle flycheck)
(use-package flycheck
  :diminish (flycheck-mode . "fc")
  :config
  (bind-keys :map flycheck-error-list-mode-map
             ("o" . delete-other-windows))
  (setq flycheck-check-syntax-automatically
        '(save new-line idle-change))
  (setq flycheck-display-errors-delay 60.0)
  (setq flycheck-checker-error-threshold 1000))

(add-hook 'c-mode-hook 'flycheck-mode)
