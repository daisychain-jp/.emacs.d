;;; 40-sql.el ---
(use-package sql
  :delight " SQ"
  :commands (sql-mode)
  :mode (("\\.sql$" . sql-mode))
  :bind (:map sql-interactive-mode-map
              ("C-j" . comint-send-input))
  :hook (sql-interactive-mode . (lambda ()
                                  (setq-local truncate-lines t)))
  :config
  (add-hook 'sql-mode-hook
            '(lambda ()
               (setq-local flycheck-checker 'sql-sqlint)
               (setq-local counsel-dash-docsets '("MySQL")))))
