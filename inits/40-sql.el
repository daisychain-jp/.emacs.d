;;; 40-sql.el ---
(use-package sql
  :delight " SQ"
  :commands (sql-mode)
  :mode (("\\.sql$" . sql-mode))
  :config
  (add-hook 'sql-mode-hook
            '(lambda ()
               (setq-local flycheck-checker 'sql-sqlint)
               (setq-local helm-dash-docsets '("MySQL")))))
