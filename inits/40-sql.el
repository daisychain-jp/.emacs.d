;;; 40-sql.el ---
(use-package sql
  :commands (sql-mode)
  :mode (("\\.sql$" . sql-mode))
  :config
  (custom-set-variables '(sql-product "mysql"))
  (add-hook 'sql-mode-hook
            '(lambda ()
               (setq-local flycheck-checker 'sql-sqlint)
               (setq-local helm-dash-docsets '("MySQL")))))
