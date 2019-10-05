;;; 40-sql.el ---
(use-package sql
  :delight " SQ"
  :commands (sql-mode)
  :mode (("\\.sql$" . sql-mode))
  :hook (sql-interactive-mode . (lambda ()
                                  (setq-local truncate-lines t)))
  :config
  (add-hook 'sql-mode-hook
            '(lambda ()
               (setq-local flycheck-checker 'sql-sqlint)
               (setq-local counsel-dash-docsets '("MySQL")))))
