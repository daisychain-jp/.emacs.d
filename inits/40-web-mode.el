(el-get-bundle web-mode)

(use-package web-mode
  :mode (("\\.phtml$"     . web-mode)
         ("\\.tpl\\.php$" . web-mode)
         ("\\.jsp$"       . web-mode)
         ("\\.as[cp]x$"   . web-mode)
         ("\\.erb$"       . web-mode)
         ("\\.html?$"     . web-mode))
  :config
  (add-hook 'web-mode-hook 'my-web-mode-hook)
  )

(defun my-web-mode-hook ()
  (setq web-mode-attr-indent-offset        2)
  (setq web-mode-attr-value-indent-offset  2)
  (setq web-mode-code-indent-offset        2)
  (setq web-mode-css-offset                2)
  (setq web-mode-markup-indent-offset      2)
  (setq web-mode-sql-indent-offset         2)

  (setq web-mode-block-padding 2)
  (setq web-mode-script-padding 2)
  )
