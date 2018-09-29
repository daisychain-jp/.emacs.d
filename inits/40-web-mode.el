(use-package web-mode
  :straight t
  :delight " WB"
  :mode (("\\.phtml$"     . web-mode)
         ("\\.tpl\\.php$" . web-mode)
         ("\\.jsp$"       . web-mode)
         ("\\.as[cp]x$"   . web-mode)
         ("\\.erb$"       . web-mode)
         ("\\.html?$"     . web-mode))
  :custom
  (web-mode-attr-indent-offset        2)
  (web-mode-attr-value-indent-offset  2)
  (web-mode-code-indent-offset        2)
  (web-mode-css-offset                2)
  (web-mode-markup-indent-offset      2)
  (web-mode-sql-indent-offset         2)
  (web-mode-block-padding             2)
  (web-mode-script-padding            2)
  :config
  (add-hook 'web-mode-hook 'my-web-mode-hook))
