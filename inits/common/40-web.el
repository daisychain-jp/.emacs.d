(use-package web-mode
  :straight t
  :delight " WB"
  :mode (("\\.phtml$"     . web-mode)
         ("\\.tpl\\.php$" . web-mode)
         ("\\.jsp$"       . web-mode)
         ("\\.as[cp]x$"   . web-mode)
         ("\\.erb$"       . web-mode)
         ("\\.html?$"     . web-mode))
  :hook (web-mode . (lambda ()
                      (setq-local helm-dash-docsets helm-dash-docsets-web-mode)))
  :custom
  (web-mode-attr-indent-offset        2)
  (web-mode-attr-value-indent-offset  2)
  (web-mode-code-indent-offset        2)
  (web-mode-css-offset                2)
  (web-mode-markup-indent-offset      2)
  (web-mode-sql-indent-offset         2)
  (web-mode-block-padding             2)
  (web-mode-script-padding            2))

(use-package css-mode
  :hook (css-mode . (lambda ()
                      (setq-local helm-dash-docsets helm-dash-docsets-css-mode)))
  :custom
  (css-indent-offset 2))

(use-package js
  :delight " JS"
  :hook (js-mode . (lambda ()
                     (setq-local helm-dash-docsets helm-dash-docsets-js-mode)))
  :custom
  (js-indent-level 2))
