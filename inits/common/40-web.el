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
                      (setq-local helm-dash-docsets '("HTML" "HTTP"))))
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
                      (setq-local helm-dash-docsets '("CSS"))))
  :custom
  (css-indent-offset 2))

(use-package js
  :delight " JS"
  :custom
  (js-indent-level 2))

(use-package js2-mode
  :straight t
  :delight " J2"
  :mode (("\\.js\\(on\\)?\\'" . js2-mode)
         ("\\.eslintrc\\'"    . js2-mode))
  :hook (js2-mode . (lambda ()
                      (setq-local flycheck-checker 'javascript-eslint)
                      (setq-local flycheck-disabled-checkers '(javascript-jshint javascript-jscs))
                      (setq-local helm-dash-docsets '("JavaScript" "HTTP" "NodeJS"))))
  :custom
  (flycheck-javascript-eslint-executable "eslint"))
