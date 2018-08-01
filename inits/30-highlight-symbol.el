(el-get-bundle highlight-symbol)

(use-package highlight-symbol
  :bind (("C-3" . highlight-symbol)
         ("M-s M-r" . highlight-symbol-query-replace))
  :config
  ;; enable M-n / M-p in source code
  (add-hook 'prog-mode-hook 'highlight-symbol-nav-mode)
  (setq highlight-symbol-foreground-color "black")
  (setq highlight-symbol-colors
        '("DarkOrange"
          "DodgerBlue1"
          "DarkOliveGreen1")))
