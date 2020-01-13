(use-package whitespace
  :diminish ((global-whitespace-mode . "Ws")
             (whitespace-mode . "ws"))
  :config
  (global-whitespace-mode -1)
  (setq whitespace-style
        '(face trailing tabs tab-mark spaces space-mark empty))
  (setq whitespace-space-regexp "\\(\x3000+\\)")
  (setq whitespace-trailing-regexp "\\([ \t\u00A0]+\\)$")
  (setq whitespace-display-mappings
        '((space-mark ?\x3000 [?\u2423])
          (tab-mark   ?\t   [?\u00BB ?\t]))))
