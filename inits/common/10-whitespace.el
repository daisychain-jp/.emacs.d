(use-package whitespace
  :diminish ((global-whitespace-mode . "Ws")
             (whitespace-mode . "ws"))
  :config
  (global-whitespace-mode -1)
  (setq whitespace-style
        '(face trailing tabs tab-mark spaces space-mark empty))
  (setq whitespace-space-regexp "\\(\x3000+\\)")
  (setq whitespace-display-mappings
        '((space-mark ?\x3000 [?\□])
          (tab-mark   ?\t   [?\xBB ?\t]))))

;; whitespace
(set-face-foreground 'whitespace-space "LightSlateGray")
(set-face-background 'whitespace-space "DarkSlateGray")
(set-face-foreground 'whitespace-tab "LightSlateGray")
(set-face-background 'whitespace-tab "DarkSlateGray")
