;; prog-mode is all programming modes should be derived
(use-package prog-mode
  :config
  (add-hook 'prog-mode-hook
            (lambda ()
              (buffer-face-set 'coding)
              (linum-mode 1)
              (whitespace-mode 1)
              (hl-line-mode 1)
              (hs-minor-mode 1)
              (abbrev-mode 1)
              (smartparens-mode 1)
              (smart-newline-mode 1)
              (yas-minor-mode 1)
              (yas-reload-all)
              (setq-local truncate-lines t))))
