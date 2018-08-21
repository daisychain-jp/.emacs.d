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

(use-package comment-dwim-2
  :straight t
  :defer 1
  :bind ("M-;" . comment-dwim-2)
  :config
  (setq comment-dwim-2--inline-comment-behavior 'reindent-comment))

(use-package quickrun
  :straight t)
