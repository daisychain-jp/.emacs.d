(use-package avy
  :ensure t
  :bind (("M-a" . avy-goto-char-timer)
         :map isearch-mode-map
         ("C-a" . avy-isearch)))

(use-package avy-migemo
  :ensure t
  :after (avy migemo)
  :config
  (avy-migemo-mode 1))
