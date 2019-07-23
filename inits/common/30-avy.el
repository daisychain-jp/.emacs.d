(use-package avy
  :straight t
  :bind (("M-a" . avy-goto-char-timer)
         :map isearch-mode-map
         ("C-a" . avy-isearch)))
