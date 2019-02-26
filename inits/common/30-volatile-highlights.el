;; highlights yank / undo region
(use-package volatile-highlights
  :straight t
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode 1))
