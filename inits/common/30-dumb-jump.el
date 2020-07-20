(use-package dumb-jump
  :straight t
  :after (hydra)
  :init
  (dumb-jump-mode)
  :custom
  (dumb-jump-selector 'ivy))
