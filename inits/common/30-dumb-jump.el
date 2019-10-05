(use-package dumb-jump
  :straight t
  :after (hydra)
  :init
  (dumb-jump-mode)
  :custom
  (dumb-jump-selector 'ivy)
  :config
  (defhydra hydra-projectile (global-map "C-c d"
                                         :exit t)
    "Dumb-Jump"
    ("g" dumb-jump-go)
    ("e" dumb-jump-go-prefer-external)
    ("i" dumb-jump-go-prompt)
    ("l" dumb-jump-quick-look)
    ("b" dumb-jump-back)
    ("C-g" nil "quit")
    ("q" nil "quit")))
