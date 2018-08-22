(use-package projectile
  :straight t
  :after (hydra)
  :config
  (defhydra hydra-projectile (global-map "C-c p"
                                         :exit t)
    "Projectile"
    ("a" projectile-ag)
    ("g" projectile-grep)
    ("d" projectile-dired)
    ("C-g" nil "quit")
    ("q" nil "quit")))
