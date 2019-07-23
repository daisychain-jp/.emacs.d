(use-package projectile
  :straight t
  :after (hydra)
  :config
  (setq projectile-completion-system 'ivy)
  (defhydra hydra-projectile (global-map "C-c p"
                                         :exit t)
    "Projectile"
    ("s a" counsel-projectile-ag)
    ("s s" counsel-projectile-git-grep)
    ("d" projectile-dired)
    ("f" projectile-find-file)
    ("C-g" nil "quit")
    ("q" nil "quit")))

(use-package counsel-projectile
  :straight t)

(use-package ag
  :straight t)
