(use-package projectile
  :straight t
  :after (hydra counsel)
  :init
  (projectile-load-known-projects)
  :custom
  (projectile-completion-system 'ivy)
  :config
  (defhydra hydra-projectile (global-map "C-c p"
                                         :exit t)
    "Projectile"
    ("sg" counsel-projectile-grep)
    ("ss" counsel-projectile-ag)
    ("sr" counsel-projectile-rg)
    ("si" counsel-projectile-git-grep)
    ("p" counsel-projectile-switch-project)
    ("+" projectile-add-known-project)
    ("d" projectile-find-dir)
    ("D" projectile-dired)
    ("f" counsel-projectile-find-file-dwim)
    ("b" counsel-projectile-switch-to-buffer)
    ("r" projectile-recentf)
    ("k" projectile-kill-buffers)
    ("C-g" nil "quit")
    ("q" nil "quit")))

(use-package counsel-projectile
  :straight t)

(use-package ag
  :straight t)
