(use-package projectile
  :straight t
  :after (hydra)
  :init
  (projectile-load-known-projects)
  :custom
  (projectile-completion-system 'ivy)
  :config
  (defhydra hydra-projectile (global-map "C-c p"
                                         :exit t)
    "Projectile"
    ("sa" counsel-projectile-ag)
    ("ss" counsel-projectile-git-grep)
    ("p" projectile-switch-project)
    ("o" projectile-switch-open-project)
    ("a" projectile-add-known-project)
    ("d" projectile-find-dir)
    ("D" projectile-dired)
    ("f" projectile-find-file)
    ("b" projectile-switch-to-buffer)
    ("r" projectile-recentf)
    ("k" projectile-kill-buffers)
    ("C-g" nil "quit")
    ("q" nil "quit")))

(use-package counsel-projectile
  :straight t)

(use-package ag
  :straight t)
