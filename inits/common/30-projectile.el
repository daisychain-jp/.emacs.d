(use-package projectile
  :straight t
  :after (hydra counsel dumb-jump)
  :init
  (projectile-load-known-projects)
  :hook
  (prog-mode . (lambda ()
                 (when-let* ((bfn (buffer-file-name))
                             (vc-registered (buffer-file-name)))
                   (projectile-mode 1))))
  :custom
  (projectile-completion-system 'ivy)
  (projectile-mode-line-prefix " P")
  (projectile-mode-line-function 'projectile-short-mode-line)
  :config
  (defun projectile-short-mode-line ()
    "Report project name and type in the modeline."
    (let ((project-name (projectile-project-name)))
      (format "%s:%s"
              projectile-mode-line-prefix
              (or project-name "-"))))
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
    ("S" projectile-save-project-buffers)
    ("f" counsel-projectile-find-file-dwim)
    ("b" counsel-projectile-switch-to-buffer)
    ("r" projectile-recentf)
    ("k" projectile-kill-buffers)
    ("g" dumb-jump-go)
    ("e" dumb-jump-go-prefer-external)
    ("i" dumb-jump-go-prompt)
    ("l" dumb-jump-quick-look)
    ("C-g" nil "quit")
    ("q" nil "quit")))

(use-package counsel-projectile
  :straight t)

(use-package ag
  :straight t)
