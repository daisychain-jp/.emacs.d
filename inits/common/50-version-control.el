(custom-set-variables '(vc-follow-symlinks t))

(use-package projectile
  :straight t
  :hook
  (find-file . projectile-mode-switch-dwim)
  :bind-keymap
  ("C-c P" . projectile-command-map)
  :custom
  (projectile-project-search-path env-proj-dir)
  (projectile-switch-project-action 'projectile-dired)
  (projectile-completion-system 'ivy)
  (projectile-track-known-projects-automatically nil)
  (projectile-mode-line-prefix " P")
  (projectile-mode-line-function 'projectile-short-mode-line)
  :config
  (projectile-load-known-projects)
  (defun projectile-mode-switch-dwim ()
    "Intelligently switch on/off projectile mode."
    (when-let* ((bfn (buffer-file-name))
                (vc-registered (buffer-file-name)))
      (setq-local projectile-mode t)))
  (defun projectile-short-mode-line ()
    "Report project name and type in the modeline."
    (let ((project-name (projectile-project-name)))
      (format "%s:%s"
              projectile-mode-line-prefix
              (or project-name "-")))))

(use-package counsel-projectile
  :straight t
  :after hydra
  :config
  (defhydra hydra-projectile (global-map "C-c p"
                                         :exit t)
    "Projectile"
    ("sg" counsel-projectile-grep)
    ("ss" counsel-projectile-ag)
    ("sr" counsel-projectile-rg)
    ("si" counsel-projectile-git-grep)
    ("%" projectile-replace-regexp)
    ("p" counsel-projectile-switch-project)
    ("P" projectile-switch-project)
    ("o" projectile-switch-open-project)
    ("+" projectile-add-known-project)
    ("d" projectile-find-dir)
    ("D" projectile-dired)
    ("t" projectile-run-vterm)
    ("&" projectile-run-shell)
    ("&" projectile-run-async-shell-command-in-root)
    ("S" projectile-save-project-buffers)
    ("f" projectile-find-file)
    ("b" projectile-switch-to-buffer)
    ("r" projectile-recentf)
    ("k" projectile-kill-buffers)
    ("cc" projectile-compile-project)
    ("cx" projectile-run-project)
    ("ct" projectile-test-project)
    ("cC" projectile-configure-project)
    ("&" projectile-run-shell-command-in-root)
    ("v" magit-status)
    ("g" dumb-jump-go)
    ("e" dumb-jump-go-prefer-external)
    ("i" dumb-jump-go-prompt)
    ("l" dumb-jump-quick-look)
    ("C-g" nil "quit")
    ("q" nil "quit")))

(use-package ag
  :straight t)
