(use-package magit
  :straight t
  :delight
  (magit-status-mode " MG")
  (magit-revision-mode " MG")
  :bind (:map magit-status-mode-map
              ("C-j" . magit-visit-thing)
              :map magit-log-mode-map
              ("C-j" . magit-visit-thing))
  :config
  (add-hook 'magit-status-mode-hook
            (lambda ()
              (buffer-face-set 'selecting))))

(use-package orgit
  :straight t
  :after (magit org)
  :custom
  (orgit-remote "github"))

(use-package git-modes
  :straight t)
