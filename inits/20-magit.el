(use-package magit
  :straight t
  :bind (:map magit-log-mode-map
              ("C-j" . magit-visit-thing))
  :config
  (add-hook 'magit-status-mode-hook
            (lambda ()
              (buffer-face-set 'selecting))))

(use-package orgit
  :disabled
  :straight t
  :after (magit org))
