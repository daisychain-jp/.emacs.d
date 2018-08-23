(use-package shell
  :commands (shell)
  :config
  (bind-keys :map shell-mode-map
             ("C-j" . comint-send-input))
  (add-hook 'shell-mode-hook
            (lambda ()
              (buffer-face-set 'selecting)
              (setq-local helm-dash-docsets helm-dash-docsets-shell-mode))))

;; term (ansi-term)
(use-package ansi-term
  :commands (ansi-term)
  :config
  (bind-keys :map term-mode-hook
             ("C-j" . term-send-input))
  (add-hook 'term-mode-hook
            (lambda ()
              (buffer-face-set 'selecting))))
