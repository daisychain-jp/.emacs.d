(use-package help-mode
  :delight " HE"
  :bind (:map help-mode-map
              ("C-M-m" . hydra-lazy-control/body))
  :hook (help-mode . (lambda ()
                       (visual-line-mode 1)
                       (adaptive-wrap-prefix-mode 1)
                       (buffer-face-set 'recognizable)))
  :config
  (setq-default default-directory (expand-file-name "inits/" user-emacs-directory)))

(use-package helpful
  :straight t
  :hook (helpful-mode . (lambda ()
                          (visual-line-mode 1)
                          (adaptive-wrap-prefix-mode 1)
                          (buffer-face-set :family "TakaoGothic" :height 678)))
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  (counsel-descbinds-function #'helpful-key))
