(use-package help-mode
  :delight " HE"
  :bind (:map help-mode-map
              ("C-M-m" . hydra-lazy-control/body))
  :init
  (defface help-buffer `((t . (:font "fontset-default"
                                     :height ,(my-adjust-font-size 835))))
    "Default face for help mode."
    :group 'help)
  :hook (help-mode . (lambda ()
                       (visual-line-mode 1)
                       (adaptive-wrap-prefix-mode 1)
                       (buffer-face-set 'help-buffer)))
  :config
  (setq-default default-directory (expand-file-name "inits/" user-emacs-directory)))

(use-package helpful
  :straight t
  :hook (helpful-mode . (lambda ()
                          (visual-line-mode 1)
                          (adaptive-wrap-prefix-mode 1)
                          (buffer-face-set 'help-buffer)))
  :bind (:map helpful-mode-map
              ("u" . helpful-push-symbol-onto-kill-ring))
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  (counsel-describe-symbol-function #'helpful-symbol)
  (counsel-descbinds-function #'helpful-key))

(defun helpful-push-symbol-onto-kill-ring ()
  "Push symbol name to kill ring."
  (interactive)
  (let ((symbol (symbol-name helpful--sym)))
    (kill-new (symbol-name helpful--sym))
    (message "KILL: %s" symbol)))
