(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq tab-width 4)
            (flycheck-mode 1)
            (setq-local helm-dash-docsets helm-dash-docsets-emacs-lisp-mode)))
