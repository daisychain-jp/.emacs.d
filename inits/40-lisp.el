(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq tab-width 4)
            (setq-local helm-dash-docsets helm-dash-docsets-emacs-lisp-mode)
            (eldoc-mode 0)))
(delight 'emacs-lisp-mode       " EL")
(delight 'lisp-interaction-mode " LI")
(diminish 'eldoc-mode)