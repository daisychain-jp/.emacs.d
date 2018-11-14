(delight 'emacs-lisp-mode       " EL")
(delight 'lisp-interaction-mode " LI")
(diminish 'eldoc-mode "ed")
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq-local tab-width 4)
            (setq-local helm-dash-docsets helm-dash-docsets-emacs-lisp-mode)
            (eldoc-mode 1)))
