(delight 'lisp-interaction-mode " LI")
(diminish 'eldoc-mode "ed")

(delight 'emacs-lisp-mode " EL")
(use-package lisp-mode
  :commands emacs-lisp-mode
  :ensure nil
  :hook (emacs-lisp-mode . (lambda ()
                             (setq-local tab-width 4)
                             (setq-local counsel-dash-docsets '("Emacs Lisp" "Common Lisp"))
                             (eldoc-mode 1)
                             (company-mode 1))))
