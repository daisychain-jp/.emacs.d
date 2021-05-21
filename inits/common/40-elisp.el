(use-package elisp-mode
  :commands emacs-lisp-mode
  :delight
  (emacs-lisp-mode " ELisp")
  (lisp-interaction-mode " LispInt")
  :hook (emacs-lisp-mode . (lambda ()
                             (setq-local tab-width 4)
                             (setq-local counsel-dash-docsets '("Emacs Lisp" "Common Lisp"))
                             (eldoc-mode 1))))

(use-package eldoc)
