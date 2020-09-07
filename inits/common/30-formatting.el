(use-package aggressive-indent
  :straight t
  :diminish (aggressive-indent-mode . "ai")
  :hook (emacs-lisp-mode . aggressive-indent-mode))

(setq tab-always-indent 'complete)
