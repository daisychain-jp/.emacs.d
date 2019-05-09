(delight 'lisp-interaction-mode " LI")
(diminish 'eldoc-mode "ed")

(delight 'emacs-lisp-mode " EL")
(use-package lisp-mode
  :commands emacs-lisp-mode
  :ensure nil
  :hook (emacs-lisp-mode . (lambda ()
                             (setq-local tab-width 4)
                             (setq-local helm-dash-docsets helm-dash-docsets-emacs-lisp-mode)
                             (eldoc-mode 1)))
  :after (sequential-command-config)
  :config
  ;; emacs-lisp-mode
  (define-sequential-command emacs-lisp-seq-home
    beginning-of-line
    sp-beginning-of-sexp
    sp-beginning-of-previous-sexp
    beginning-of-defun
    beginning-of-buffer
    seq-return)
  (define-sequential-command emacs-lisp-seq-end
    end-of-line
    sp-end-of-sexp
    sp-beginning-of-next-sexp
    end-of-defun
    end-of-buffer
    seq-return)
  (bind-keys :map emacs-lisp-mode-map
             ("C-a" . emacs-lisp-seq-home)
             ("C-e" . emacs-lisp-seq-end))
  )
