(use-package sequential-command
  :straight t
  :config
  ;; for org-mode
  (define-sequential-command org-seq-home
    org-beginning-of-line
    outline-previous-heading
    beginning-of-buffer
    seq-return)
  (define-sequential-command org-seq-end
    org-end-of-line
    outline-next-heading
    end-of-buffer
    seq-return)
  ;; for emacs-lisp-mode
  (define-sequential-command emacs-lisp-seq-home
    sp-beginning-of-sexp
    sp-beginning-of-previous-sexp
    beginning-of-defun
    beginning-of-buffer
    seq-return)
  (define-sequential-command emacs-lisp-seq-end
    sp-end-of-sexp
    sp-beginning-of-next-sexp
    end-of-defun
    end-of-buffer
    seq-return)
  (bind-keys :map emacs-lisp-mode-map
             ("A-a" . emacs-lisp-seq-home)
             ("A-e" . emacs-lisp-seq-end))
  ;; for ruby-mode
  (with-eval-after-load 'ruby-mode
    (define-sequential-command ruby-seq-home
      ruby-beginning-of-block
      ruby-beginning-of-defun
      beginning-of-buffer
      seq-return)
    (define-sequential-command ruby-seq-end
      ruby-end-of-block
      ;; ruby-end-of-defun ; FIXME: cause error
      end-of-buffer
      seq-return)
    (bind-keys :map ruby-mode-map
               ("A-a" . ruby-seq-home)
               ("A-e" . ruby-seq-end))))

;; sequential-command-config is at EmacsWiki
(el-get-bundle sequential-command-config)
(use-package sequential-command-config
  :after (sequential-command)
  :config
  (sequential-command-setup-keys))
