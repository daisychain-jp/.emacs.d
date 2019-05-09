(use-package sequential-command
  :straight t
  :after (org))

;; sequential-command-config is at EmacsWiki
(el-get-bundle sequential-command-config)
(use-package sequential-command-config
  :after (sequential-command)
  :config
  (sequential-command-setup-keys)
  ;; org-mode
  (define-sequential-command org-seq-home
    org-beginning-of-line
    org-previous-visible-heading
    beginning-of-buffer
    seq-return)
  (define-sequential-command org-seq-end
    org-end-of-line
    outline-next-heading
    end-of-buffer
    seq-return)
  (bind-keys :map org-mode-map
             ("C-a" . org-seq-home)
             ("C-e" . org-seq-end)))
