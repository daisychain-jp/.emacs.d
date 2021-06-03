(use-package lsp-mode
  :straight t
  :commands (lsp lsp-deferred)
  :hook ((python-mode) . lsp-deferred))

(use-package lsp-ivy
  :straight t
  :after lsp
  :hook (lsp-mode . lsp-ivy-mode))
