(delight 'snippet-mode " SN")
(use-package yasnippet
  :straight t
  :diminish (yas-minor-mode . "ys")
  :hook
  ((prog-mode org-mode shell-mode) . yas-minor-mode-on)
  :custom
  (yas-triggers-in-field t)
  :config
  ;; work around
  (yas-reload-all))

(use-package yasnippet-snippets
  :straight t
  :after yasnippet)
