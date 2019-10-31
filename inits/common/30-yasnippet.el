(delight 'snippet-mode " SN")
(use-package yasnippet
  :straight t
  :diminish (yas-minor-mode . "ys")
  :after (hydra)
  :hook
  ((prog-mode org-mode shell-mode) . yas-minor-mode)
  :custom
  (yas-triggers-in-field t)
  :config
  (defhydra hydra-yasnippet (yas-minor-mode-map "C-c y"
                                                :exit t)
    "Yasnippet"
    ("n" yas-new-snippet)
    ("y" yas-insert-snippet)
    ("v" yas-visit-snippet-file)
    ("C-g" nil "quit")
    ("q" nil "quit")))

(use-package yasnippet-snippets
  :straight t
  :after (yasnippet))
