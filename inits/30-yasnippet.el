(use-package yasnippet
  :straight t
  :diminish (yas-minor-mode . "ys")
  :after (hydra)
  :hook
  ((prog-mode org-mode shell-mode) . yas-minor-mode)
  (after-init . yas-reload-all)
  :config
  (setq yas-snippet-dirs `(,env-snippets-dir))
  (setq yas-triggers-in-field t)
  (defhydra hydra-yasnippet (global-map "C-c y"
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
