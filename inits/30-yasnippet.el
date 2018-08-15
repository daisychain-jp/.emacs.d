(use-package yasnippet
  :straight t
  :diminish (yas-minor-mode . "ys")
  :config
  (setq yas-snippet-dirs
        `(,env-snippets-dir))
  (setq yas-triggers-in-field t)
  (add-hook 'snippet-mode-hook
            (lambda ()
              (yas-minor-mode 1))))

(use-package yasnippet-snippets
  :straight t
  :after (yasnippet))

(use-package helm-c-yasnippet
  :straight t
  :after (yasnippet hydra)
  :config
  (setq helm-yas-space-match-any-greedy t)
  (defhydra hydra-yasnippet (global-map "C-c y"
                                        :exit t)
    "Yasnippet"
    ("n" yas-new-snippet)
    ("y" helm-yas-complete)
    ("v" helm-yas-visit-snippet-file)
    ("C-g" nil "quit")
    ("q" nil "quit")))
