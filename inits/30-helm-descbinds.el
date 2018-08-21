(use-package helm-descbinds
  :straight t
  :config
  (helm-descbinds-mode 1)
  (bind-keys :map global-map
             ("C-c b" . helm-descbinds)))
