(use-package smart-newline
  :straight t
  :diminish "sn"
  :config
  (bind-keys :map smart-newline-mode-hook
             ("C-j" . smart-newline))
  (defadvice smart-newline (around C-u activate)
    "C-u to disable smart-newline"
    (if (not current-prefix-arg)
        ad-do-it
      (let (current-prefix-arg)
        (let (smart-newline-mode)
          (call-interactively (key-binding (kbd "C-m"))))))))
