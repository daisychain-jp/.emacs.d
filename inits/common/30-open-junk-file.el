(use-package open-junk-file
  :straight t
  :config
  (setq open-junk-file-format
        (concat env-var-dir "/tmp/junk/%Y/%m/j-%d-%H%M."))
  (remove-hook 'find-file-hook 'find-file-hook--open-junk-file))
