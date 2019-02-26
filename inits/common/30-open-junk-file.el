(use-package open-junk-file
  :straight t
  :bind ("s-j" . open-junk-file)
  :config
  (setq open-junk-file-format
        (concat env-var-dir "/lib/junk/%Y/%m/j-%d-%H%M.")))
