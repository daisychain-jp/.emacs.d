(el-get-bundle open-junk-file)

(use-package open-junk-file
  :bind ("s-j" . open-junk-file)
  :config
  (setq open-junk-file-format
        (concat env-emacs-data-dir "/junk/%Y/%m/j-%d-%H%M.")))
