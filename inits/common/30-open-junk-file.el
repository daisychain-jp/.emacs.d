(use-package open-junk-file
  :straight t
  :custom
  (open-junk-file-format
   (concat env-var-dir "/tmp/junk/j-%Y%m%d-%H%M."))
  :config
  (remove-hook 'find-file-hook 'find-file-hook--open-junk-file))
