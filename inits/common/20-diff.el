(use-package ediff
  :commands ediff-files
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-default)
  (ediff-split-window-function 'split-window-horizontally))
