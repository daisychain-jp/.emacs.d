(use-package which-key
  :straight t
  :diminish which-key-mode
  :custom
  (which-key-use-C-h-commands t)
  (which-key-max-description-length 43)
  (which-key-idle-delay 0.7)
  (which-key-separator " : ")
  (which-key-side-window-max-height 0.5)
  :config
  (which-key-mode 1)
  (which-key-setup-side-window-bottom))
