(use-package shackle
  :straight t
  :after (flycheck)
  :custom
  (shackle-rules
   `(('undo-tree-visualizer-mode :popup t)
     ('sql-interactive-mode :popup t)
     ('magit-diff-mode :below t)
     (,flycheck-error-message-buffer :below t)
     ("*shell*" :same t)
     ("*Help*" :same t)
     ("*Apropos*" :same t)
     ("*el-get packages*" :same t)
     ("*pry*" :same t)
     ("j-\*" :regexp t :same t)))
  :config
  (custom-set-variables '(shackle-default-rule '(:same t)))
  (shackle-mode 1))
