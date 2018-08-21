(use-package shackle
  :straight t
  :config
  (custom-set-variables '(shackle-default-rule '(:same t)))
  (custom-set-variables
   '(shackle-rules
     '(('undo-tree-visualizer-mode :popup t)
       ('sql-interactive-mode :popup t)
       ('magit-diff-mode :below t)
       ("*shell*" :same t)
       ("*Help*" :same t)
       ("*Apropos*" :same t)
       ("*el-get packages*" :same t)
       ("*pry*" :same t)
       ("j-\*" :regexp t :same t))))
  (shackle-mode 1))
