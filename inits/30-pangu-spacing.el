(el-get-bundle pangu-spacing)
(use-package pangu-spacing
  :diminish "Ps"
  :config
  (setq pangu-spacing-chinese-before-english-regexp
        (rx (group-n 1 (category japanese))
            (group-n 2 (in "a-zA-Z0-9"))))
  (setq pangu-spacing-chinese-after-english-regexp
        (rx (group-n 1 (in "a-zA-Z0-9"))
            (group-n 2 (category japanese))))
  (setq pangu-spacing-real-insert-separtor t))
