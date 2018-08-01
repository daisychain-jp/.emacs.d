(el-get-bundle visual-regexp)
(el-get-bundle visual-regexp-steroids)

(use-package visual-regexp
  :bind ("M-%" . vr/query-replace)
  )

(use-package visual-regexp-steroids
  :config
  (setq vr/engine 'python)
  )
