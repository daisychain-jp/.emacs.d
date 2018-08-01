(el-get-bundle expand-region)

(use-package expand-region
  :bind (("C-,"   . er/expand-region)
         ("C-M-," . er/contract-region))
  :config
  (push 'er/mark-outside-pairs er/try-expand-list)
  )
