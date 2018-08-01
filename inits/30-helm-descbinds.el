(el-get-bundle helm-descbinds)

(bind-keys :map global-map
           ("C-c b" . helm-descbinds))
(helm-descbinds-mode 1)
