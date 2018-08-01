(el-get-bundle anzu)

(use-package anzu
    :bind (("C-%" . anzu-query-replace)
           ("C-M-%" . anzu-query-replace-regexp))
    :config
    (global-anzu-mode t)
    ;; for comfortable migemo search
    (setq anzu-minimum-input-length 3)
    (setq anzu-search-threshold 1000)
    (setq anzu-use-migemo t)
    ;; set mode-line face
    (set-face-attribute 'anzu-mode-line nil
                        :foreground "gray95"
                        :background "gray10"
                        :weight 'bold))
