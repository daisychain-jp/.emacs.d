(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(enable-remote-dir-locals t)
 '(org-plantuml-jar-path (format "%s/lib/plantuml/plantuml.jar" env-var-dir))
 '(safe-local-variable-values
   (quote
    ((sql-connection-alist
      ("pmonitor"
       (sql-product
        (quote mysql))
       (sql-user "pmonitor")
       (sql-password "eevie7Ah")
       (sql-server "svr01.daisychain.jp")
       (sql-database "pmonitor"))))))
 '(shackle-default-rule (quote (:same t)))
 '(shackle-rules
   (quote
    (((quote undo-tree-visualizer-mode)
      :popup t)
     ((quote sql-interactive-mode)
      :popup t)
     ((quote magit-diff-mode)
      :below t)
     ("*shell*" :same t)
     ("*Help*" :same t)
     ("*Apropos*" :same t)
     ("*el-get packages*" :same t)
     ("*pry*" :same t)
     ("j-*" :regexp t :same t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
