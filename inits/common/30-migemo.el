(use-package migemo
  :straight t
  :ensure-system-package cmigemo
  :custom
  (migemo-command "cmigemo")
  (migemo-options '("-q" "--emacs"))
  (migemo-user-dictionary nil)
  (migemo-coding-system 'utf-8-unix)
  (migemo-regex-dictionary nil)
  :config
  (cond
   ((string= system-type "gnu/linux")
    (setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict"))
   ((string= system-type "darwin")
    (setq migemo-dictionary "/usr/local/Cellar/cmigemo/20110227/share/migemo/utf-8/migemo-dict")))
  (load-library "migemo")
  (migemo-init))

(use-package ivy-migemo
  :straight (:host github
                   :repo "ROCKTAKEY/ivy-migemo"
                   :branch "master"))
