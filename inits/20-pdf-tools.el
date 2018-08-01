(el-get-bundle pdf-tools)
(use-package pdf-tools
  :mode ("\\.pdf$" . pdf-view-mode)
  :init
  (pdf-tools-install)
  :config
  (add-hook 'pdf-view-mode-hook
            (lambda ()
              (pdf-view-midnight-minor-mode t)))
  (setq pdf-info-epdfinfo-program env-epdfinfo-prog))
