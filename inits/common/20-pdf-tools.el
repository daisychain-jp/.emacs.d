(use-package pdf-tools
  :straight t
  :delight
  (pdf-view-mode " PV")
  :mode ("\\.pdf$" . pdf-view-mode)
  :init
  (pdf-tools-install)
  :config
  (add-hook 'pdf-view-mode-hook
            (lambda ()
              (pdf-view-midnight-minor-mode t)))
  (setq pdf-info-epdfinfo-program env-epdfinfo-prog))
