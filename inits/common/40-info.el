(use-package Info
  :defer t
  :bind (:map Info-mode-map
              ("C-j" . Info-follow-nearest-node)
              ("C-c C-o" . Info-follow-nearest-node)
              ("a"   . info-apropos))
  :init
  (defface info-buffer `((t . (:font "fontset-default"
                                     :height ,(my-adjust-font-size 835))))
    "Default face for info mode."
    :group 'info)
  :hook (Info-mode . (lambda ()
                       (buffer-face-set 'info-buffer)
                       (visual-line-mode 1)
                       (adaptive-wrap-prefix-mode 1)))
  :custom
  (Info-use-header-line nil))

(use-package info-look
  :defer t
  :init
  (autoload 'info-lookup-add-help "info-look"))
