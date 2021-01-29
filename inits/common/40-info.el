(use-package Info
  :defer t
  :bind (:map Info-mode-map
              ("C-j" . Info-follow-nearest-node)
              ("C-c C-o" . Info-follow-nearest-node)
              ("a"   . info-apropos))
  :hook (Info-mode . (lambda ()
                       (buffer-face-set :family "TakaoGothic" :height 678)
                       (visual-line-mode 1)
                       (adaptive-wrap-prefix-mode 1)))
  :custom
  (Info-use-header-line nil))
