;; take over path settings from shell
(el-get-bundle exec-path-from-shell)
(exec-path-from-shell-initialize)

;; hideshow
(require 'hideshow)
(diminish 'hs-minor-mode "hs")

;; transpose mark
(el-get-bundle kwrooijen/transpose-mark)
(require 'transpose-mark)

(el-get-bundle adaptive-wrap)
(use-package epa-file
  :config
  (setq epg-gpg-program "/usr/bin/gpg")
  (setq epa-pinentry-mode 'loopback))

(use-package selected
  :straight t
  :hook (after-init . selected-global-mode)
  :diminish selected-minor-mode
  :bind (:map selected-keymap
              ("t" . google-translate-enja-or-jaen)
              ("a" . xah-lookup-amazon)
              ("e" . xah-lookup-eijiro)
              ("g" . xah-lookup-google)
              ("w" . xah-lookup-wikipedia)
              ("j" . open-jtalk-say)
              ("=" . count-words-region)
              ("m" . org-tags-view-archive)
              ("q" . selected-off)))
