;; take over path settings from shell
(use-package exec-path-from-shell
  :straight t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; hideshow
(use-package hideshow
  :diminish "hs")

;; transpose mark
(use-package transpose-mark
  :straight t)

(use-package adaptive-wrap
  :straight t)

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

(use-package expand-region
  :straight t
  :bind (("C-,"   . er/expand-region)
         ("C-M-," . er/contract-region))
  :config
  (push 'er/mark-outside-pairs er/try-expand-list))
