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
              ("C-t" . google-translate-enja-or-jaen)
              ("C-l g" . lookup-google)
              ("C-l w" . lookup-wikipedia-ja)
              ("C-l W" . lookup-wikipedia-en)
              ("C-l a" . lookup-amazon)
              ("C-l y" . lookup-youtube)
              ("C-l d" . lookup-duckduckgo)
              ("C-l e" . lookup-eijiro)
              ("C-'" . utl-say)
              ("C-=" . count-words-region)
              ("C-m" . org-tags-view-archive)
              ("C-g" . selected-off)))

(use-package expand-region
  :straight t
  :bind (("C-,"   . er/expand-region)
         ("C-M-," . er/contract-region))
  :config
  (push 'er/mark-outside-pairs er/try-expand-list))
