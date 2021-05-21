;; take over path settings from shell
(use-package exec-path-from-shell
  :straight t
  :config
  (when (memq window-system '(mac))
    (exec-path-from-shell-initialize)))

;; hideshow
(use-package hideshow
  :diminish "hs"
  :bind (("C-c TAB" . hs-toggle-hiding)))

(use-package proced
  :no-require t
  :custom
  (proced-auto-update-flag t)
  (proced-auto-update-interval 3))

;; transpose mark
(use-package transpose-mark
  :straight t)

(diminish 'visual-line-mode "wr")

(use-package adaptive-wrap
  :straight t)

(use-package epg
  :custom
  (epg-pinentry-mode 'loopback))

(use-package selected
  :straight t
  :hook (after-init . selected-global-mode)
  :diminish selected-minor-mode
  :bind (:map selected-keymap
              ("C-l t" . google-translate-enen-or-jaen)
              ("C-l g" . lookup-google)
              ("C-l w" . lookup-wikipedia-ja)
              ("C-l W" . lookup-wikipedia-en)
              ("C-l a" . lookup-amazon)
              ("C-l y" . lookup-youtube)
              ("C-l s" . synosaurus-lookup)
              ("C-l S" . synosaurus-choose-and-replace)
              ("C-l d" . lookup-duckduckgo)
              ("C-l e" . lookup-eijiro)
              ("C-l l" . lookup-weblio)
              ("C-'" . utl-say)
              ("C-=" . count-words-region)
              ("C-g" . selected-off)))

(use-package expand-region
  :straight t
  :bind (("C-,"   . er/expand-region)
         ("C-M-," . er/contract-region))
  :config
  (push 'er/mark-outside-pairs er/try-expand-list))

(use-package ts
  :straight t)

(use-package synosaurus
  :straight t
  :custom
  (synosaurus-choose-method 'ivy))

(use-package try
  :straight t)

(use-package free-keys
  :straight t)

(use-package company
  :straight t
  :hook ((prog-mode) . company-mode)
  :bind (:map company-active-map
              ("C-j" . company-complete-selection)
              ("C-i" . counsel-company)
              ("C-h" . backward-delete-char-untabify)
              ("C-M-h" . company-show-doc-buffer)
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)))
