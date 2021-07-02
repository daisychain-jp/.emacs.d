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

(use-package expand-region
  :straight t
  :bind (("C-,"   . er/expand-region)
         ("C-M-," . er/contract-region))
  :config
  (push 'er/mark-outside-pairs er/try-expand-list))

(use-package ts
  :straight t)

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
