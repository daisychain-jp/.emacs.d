;; take over path settings from shell
(use-package exec-path-from-shell
  :straight t
  :config
  (when (memq window-system '(mac))
    (exec-path-from-shell-initialize)))

;; hideshow
(use-package hideshow
  :diminish "hs")

(use-package proced
  :no-require t
  :custom
  (proced-auto-update-flag t)
  (proced-auto-update-interval 3))

(use-package switch-buffer-functions
  :straight t
  :config
  (add-hook 'switch-buffer-functions
            (lambda (prev cur)
              (when (string= (buffer-name cur) "*Help*")
                (setq-local truncate-lines nil)
                (setq-local hlc/enter-func #'push-button)
                (hydra-lazy-control/body)
                (adaptive-wrap-prefix-mode 1)))))

;; transpose mark
(use-package transpose-mark
  :straight t)

(use-package adaptive-wrap
  :straight t)

(use-package epa-file
  :custom
  (epg-gpg-program "/usr/bin/gpg")
  (epa-pinentry-mode 'loopback))

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
              ("C-m" . org-tags-view-archive)
              ("C-g" . selected-off)))

(use-package expand-region
  :straight t
  :bind (("C-,"   . er/expand-region)
         ("C-M-," . er/contract-region))
  :config
  (push 'er/mark-outside-pairs er/try-expand-list))

(use-package ts
  :straight (ts :type git
                :host github
                :repo "alphapapa/ts.el"))

(use-package synosaurus
  :straight t
  :custom
  (synosaurus-choose-method 'ivy))
