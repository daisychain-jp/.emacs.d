;; prog-mode is all programming modes should be derived
(use-package prog-mode
  :init
  (defface prog-buffer `((t . (:font "fontset-default"
                                     :height ,(my-adjust-font-size 570))))
    "Default face for coding.")
  :config
  (add-hook 'prog-mode-hook
            (lambda ()
              (buffer-face-set 'prog-buffer)
              (whitespace-mode 1)
              (hl-line-mode 1)
              (hs-minor-mode 1)
              (abbrev-mode 1)
              (flyspell-prog-mode)
              (display-fill-column-indicator-mode 1)
              (setq-local truncate-lines t))))

(use-package comment-dwim-2
  :straight t
  :defer 1
  :bind ("M-;" . comment-dwim-2)
  :config
  (setf comment-dwim-2--inline-comment-behavior 'reindent-comment
        cd2/region-command 'cd2/comment-or-uncomment-region))

(use-package quickrun
  :straight t)
