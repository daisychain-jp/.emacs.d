(use-package dired
  :delight " DD"
  :bind (:map dired-mode-map
              ("(" . dired-hide-details-mode)
              (")" . dired-hide-details-mode)
              ("o" . dired-omit-mode)
              ("TAB" . dired-subtree-cycle)
              ("r" . wdired-change-to-wdired-mode)
              ("C-j" . dired-find-alternate-file)
              ("a" . dired-find-file))
  :custom
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  (setq dired-isearch-filenames t)
  (add-hook 'dired-mode-hook
            (lambda ()
              (buffer-face-set 'selecting)
              (dired-hide-details-mode t)
              (setq truncate-lines t))))
