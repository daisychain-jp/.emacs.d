(use-package dired
  :delight " DD"
  :bind (:map dired-mode-map
              ("(" . dired-hide-details-mode)
              (")" . dired-hide-details-mode)
              ("o" . dired-omit-mode)
              ("r" . wdired-change-to-wdired-mode)
              ("C-j" . dired-find-alternate-file)
              ("C-o" . dired-open))
  :custom
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  (dired-dwim-target t)
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  (setq dired-isearch-filenames t)
  (add-hook 'dired-mode-hook
            (lambda ()
              (buffer-face-set 'selecting)
              (dired-hide-details-mode 1)
              (setq-local truncate-lines t)
              (whitespace-mode 1))))

(use-package dired-subtree
  :straight t
  :bind (:map dired-mode-map
              ("TAB" . dired-subtree-cycle)))

(use-package dired-narrow
  :straight t
  :bind
  (:map dired-mode-map
        ("z" . dired-narrow))
  (:map dired-narrow-map
        ("C-j" . exit-minibuffer)))

(use-package dired-ranger
  :straight t
  :bind
  (:map dired-mode-map
        ("c" . dired-ranger-copy)
        ("P" . dired-ranger-paste)
        ("V" . dired-ranger-move)))

(defun dired-open (&optional arg)
  "Open file in Dired.

File is either on current line or marked targets.
Optional argument `ARG' is passed to `open-file'."
  (interactive "P")
  (let ((files (dired-get-marked-files nil nil)))
    (mapc (lambda (file) (open-file file arg)) files)))
