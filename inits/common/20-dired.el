(use-package dired
  :delight " DD"
  :bind (:map dired-mode-map
              ("(" . dired-hide-details-mode)
              (")" . dired-hide-details-mode)
              ("o" . dired-omit-mode)
              ("TAB" . dired-subtree-cycle)
              ("r" . wdired-change-to-wdired-mode)
              ("C-j" . dired-find-alternate-file)
              ("C-o" . dired-open)
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
              (dired-hide-details-mode 1)
              (setq-local truncate-lines t)
              (whitespace-mode 1))))

(defun dired-open (&optional arg)
  "Open file in Dired.

File is either on current line or marked targets.
Optional argument `ARG' is passed to `open-file'."
  (interactive "P")
  (let ((files (dired-get-marked-files nil nil)))
    (mapc (lambda (file) (open-file file arg)) files)))
