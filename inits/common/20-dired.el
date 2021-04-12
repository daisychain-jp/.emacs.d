(use-package dired
  :bind (:map dired-mode-map
              ("(" . dired-hide-details-mode)
              (")" . dired-hide-details-mode)
              ("E" . dired-create-empty-file)
              ("Y" . dired-do-relsymlink)
              ("o" . dired-omit-mode)
              ("r" . wdired-change-to-wdired-mode)
              ("x" . (lambda (arg) (interactive "P")
                       (let ((delete-by-moving-to-trash
                              (if (equal arg '(4))
                                  nil t)))
                         (dired-do-flagged-delete))))
              ("C-o" . nil)
              ("o" . dired-open)
              ("C-c C-o" . dired-open))
  :custom
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  (dired-listing-switches "-ahgG --time-style=iso --group-directories-first")
  (dired-dwim-target 'dired-dwim-target-recent)
  (dired-hide-details-hide-information-lines nil)
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  (setq dired-isearch-filenames t)
  (add-hook 'dired-mode-hook
            (lambda ()
              (buffer-face-set 'selecting)
              (dired-hide-details-mode 1)
              (setq-local truncate-lines t)
              (whitespace-mode 1))))

(use-package dired-rsync
  :straight t
  :bind
  (:map dired-mode-map
        ("C-c C-r" . dired-rsync))
  :custom
  (dired-rsync-options "-auz --info=progress2"))

(use-package dired-single
  :straight t
  :bind (:map dired-mode-map
              ("C-j" . dired-single-buffer)
              ("^" . dired-single-up-directory)))

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

(use-package dired-hide-dotfiles
  :straight t
  :bind (:map dired-mode-map
              ("." . dired-hide-dotfiles-mode))
  :config
  (dired-hide-dotfiles-mode))

(use-package dired-du
  :straight t
  :diminish ((dired-du-mode . "du"))
  :bind (:map dired-mode-map
              ("C-M-S-u" . dired-du-mode))
  :custom
  (dired-du-size-format 'comma))

(defun dired-open (&optional arg)
  "Open file in Dired.

File is either on current line or marked targets.
Optional argument `ARG' is passed to `my/view-file'."
  (interactive "P")
  (let ((files (dired-get-marked-files nil nil)))
    (mapc (lambda (file)
            (if (equal arg '(4))
                (find-file file)
              (my/view-file file arg)))
          files)))
