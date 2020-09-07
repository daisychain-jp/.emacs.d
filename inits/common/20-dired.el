(use-package dired
  :delight " DR"
  :bind (:map dired-mode-map
              ("(" . dired-hide-details-mode)
              (")" . dired-hide-details-mode)
              ("E" . dired-create-empty-file)
              ("o" . dired-omit-mode)
              ("r" . wdired-change-to-wdired-mode)
              ("C-o" . dired-open))
  :custom
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  (dired-listing-switches "-ahgG --time-style=iso")
  (dired-dwim-target t)
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

(use-package dired-du
  :straight t
  :diminish ((dired-du-mode . "du"))
  :bind (:map dired-mode-map
              ("C-M-S-u" . dired-du-mode))
  :custom
  (dired-du-size-format 'comma))

(unbind-key "C-x f")
(use-package find-dired
  :bind (:map global-map
              ("C-x f f" . find-dired)
              ("C-x f F" . find-lisp-find-dired)
              ("C-x f g" . find-grep-dired)
              ("C-x f n" . find-name-dired)
              ("C-x f d" . find-lisp-find-dired-subdirectories))
  :custom
  (find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld")))

(defun dired-open (&optional arg)
  "Open file in Dired.

File is either on current line or marked targets.
Optional argument `ARG' is passed to `open-file'."
  (interactive "p")
  (let ((files (dired-get-marked-files nil nil)))
    (case arg
      (16 (mapc (lambda (file) (open-file-external file)) files))
      (4 (mapc (lambda (file) (find-file file)) files))
      (t (mapc (lambda (file) (open-file file)) files)))))
