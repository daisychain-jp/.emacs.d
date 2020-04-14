(use-package dired
  :delight " DD"
  :bind (:map dired-mode-map
              ("(" . dired-hide-details-mode)
              (")" . dired-hide-details-mode)
              ("o" . dired-omit-mode)
              ("r" . wdired-change-to-wdired-mode)
              ("C-o" . dired-open)
              ("C-|" . dired-snatch))
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
  (interactive "p")
  (let ((files (dired-get-marked-files nil nil)))
    (case arg
      (16 (mapc (lambda (file) (open-file-external file)) files))
      (4 (mapc (lambda (file) (find-file file)) files))
      (t (mapc (lambda (file) (open-file file)) files)))))

(defun dired-snatch ()
  "Snatch a video file.
Namely the file is replaced with the compressed as if it's an original.

If directory is chosen, all files in the directory are compressed."
  (interactive)
  (lexical-let* ((files (dired-get-marked-files nil nil))
                 (output-buffer (generate-new-buffer "snatch"))
                 (proc (progn
                         (start-process-shell-command "snatch" output-buffer (format "snatch %s" (mapconcat 'identity files " ")))
                         (get-buffer-process output-buffer))))
    (if (process-live-p proc)
        (set-process-sentinel proc (lambda (process signal)
                                     (when (memq (process-status process) '(exit signal))
                                       (alert "snatched" :buffer output-buffer :mode 'dired-mode :severity 'normal)
                                       (kill-buffer output-buffer))))
      (message "No process running."))))
