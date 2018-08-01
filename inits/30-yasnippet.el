(el-get-bundle yasnippet)
(el-get-bundle yasnippet-snippets)
(el-get-bundle yatemplate)
(el-get-bundle helm-c-yasnippet)

(use-package yasnippet
  :diminish (yas-minor-mode . "ys")
  :config
  (require 'helm-c-yasnippet)
  (bind-keys :map global-map
             ("C-c y n" . yas-new-snippet)
             ("C-c y y" . helm-yas-complete)
             ("C-c y v" . helm-yas-visit-snippet-file))
  (setq yas-snippet-dirs
        `(,env-snippets-dir))
  (setq yas-triggers-in-field t)
  (add-hook 'snippet-mode-hook
            (lambda ()
              (yas-minor-mode 1)))
  (setq helm-yas-space-match-any-greedy t))

(use-package yatemplate
  :config
  (yatemplate-fill-alist)
  (auto-insert-mode 1)
  (add-hook 'find-file-hook 'find-file-hook--yatemplate)
  (add-hook 'after-save-hook 'after-save-hook--yatemplate))

(defun find-file-hook--yatemplate ()
  (when (string-match "emacs.d/templates/" buffer-file-name)
    (let ((mode major-mode))
      (snippet-mode)
      (setq-local yas--guessed-modes (list mode)))))
(defun after-save-hook--yatemplate ()
  "append to auto-insert-alist right after saving template file"
  (when (string-match "emacs.d/templates/" buffer-file-name)
    (yatemplate-fill-alist)))
