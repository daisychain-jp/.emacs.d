;; https://github.com/syohex/emacs-helm-ag
(el-get-bundle helm-ag)
(use-package helm-ag
  :bind (:map helm-ag-mode-map
              ("C-/" . helm-ag-pop-stack))
  :config
  ;; use current symbol as query
  (setq helm-ag-insert-at-point 'symbol)
  )

(defun helm-do-ag-current-dir ()
  "Search in current directory with ag."
  (interactive)
  (helm-do-ag default-directory))
(defun helm-do-ag-home ()
  "Search in home directory with ag."
  (interactive)
  (helm-do-ag "~/"))
(defun helm-do-ag-org ()
  "Search org files with ag."
  (interactive)
  (helm-do-ag env-doc-dir))
