(use-package helm-ag
  :straight t
  :bind (:map helm-ag-mode-map
              ("C-/" . helm-ag-pop-stack))
  :custom
  ;; use current symbol as query
  (helm-ag-insert-at-point 'symbol)
  (helm-ag-command-option "--hidden"))

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
