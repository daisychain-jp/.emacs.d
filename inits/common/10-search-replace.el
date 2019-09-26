;; search

(bind-keys :map isearch-mode-map
           ("C-j" . isearch-exit)
           ("M-w" . copy-isearch-region)
           ("M-d" . kill-isearch-region))
(diminish 'isearch-mode "is")

(defun copy-isearch-region ()
  (interactive)
  (kill-new isearch-string)
  (isearch-exit))
(defun kill-isearch-region ()
  (interactive)
  (kill-region isearch-other-end (point))
  (isearch-exit))

;; isearch with region
(defadvice isearch-mode
    (around isearch-mode-default-string
            (forward &optional regexp op-fun recursive-edit word-p)
            activate)
  (if (and transient-mark-mode mark-active (not (eq (mark) (point))))
      (progn
        (isearch-update-ring (buffer-substring-no-properties (mark) (point)))
        (deactivate-mark)
        ad-do-it
        (if (not forward)
            (isearch-repeat-backward)
          (goto-char (mark))
          (isearch-repeat-forward)))
    ad-do-it))

(use-package wgrep
  :straight t)

;; replace

(use-package visual-regexp
  :straight t
  :bind ("C-%" . vr/query-replace))
(use-package visual-regexp-steroids
  :straight t
  :after (visual-regexp)
  :config
  (setq vr/engine 'python))
