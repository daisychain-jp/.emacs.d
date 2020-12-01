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

;; grep

(use-package compile
  :bind (:map compilation-mode-map
              ("C-j" . compile-goto-error)))

(use-package grep
  :bind (("M-s g" . grep-find)
         (:map grep-mode-map
               ("C-j" . compile-goto-error)))
  :custom
  (grep-program "rg")
  (grep-find-command '("rga --with-filename --no-heading --line-number --color never -e ''" . 66)))

(use-package find-dired
  :after (grep hydra)
  :custom
  (find-grep-options "-n -H --no-heading -q")
  (find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))
  :config
  (unbind-key "C-x f")
  (defhydra hydra-find-dired (global-map "C-x f"
                                         :color teal)
    "find dired"
    ("f" find-dired)
    ("F" find-lisp-find-dired)
    ("g" find-grep-dired)
    ("r" find-rg-dired)
    ("n" find-name-dired)
    ("d" find-lisp-find-dired-subdirectories)
    ("q" nil "quit")))

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
