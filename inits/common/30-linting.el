(use-package flycheck
  :straight t
  :diminish (flycheck-mode . "fc")
  :after (hydra)
  :hook ((c-mode sh-mode emacs-lisp-mode ruby-mode python-mode js2-mode sql-mode) . flycheck-mode)
  :custom
  (flycheck-check-syntax-automatically
   '(save new-line idle-change))
  (flycheck-display-errors-delay 10.0)
  (flycheck-checker-error-threshold 1000)
  :config
  (bind-keys :map flycheck-error-list-mode-map
             ("o" . delete-other-windows))
  (defhydra hydra-flycheck (flycheck-mode-map "C-c !"
                                              :color red)
    "Flycheck"
    ("C-c" flycheck-compile)
    ("C-w" flycheck-copy-errors-as-kill)
    ("?" flycheck-describe-checker)
    ("C" flycheck-clear)
    ("H" display-local-help)
    ("V" flycheck-version)
    ("c" flycheck-buffer)
    ("e" flycheck-explain-error-at-point)
    ("d" flycheck-display-error-at-point)
    ("i" flycheck-manual)
    ("l" flycheck-list-errors)
    ("n" flycheck-next-error)
    ("p" flycheck-previous-error)
    ("s" flycheck-select-checker)
    ("v" flycheck-verify-setup)
    ("x" flycheck-disable-checker)
    ("q" nil "quit")))

(use-package flyspell
  :diminish "fs"
  :if (executable-find "aspell")
  :after (hydra)
  :custom
  (ispell-program-name "aspell")
  (flyspell-issue-message-flag nil)
  :config
  ;; avoid checking for Japanese characters
  (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+"))
  (setq-default ispell-extra-args '("--sug-mode=ultra"
                                    "--lang=en_US"))
  (when (string-match-p "--camel-case"
                        (shell-command-to-string (concat ispell-program-name " --help")))
    (push "--camel-case" ispell-extra-args))
  (bind-keys :map flyspell-mode-map
             ("C-,"   . nil)
             ("C-."   . nil)
             ("C-;"   . nil)
             ("C-c $" . nil)
             ("C-M-i" . nil))
  (defhydra hydra-flyspell (flyspell-mode-map "C-c $"
                                              :color red)
    "Flyspell"
    ("c" flyspell-buffer)
    ("n" flyspell-goto-next-error)
    ("." flyspell-auto-correct-word)
    ("q" nil "quit")))
