(use-package flycheck
  :straight t
  :diminish (flycheck-mode . "fc")
  :after (hydra)
  :hook ((c-mode sh-mode emacs-lisp-mode ruby-mode python-mode sql-mode) . flycheck-mode)
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

(use-package ispell
  :custom
  (ispell-program-name "/usr/bin/aspell"))

(use-package flyspell
  :diminish "fs"
  :after (hydra)
  :custom
  (flyspell-issue-message-flag nil)
  :config
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
