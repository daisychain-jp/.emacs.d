(use-package smartparens
  :straight t
  :diminish
  ((smartparens-global-mode . "Sp")
   (smartparens-mode . "sp"))
  :after hydra
  :hook ((prog-mode . smartparens-mode)
         (prog-mode . show-smartparens-mode))
  :custom
  (sp-navigate-interactive-always-progress-point t)
  :config
  (defhydra hydra-smartparens (global-map "M-p"
                                          :foreign-keys warn
                                          :pre (set-face-background 'mode-line "DarkRed")
                                          :post (set-face-background 'mode-line "gray10")
                                          :hint nil)
    ;; Moving
    ("a" sp-beginning-of-sexp)
    ("e" sp-end-of-sexp)
    ("d" sp-down-sexp)
    ("f" sp-next-sexp)
    ("n" sp-down-sexp)
    ("b" sp-backward-sexp)
    ("p" sp-backward-up-sexp)
    ;; Slurping & barfing
    ("[" sp-backward-slurp-sexp)
    ("{" sp-backward-barf-sexp)
    ("]" sp-forward-slurp-sexp)
    ("}" sp-forward-barf-sexp)
    ;; Wrapping
    ("r" sp-rewrap-sexp)
    ("u" sp-unwrap-sexp)
    ;; ;; Sexp juggling
    ("j" sp-join-sexp)
    ("t" sp-transpose-sexp)
    ;; Destructive editing
    ("k" sp-kill-sexp)
    ("w" sp-copy-sexp)
    ;; Quit
    ("q" nil)
    ("g" nil)))
