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
  (defhydra hydra-smartparens (global-map "M-("
                                          :foreign-keys warn
                                          :pre (set-face-background 'mode-line "DarkRed")
                                          :post (set-face-background 'mode-line "gray10")
                                          :hint nil)
    "Parens"
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
    ("r" sp-rewrap-sexp :exit t)
    ("u" sp-unwrap-sexp :exit t)
    ;; ;; Sexp juggling
    ("j" sp-join-sexp)
    ("t" sp-transpose-sexp)
    ;; Destructive editing
    ("k" sp-kill-sexp)
    ("w" sp-copy-sexp)
    ;; Quit
    ("q" nil)
    ("g" nil))
  ;; workaround for reverting background color in mode-line
  (bind-key* "M-)" (lambda ()
                     (interactive)
                     (set-face-background 'mode-line "gray10"))))
