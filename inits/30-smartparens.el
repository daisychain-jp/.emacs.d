(el-get-bundle smartparens)
(use-package smartparens-config
  :diminish (smartparens-mode . "sp")
  :config
  (show-smartparens-global-mode 1)
  (bind-keys
   :map smartparens-mode-map
   ("C-M-f"       . sp-forward-sexp)
   ("C-M-b"       . sp-backward-sexp)
   ("C-M-n"       . sp-next-sexp)
   ("C-M-p"       . sp-previous-sexp)
   ("C-M-a"       . sp-beginning-of-sexp)
   ("C-M-e"       . sp-end-of-sexp)
   ("C-M-w"       . sp-copy-sexp)
   ("C-M-k"       . sp-kill-sexp)
   ("C-M-t"       . sp-transpose-sexp)
   ("M-U"         . sp-unwrap-sexp)
   ("M-R"         . sp-rewrap-sexp)
   ("M-<right>"   . sp-forward-slurp-sexp)
   ("C-M-<right>" . sp-forward-barf-sexp)
   ("M-<left>"    . sp-backward-slurp-sexp)
   ("C-M-<left>"  . sp-backward-barf-sexp)))
