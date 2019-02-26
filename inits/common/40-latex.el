(use-package latex-mode
  :mode (("\\.tex$" . latex-mode))
  :hook (latex-mode . (lambda ()
                        (setq-local helm-dash-docsets helm-dash-docsets-latex-mode))))
