(use-package latex-mode
  :mode (("\\.tex$" . latex-mode))
  :hook (latex-mode . (lambda ()
                        (setq-local counsel-dash-docsets '("LaTeX")))))
