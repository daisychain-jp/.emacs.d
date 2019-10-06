(use-package python
  :delight " PY"
  :after (counsel)
  :bind (:map python-mode-map
              ("C-c C-/" . counsel-jedi))
  :hook (python-mode . (lambda ()
                         (setq-local flycheck-checker 'python-pylint)
                         (jedi:setup)
                         (jedi-mode 1)
                         (setq-local counsel-dash-docsets
                                     '("Python 3" "Pandas" "NumPy" "Matplotlib" "Scrapy" "PEPs" "lxml")))))

(use-package jedi
  :straight t)

(use-package elpy
  :straight t
  :custom
  (elpy-rpc-backend "jedi")
  :config
  (elpy-enable))
