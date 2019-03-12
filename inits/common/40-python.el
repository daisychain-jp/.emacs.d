(add-hook 'python-mode-hook
          '(lambda ()
             (setq-local flycheck-checker 'python-pylint)
             (setq-local helm-dash-docsets
                         '("Python 3" "Pandas" "NumPy" "Matplotlib" "Scrapy" "PEPs" "lxml"))))
(delight 'python-mode " PY")
