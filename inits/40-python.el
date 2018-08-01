(add-hook 'python-mode-hook
          '(lambda ()
             (setq-local flycheck-checker 'python-pylint)
             (flycheck-mode 1)
             (setq-local helm-dash-docsets helm-dash-docsets-python-mode)))
