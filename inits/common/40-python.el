(use-package python
  :mode "\\.py\\'"
  :interpreter "python"
  :delight " PY"
  :hook
  ((python-mode inferior-python-mode) . my-python-mode-hook)
  (lsp-mode . (lambda ()
                (add-to-list 'lsp-disabled-clients 'jedi)
                (add-to-list 'lsp-enabled-clients 'pyls)))
  :config
  (defvar python-mode-initialized nil)

  (defun my-python-mode-hook ()
    (setq-local flycheck-checker 'python-pylint)
    (jedi:setup)
    (jedi-mode 1)
    (setq-local counsel-dash-docsets
                '("Python 3" "Pandas" "NumPy" "Matplotlib" "Scrapy" "PEPs" "lxml"))
    (unless python-mode-initialized
      (setq python-mode-initialized t)
      (info-lookup-add-help
       :mode 'python-mode
       :regexp "[a-zA-Z_0-9.]+"
       :doc-spec
       '(("(python)Python Module Index" )
         ("(python)Index"
          (lambda
            (item)
            (cond
             ((string-match
               "\\([A-Za-z0-9_]+\\)() (in module \\([A-Za-z0-9_.]+\\))" item)
              (format "%s.%s" (match-string 2 item)
                      (match-string 1 item)))))))))))

(use-package company-jedi
  :straight t
  :after python
  :hook (python-mode . my/python-mode-hook)
  :custom
  (jedi:tooltip-method nil)
  :config
  (defun my/python-mode-hook ()
    (add-to-list 'company-backends 'company-jedi)))
