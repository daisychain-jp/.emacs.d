(use-package nxml-mode
  :mode
  (("\.xml$"   . nxml-mode)
   ("\.xsl$"   . nxml-mode)
   ("\.xhtml$" . nxml-mode)
   ("\.page$"  . nxml-mode)
   ("\.plist$" . nxml-mode))
  :config
  (setq nxml-child-indent 2)
  (setq nxml-attribute-indent 2)
  (setq nxml-slash-auto-complete-flag t)
  (setq tab-width 2)
  (add-hook 'nxml-mode-hook 'coding-mode-hook-function)
  ;; pattern in hideshow mode for nxml mode
  (add-to-list 'hs-special-modes-alist
               (list 'nxml-mode
                     "<!--\\|<[^/>]*[^/]>"
                     "-->\\|</[^/>]*[^/]>"
                     "<!--"
                     'nxml-forward-element
                     nil)))
