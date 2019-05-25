(use-package plantuml-mode
  :straight t
  :delight " UM"
  :hook
  (plantuml-mode . (lambda ()
                     (setq-local helm-dash-docsets helm-dash-docsets-plantuml-mode)))
  :custom
  (plantuml-jar-path "/usr/share/plantuml/plantuml.jar"))
