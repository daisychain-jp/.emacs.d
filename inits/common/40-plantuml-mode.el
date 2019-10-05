(use-package plantuml-mode
  :straight t
  :delight " UM"
  :hook
  (plantuml-mode . (lambda ()
                     (setq-local counsel-dash-docsets '("PlantUML"))))
  :custom
  (plantuml-jar-path "/usr/share/plantuml/plantuml.jar"))
