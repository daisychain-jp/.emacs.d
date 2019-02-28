(use-package helm-dash
  :straight t
  :config
  (setq helm-dash-docsets-path (concat env-var-dir "/lib/dash/docsets"))
  (setq helm-dash-browser-func 'eww)
  (setq helm-dash-min-length 2)
  (bind-keys :map global-map
             ("M-D" . helm-dash)))

(defvar helm-dash-docsets-c-mode '("C") "Docsets for 'c-mode.")
(defvar helm-dash-docsets-sh-mode '("Bash") "Docsets for 'sh-mode.")
(defvar helm-dash-docsets-shell-mode '("Linux" "Pandoc") "Docsets for 'shell-mode.")
(defvar helm-dash-docsets-emacs-lisp-mode '("Emacs Lisp" "Common Lisp") "Docsets for 'emacs-lisp-mode.")
(defvar helm-dash-docsets-ruby-mode '("Ruby") "Docsets for 'ruby-mode.")
(defvar helm-dash-docsets-python-mode '("Python 3" "Pandas" "NumPy" "Matplotlib" "Scrapy" "PEPs") "Docsets for 'python-mode.")
(defvar helm-dash-docsets-web-mode '("HTML") "Docsets for 'web-mode.")
(defvar helm-dash-docsets-css-mode '("CSS") "Docsets for 'css-mode.")
(defvar helm-dash-docsets-js-mode '("JavaScript") "Docsets for 'js-mode.")
(defvar helm-dash-docsets-latex-mode '("LaTeX") "Docsets for 'latex-mode.")
(defvar helm-dash-docsets-plantuml-mode '("PlantUML") "Docsets for 'plantuml-mode.")
