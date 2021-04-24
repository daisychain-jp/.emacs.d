(use-package text-mode
  :mode ("\\.txt$" . text-mode)
  :delight " TX"
  :init
  (defface text-buffer `((t . (:font "fontset-default"
                                     :height ,(my-adjust-font-size 720))))
    "Default face for editing text.")
  :hook (text-mode . (lambda ()
                       (buffer-face-set 'text-buffer)
                       (setq-local truncate-lines nil)))
  :bind (:map text-mode-map
              ("C-M-m" . text-lazy-control)))

(defun text-lazy-control ()
  "Lazy control in text-mode."
  (interactive)
  (setq-local hlc/forward-paragraph-func
              (lambda ()
                (interactive)
                (forward-paragraph 2)
                (backward-paragraph 1)
                (forward-line 1)
                (recenter-top-bottom 0)))
  (setq-local hlc/backward-paragraph-func
              (lambda ()
                (interactive)
                (backward-paragraph 2)
                (forward-paragraph 1)
                (backward-paragraph 1)
                (forward-line 1)
                (recenter-top-bottom 0)))
  (hydra-lazy-control/body))
