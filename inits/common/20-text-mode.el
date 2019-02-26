(use-package text-mode
  :mode ("\\.txt$" . text-mode)
  :delight " TX"
  :hook (text-mode . (lambda ()
                       (buffer-face-set 'visible)
                       (setq-local truncate-lines t)))
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
