(use-package restart-emacs
  :straight t
  :after hydra
  :config
  (defhydra hydra-exit (global-map "C-M-<delete>"
                                   :exit t)
    "Exit"
    ("r" restart-emacs)
    ("R" (lambda () (interactive)
           (let ((confirm-kill-processes nil))
             (restart-emacs)))
     "force to restart emacs")
    ("k" save-buffers-kill-emacs)
    ("K" (lambda () (interactive)
           (let ((confirm-kill-processes nil))
             (save-buffers-kill-emacs)))
     "force to kill processes")
    ("c" nil "cancel")
    ("C-g" nil "quit")
    ("q" nil "quit")))
