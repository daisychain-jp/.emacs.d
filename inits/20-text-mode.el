(add-hook 'text-mode-hook
          (lambda ()
            (when (eq major-mode 'text-mode)
              (buffer-face-set 'visible))))
(delight 'text-mode " TX")
