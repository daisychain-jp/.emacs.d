(delight 'conf-unix-mode " CF")

(delight 'fundamental-mode " FU")

(add-hook 'Info-mode-hook
          (lambda ()
            (buffer-face-set 'recognizable)
            (setq-local truncate-lines t)
            (view-mode 1)))
