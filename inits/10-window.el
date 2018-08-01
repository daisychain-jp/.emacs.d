(defun maximize-next-window ()
  "Switch to next window and delete the others."
  (interactive)
  (other-window 1)
  (delete-other-windows))

(el-get-bundle zoom-window)
(use-package zoom-window
  :init
  (with-eval-after-load "persp-mode-autoloads"
    (add-hook 'after-init-hook #'(lambda () (persp-mode 1))))
  :bind (("s-1" . zoom-window-zoom))
  :config
  (setq zoom-window-mode-line-color "#01012c")
  (setq zoom-window-use-persp t)
  (zoom-window-setup))
(defun zoom-another-window ()
  "Toggle the other window zoom."
  (interactive)
  (zoom-window-zoom)
  (zoom-window-next))
