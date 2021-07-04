(use-package woman
  :delight " WM"
  :after (hydra)
  :bind (:map woman-mode-map
              ("C-M-m" . hydra-lazy-control/body))
  :init
  (defface woman-buffer `((t . (:font "fontset-default"
                                      :height ,(my-adjust-font-size 835))))
    "Default face in woman mode.")
  :hook (woman-mode . (lambda ()
                        (buffer-face-set 'woman-buffer)
                        (visual-line-mode 1)
                        (adaptive-wrap-prefix-mode 1))))

(defun my/tldr (command)
  "Show the output of tldr for COMMAND in a dedicated buffer."
  (interactive (list (read-string "Coomand: ")))
  (let* ((buf-name "*TL;DR*")
         (buffer (get-buffer-create buf-name))
         (display-buffer-alist
          `((,buf-name
             (display-buffer-reuse-window display-buffer-below-selected display-buffer-at-bottom)
             (window-height . 0.85)))))
    (with-current-buffer buffer
      (read-only-mode -1)
      (erase-buffer)
      (insert (shell-command-to-string (format "tldr %s"
                                               (shell-quote-argument command))))
      (ansi-color-apply-on-region (point-min) (point-max))
      (goto-char (point-min))
      (read-only-mode 1))
    (display-buffer buffer)
    (switch-to-buffer-other-window buffer)))
