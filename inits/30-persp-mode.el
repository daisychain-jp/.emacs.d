(el-get-bundle Bad-ptr/persp-mode.el)

(use-package persp-mode
  :config
  (setq persp-nil-name "default")
  (add-hook 'persp-activated-hook 'persp-register-buffers-on-create)

  (bind-keys :map global-map
             ("s-W" . persp-mode))
  )

(defun persp-register-buffers-on-create ()
  (interactive)
  (dolist (bufname (condition-case _
                       (helm-comp-read
                        "Buffers: "
                        (mapcar 'buffer-name (buffer-list))
                        :must-match t
                        :marked-candidates t)
                     (quit nil)))
    (persp-add-buffer (get-buffer bufname))))
