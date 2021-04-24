(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)
(defun my-minibuffer-setup-hook ()
  ;; disable input method in mini buffer
  (if current-input-method
      (deactivate-input-method))
  (setq-local face-remapping-alist '((default :height 0.9))))
