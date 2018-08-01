;; disable input method in mini buffer
(defun my-minibuffer-setup-hook ()
  (if current-input-method
      (deactivate-input-method))
  )
(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)
