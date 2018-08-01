(defun my-before-save-hook ()
  (delete-trailing-whitespace))

(add-hook 'before-save-hook 'my-before-save-hook)
