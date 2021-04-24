(use-package nov
  :straight t
  :after (org)
  :demand t
  :mode (("\\.epub$" . nov-mode))
  :init
  (defface nov-buffer `((t . (:font "fontset-default"
                                    :height ,(my-adjust-font-size 1020))))
    "Default face for nov-mode.")
  :hook ((nov-mode . nov-abbreviate-file-name)
         (nov-mode . visual-line-mode)
         (nov-mode . (lambda ()
                       (buffer-face-set 'nov-buffer))))
  :custom
  (nov-variable-pitch nil)
  (nov-text-width t))

(defun nov-abbreviate-file-name ()
  "Shorten `nov-file-name' using `directory-abbrev-alist'."
  (setq nov-file-name (abbreviate-file-name nov-file-name)))
