(electric-indent-mode 1)

(use-package hungry-delete
  :straight t
  :diminish "hd"
  :config
  (global-hungry-delete-mode 1))

;; invoke up/downcase-region without any inquiry
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
