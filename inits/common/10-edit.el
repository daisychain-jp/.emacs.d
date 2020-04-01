(electric-indent-mode 1)

(use-package hungry-delete
  :straight t
  :diminish "hd"
  :config
  (global-hungry-delete-mode 1))

(bind-keys :map global-map
           ("M-u" . upcase-dwim)
           ("M-l" . downcase-dwim)
           ("M-c" . capitalize-dwim))

;; suppress inquiry in up/downcase-region
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; prevent from executing save-buffers-kill-emacs by mistake
(unbind-key "C-x C-c")
(unbind-key "C-x C-u")
(unbind-key "C-x C-l")
