(use-package ivy
  :straight t
  :diminish (ivy-mode)
  :bind (("C-x b" . ivy-switch-buffer)
         :map ivy-minibuffer-map
         ("M-=" . ivy-minibuffer-grow)
         ("M--" . ivy-minibuffer-shrink))
  :custom
  (ivy-display-style 'fancy)
  (ivy-use-virtual-buffers t)
  (ivy-count-format "%d/%d ")
  :config
  (ivy-mode 1))

(use-package swiper
  :ensure t
  :after (ivy avy-migemo)
  :bind (("C-s" . swiper-isearch)
         ("C-r" . swiper-isearch-backward)
         ("C-c C-r" . ivy-resume)))

(use-package counsel
  :straight t
  :after (ivy)
  :bind
  (("M-y" . counsel-yank-pop)
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-c b" . counsel-bookmark)))
