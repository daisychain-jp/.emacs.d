(use-package ivy
  :straight t
  :diminish (ivy-mode)
  :after (avy-migemo)
  :bind (("C-x b"   . ivy-switch-buffer)
         ("C-c C-r" . ivy-resume)
         :map ivy-minibuffer-map
         ("M-=" . ivy-minibuffer-grow)
         ("M--" . ivy-minibuffer-shrink))
  :custom
  (ivy-display-style 'fancy)
  (ivy-use-virtual-buffers t)
  (ivy-count-format "%d/%d ")
  :config
  (ivy-mode 1)
  (require 'avy-migemo-e.g.ivy))

(use-package swiper
  :ensure t
  :after (ivy avy-migemo)
  :bind (("C-s" . (lambda ()
                    (interactive)
                    (let ((ivy-height 5))
                      (if (not (use-region-p))
                          (swiper-isearch)
                        (swiper-isearch-thing-at-point))))))
  :config
  (require 'avy-migemo-e.g.swiper))

(use-package counsel
  :straight t
  :after (ivy)
  :bind
  (("M-y" . counsel-yank-pop)
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-c b" . counsel-bookmark)
   ("C-c r" . counsel-recentf))
  :config
  (require 'avy-migemo-e.g.counsel))
