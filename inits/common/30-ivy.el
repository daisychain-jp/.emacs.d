(use-package ivy
  :straight t
  :diminish (ivy-mode)
  :after (avy-migemo)
  :bind (("C-x b" . ivy-switch-buffer)
         ("C-M-r" . ivy-resume)
         :map ivy-minibuffer-map
         ("M-=" . ivy-minibuffer-grow)
         ("M--" . ivy-minibuffer-shrink))
  :custom
  (ivy-display-style 'fancy)
  (ivy-use-virtual-buffers t)
  (ivy-count-format "%d/%d ")
  :config
  (ivy-mode 1)
  (setf ivy-re-builders-alist
        '((t . ivy--regex-plus)))
  (require 'avy-migemo-e.g.ivy))

(use-package swiper
  :straight t
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
  :after (swiper hydra)
  :bind
  (("C-;" . counsel-switch-buffer)
   ("M-y" . counsel-yank-pop)
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-c b" . counsel-bookmark)
   ("C-c i" . counsel-semantic-or-imenu)
   ("C-c r" . counsel-recentf)
   ("C-M-SPC" . counsel-mark-ring)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line))
  :config
  (require 'avy-migemo-e.g.counsel)
  (bind-keys :map hydra-base-map
             ("C-d" . counsel-hydra-heads))
  (defhydra hydra-describe (global-map "C-c h"
                                       :color teal)
    "Describe"
    ("h" counsel-apropos)
    ("f" counsel-describe-function)
    ("v" counsel-describe-variable)
    ("c" counsel-describe-face)
    ("d" counsel-descbinds)
    ("i" counsel-info-lookup-symbol)
    ("q" nil "quit")))
