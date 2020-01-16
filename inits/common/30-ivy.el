(use-package ivy
  :straight t
  :diminish (ivy-mode)
  :after (avy-migemo)
  :bind (("C-;" . ivy-switch-buffer)
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
  :bind (("C-s" . (lambda (&optional arg)
                    (interactive "P")
                    (let ((ivy-height 7)
                          (swiper-func (cl-case (prefix-numeric-value arg)
                                         (4 '(swiper-all))
                                         (t '(swiper-isearch))))
                          (swiper-thing-at-point-func (cl-case (prefix-numeric-value arg)
                                                        (4 '(swiper-all-thing-at-point))
                                                        (t '(swiper-isearch-thing-at-point)))))
                      (if (use-region-p)
                          (apply swiper-thing-at-point-func)
                        (apply swiper-func))))))
  :config
  (require 'avy-migemo-e.g.swiper))

(use-package counsel
  :straight t
  :after (swiper hydra)
  :bind
  (("C-x b" . counsel-switch-buffer)
   ("C-M-y" . counsel-yank-pop)
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-c b" . counsel-bookmark)
   ("C-c i" . counsel-semantic-or-imenu)
   ("C-c r" . counsel-recentf)
   ("C-c z" . counsel-fzf)
   ("M-\\" . counsel-locate)
   ("C-M-SPC" . counsel-mark-ring)
   :map ivy-minibuffer-map
   ("C-l" . counsel-up-directory)
   ("TAB" . counsel-down-directory)
   ("M-y" . ivy-next-line))
  :config
  (require 'avy-migemo-e.g.counsel)
  (bind-keys :map hydra-base-map
             ("C-d" . counsel-hydra-heads))
  (defhydra hydra-org (global-map "C-c o"
                                  :color teal)
    "Org Search"
    ("a" counsel-org-agenda-headlines)
    ("c" counsel-org-capture)
    ("g" counsel-org-goto)
    ("s" (helm-org-ql (current-buffer)))
    ("r" (helm-org-ql (file-expand-wildcards (concat env-doc-dir "/**/*.org"))))
    ("i" org-info-find-node)
    ("q" nil "quit"))
  (defhydra hydra-search (global-map "M-s"
                                     :color teal)
    "Search"
    ("ss" counsel-ag)
    ("sr" counsel-rg)
    ("sg" counsel-grep-or-swiper)
    ("si" counsel-git-grep)
    ("q" nil "quit"))
  (defhydra hydra-hint (global-map "C-c h"
                                   :color teal)
    "Hint"
    ("a" counsel-apropos)
    ("f" counsel-describe-function)
    ("v" counsel-describe-variable)
    ("c" counsel-describe-face)
    ("d" counsel-descbinds)
    ("i" counsel-info-lookup-symbol)
    ("l" counsel-find-library)
    ("s" counsel-info-lookup-symbol)
    ("q" nil "quit"))
  (defhydra hydra-git (global-map "C-c g"
                                  :color teal)
    "Git"
    ("s" counsel-git-grep)
    ("c" counsel-git-checkout)
    ("l" counsel-git-log)
    ("w" counsel-git-change-worktree)
    ("z" counsel-git-stash)
    ("q" nil "quit")))

(use-package counsel-dash
  :straight t
  :after (counsel)
  :bind (:map global-map
              ("C-c D" . counsel-dash))
  :config
  (setf counsel-dash-docsets-path (expand-file-name (format "%s/lib/dash/docsets" env-var-dir)))
  (setf counsel-dash-browser-func 'eww)
  (setf counsel-dash-min-length 1))
