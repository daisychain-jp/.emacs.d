(use-package ivy
  :straight t
  :diminish (ivy-mode)
  :bind (("C-;" . counsel-switch-buffer)
         ("C-M-r" . ivy-resume)
         :map ivy-minibuffer-map
         ("M-=" . ivy-minibuffer-grow)
         ("M--" . ivy-minibuffer-shrink)
         ("C-<" . ivy-describe-actions))
  :custom
  (ivy-display-style 'fancy)
  (ivy-use-virtual-buffers t)
  (ivy-count-format "%d/%d ")
  :config
  (ivy-mode 1)
  (setf ivy-re-builders-alist
        '((swiper . ivy-migemo--regex-plus)
          (counsel-ag . ivy--regex-plus)
          (counsel-rg . ivy--regex-plus)
          (counsel-describe-function . ivy--regex-ignore-order)
          (counsel-describe-variable . ivy--regex-ignore-order)
          (counsel-M-x . ivy--regex-ignore-order)
          (counsel-org-capture . ivy--regex-ignore-order)
          (org-refile . ivy--regex-ignore-order)
          (woman . ivy--regex-plus)
          (t . ivy-migemo--regex-plus))))

(defun ivy-describe-actions ()
  "Show available actions in current context."
  (interactive)
  (let ((ivy-read-action-function #'ivy-read-action-ivy))
    (ivy-read-action)))

(use-package swiper
  :straight t
  :after ivy
  :bind (:map global-map
              ("C-s" . my-swiper)
              :map swiper-map
              ("C-i" . swiper-isearch-toggle)
              :map swiper-isearch-map
              ("C-i" . swiper-isearch-toggle)))

(defun my-swiper (&optional Initial-Input)
  "Exec swiper in my way with `INITIAL-INPUT'."
  (interactive)
  (if (region-active-p)
      (swiper-thing-at-point)
    (swiper)))

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
   ("M-y" . yank-pop))
  :config
  ;; user modification of 'ivy-initial-inputs-alist must sit here
  ;; since counsel does modify it
  (mapc (lambda (inputs)
          (map-put ivy-initial-inputs-alist
                   (car inputs) (cdr inputs)))
        '((counsel-org-capture . "")))
  (setq counsel-linux-apps-directories
        (append counsel-linux-apps-directories
                '("~/Desktop"
                  "~/usr/share/applications")))
  (bind-keys :map hydra-base-map
             ("C-d" . counsel-hydra-heads))
  (defhydra hydra-search (global-map "M-s s"
                                     :color teal)
    "Search"
    ("s" counsel-ag)
    ("r" counsel-rg)
    ("g" counsel-grep-or-swiper)
    ("i" counsel-git-grep)
    ("q" nil "quit"))
  (defhydra hydra-help (global-map "C-M-h"
                                   :color teal)
    "Hint"
    ("a" counsel-apropos)
    ("f" counsel-describe-function)
    ("c" counsel-faces)
    ("v" counsel-describe-variable)
    ("m" counsel-descbinds)
    ("i" info)
    ("S" info-lookup-symbol)
    ("n" view-emacs-news)
    ("o" counsel-info-lookup-symbol)
    ("l" counsel-find-library)
    ("d" counsel-dash)
    ("p" describe-package)
    ("q" nil "quit"))
  (defhydra hydra-git (global-map "C-c g"
                                  :color teal)
    "Git"
    ("s" counsel-git-grep)
    ("c" counsel-git-checkout)
    ("f" magit-file-dispatch)
    ("l" counsel-git-log)
    ("w" counsel-git-change-worktree)
    ("z" counsel-git-stash)
    ("q" nil "quit")))

(use-package counsel-dash
  :straight t
  :after (counsel)
  :bind (:map global-map
              ("C-c d" . counsel-dash))
  :config
  (setf counsel-dash-docsets-path (expand-file-name (format "%s/lib/dash/docsets" env-var-dir)))
  (setf counsel-dash-browser-func 'eww)
  (setf counsel-dash-min-length 1))
