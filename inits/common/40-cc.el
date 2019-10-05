(defconst my-c-style
  '("stroustrup"
    (c-basic-offset . 4)
    (c-comment-only-line-offset . 0)
    (c-hanging-braces-alist
     . ((brace-if-brace before after)
        (substatement-open before after)
        ))
    (c-hanging-colons-alist
     . ((case-label after)
        ))
    (c-cleanup-list
     . (brace-else-brace
        brace-elseif-brace
        empty-defun-braces
        defun-close-semi
        list-close-comma
        scope-operator
        ))
    (c-offsets-alist
     . ((arglist-intro . +)
        (arglist-cont-nonempty . c-lineup-arglist)
        ))
    ))

(defconst my-objc-style
  '("stroustrup"
    (c-basic-offset . 2)
    (c-comment-only-line-offset . 0)
    (c-hanging-braces-alist
     . ((brace-if-brace before after)
        (substatement-open before after)
        ))
    (c-hanging-colons-alist
     . ((case-label after)
        ))
    (c-cleanup-list
     . (brace-else-brace
        brace-elseif-brace
        empty-defun-braces
        defun-close-semi
        list-close-comma
        scope-operator
        ))
    (c-offsets-alist
     . ((arglist-intro . +)
        (arglist-cont-nonempty . c-lineup-arglist)
        ))
    ))

(c-add-style "my-c-style" my-c-style)
(c-add-style "my-objc-style" my-objc-style)
(setq c-default-style "my-c-style")

(defun my-c-mode-common-hook ()
  (setq c-tab-always-indent t)
  (setq c-auto-newline t)
  (c-toggle-auto-newline t)
  (setq c-hungry-delete-key t)
  (setq c-auto-align-backslashes nil)
  (setq c-echo-syntactic-information-p t)
  (setq-local counsel-dash-docsets '("C")))

(defun my-c-mode-hook ()
  (c-set-style "my-c-style")
  (setq tab-width 4))

(defun my-c++-mode-hook ()
  (c-set-style "my-c-style")
  (setq tab-width 4))

(defun my-objc-mode-hook ()
  (c-set-style "my-objc-style")
  (setq tab-width 2))

(add-hook 'c-mode-hook 'my-c-mode-common-hook)
(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c-mode-common-hook)
(add-hook 'c++-mode-hook 'my-c++-mode-hook)
(add-hook 'objc-mode-hook 'my-c-mode-common-hook)
(add-hook 'objc-mode-hook 'my-objc-mode-hook)
