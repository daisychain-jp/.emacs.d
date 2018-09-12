(use-package shell
  :delight " SM"
  :commands (shell)
  :config
  (bind-keys :map shell-mode-map
             ("C-j" . comint-send-input))
  (add-hook 'shell-mode-hook
            (lambda ()
              (buffer-face-set 'selecting)
              (setq-local helm-dash-docsets helm-dash-docsets-shell-mode))))

;; term (ansi-term)
(use-package term
  :delight
  (term-mode " TR")
  :config
  (bind-keys :map term-mode-hook
             ("C-j" . term-send-input))
  (add-hook 'term-mode-hook
            (lambda ()
              (buffer-face-set 'selecting))))

(use-package eshell
  :delight " ES"
  :hook (eshell-mode . (lambda ()
                         (bind-keys :map eshell-mode-map
                                    ("C-j" . eshell-send-input))))
  :custom
  (eshell-prompt-function
   #'(lambda ()
       (format "[%s]\n%s"
               (eshell/basename (eshell/pwd))
               (if (= (user-uid) 0) "# " "$ "))))
  (eshell-prompt-regexp "[#$] ")
  :config
  (setq eshell-path-env (getenv "PATH")))

(bind-keys :map completion-list-mode-map
           ("C-j" . choose-completion))
(delight 'completion-list-mode " CL")
