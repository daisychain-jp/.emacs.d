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
  :hook
  (eshell-mode . (lambda ()
                   (bind-keys :map eshell-mode-map
                              ("C-j"    . eshell-send-input)
                              ("<up>"   . previous-line)
                              ("<down>" . next-line))))
  (eshell-kill . eshell-command-alert)
  :custom
  (eshell-prompt-function
   #'(lambda ()
       (format "[%s]\n%s"
               (eshell/basename (eshell/pwd))
               (if (= (user-uid) 0) "# " "$ "))))
  (eshell-prompt-regexp "[#$] ")
  (eshell-visual-subcommands '(("git" "log" "diff" "show")))
  :config
  (setq eshell-path-env (getenv "PATH")))

(defun eshell-command-alert (process status)
  "Send `alert' with severity based on STATUS when PROCESS finished."
  (let* ((cmd (process-command process))
         (buffer (process-buffer process))
         (msg (format "%s: %s" (mapconcat 'identity cmd " ")  status)))
    (if (string-prefix-p "finished" status)
        (alert msg :buffer buffer :severity  'normal)
      (alert msg :buffer buffer :severity 'urgent))))

(use-package tramp
  :defer t
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (add-to-list 'tramp-remote-path "~/bin")
  (add-to-list 'tramp-remote-path "~/usr/bin"))

(bind-keys :map completion-list-mode-map
           ("C-j" . choose-completion))
(delight 'completion-list-mode " CL")
