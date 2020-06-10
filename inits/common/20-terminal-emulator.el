(use-package shell
  :delight " SM"
  :commands (shell)
  :config
  (bind-keys :map shell-mode-map
             ("C-j" . comint-send-input))
  (add-hook 'shell-mode-hook
            (lambda ()
              (buffer-face-set 'selecting)
              (setq-local counsel-dash-docsets '("Linux" "Pandoc")))))

;; term (ansi-term)
(use-package term
  :delight
  (term-mode " TR")
  :config
  (bind-keys :map term-mode-hook
             ("C-j" . term-send-input))
  (add-hook 'term-mode-hook
            (lambda ()
              (buffer-face-set 'selecting)
              (setq-local counsel-dash-docsets '("Linux" "Pandoc")))))

(use-package eshell
  :delight " ES"
  :hook
  (eshell-mode . (lambda ()
                   (buffer-face-set 'selecting)
                   (setq-local counsel-dash-docsets '("Linux" "Pandoc"))
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

(defun shell-switcher (shell-buf-name &optional force-create eshell-mode default-dir)
  "Switch to shell buffer named SHELL-BUF-NAME.

If optional argument FORCE-CREATE is non-nil,
 create shell buffer rather than switching.
If optional argument ESHELL-MODE is non-nil,
 use eshell mode instead of shell mode.
If optional argument DEFAULT-DIR is string, change directory to there at first."
  (interactive (let* ((shell-buf-name-select (read-buffer "Buffer name: " nil nil
                                                          (lambda (cand) (if (member (buffer-local-value 'major-mode (cdr cand))
                                                                                     '(shell-mode eshell-mode))
                                                                             t nil))))
                      (force-create-select (not (member shell-buf-name-select (mapcar (lambda (buf) (buffer-name buf))
                                                                                      (buffer-list)))))
                      (eshell-mode-select (if force-create-select
                                              (read-char-choice "Use shell rather than eshell? (y(s) or n(e)): " '(?y ?s ?n ?e))
                                            ?y)))
                 (list shell-buf-name-select
                       force-create-select
                       (or (char-equal ?n eshell-mode-select)
                           (char-equal ?e eshell-mode-select))
                       nil)))
  (if force-create
      (let ((default-directory (or default-dir (buffer-local-value 'default-directory (current-buffer)))))
        (if eshell-mode
            (progn
              (eshell t)
              (rename-buffer shell-buf-name t))
          (shell shell-buf-name)))
    (when (member shell-buf-name
                  (mapcar (lambda (buf) (buffer-name buf))
                          (buffer-list)))
      (switch-to-buffer shell-buf-name))))

(use-package tramp
  :defer t
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (add-to-list 'tramp-remote-path "~/bin")
  (add-to-list 'tramp-remote-path "~/usr/bin"))

(bind-keys :map completion-list-mode-map
           ("C-j" . choose-completion))
(delight 'completion-list-mode " CL")
