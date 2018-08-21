(setq line-number-mode nil)
(setq column-number-mode nil)
(setq display-time-24hr-format t)
(setq display-time-mail-string "")
(setq display-time-default-load-average nil)
(display-time-mode t)

(defface mode-line-modified nil nil)
(defface mode-line-read-only nil nil)
(defface mode-line-read-only-modified nil nil)

(defun truncate-file-name (file-name len)
  (let* ((base (file-name-base file-name))
         (ext (file-name-extension file-name)))
    (cond
     ((<= (length file-name) len) file-name)
     (t (concat
         (s-truncate (- len (length ext) 1) base)
         ext)))))

(setq-default mode-line-format
              '(" "
                mode-line-mule-info
                (:eval
                 (let* ((raw-str (truncate-file-name (buffer-name) 24))
                        (str (s-replace "%" "%%" raw-str)))
                   (cond
                    ((and buffer-read-only (buffer-modified-p))
                     (propertize str 'face 'mode-line-read-only-modified))
                    (buffer-read-only
                     (propertize str 'face 'mode-line-read-only))
                    ((buffer-modified-p)
                     (propertize str 'face 'mode-line-modified))
                    (t str))))
                (:eval
                 (let ((str (if line-number-mode
                                "^%l-%p" "")))
                   (if column-number-mode
                       (concat str "_%c") str)))
                " "
                global-mode-string
                " "
                mode-name
                minor-mode-alist))

;; abbreviate mode name
(defvar mode-line-cleaner-alist
  '( ;; For minor-mode, first char is 'space'
    (buffer-face-mode         . "")
    (ace-isearch-mode         . "")
    (company-mode             . " Co")
    (eldoc-mode               . "")
    (elisp-slime-nav-mode     . " En")
    (flymake-mode             . " Fm")
    (helm-gtags-mode          . " Hg")
    (helm-migemo-mode         . "")
    (helm-mode                . "")
    (rainbow-mode             . " rb")
    (reveal-mode              . " rv")
    ;; (volatile-highlights-mode . "")
    ;; Major modes
    (magit-status-mode     . "MG")
    (eww-mode              . "EW")
    (elfeed-show-mode      . "EF")
    (elfeed-search-mode    . "EF")
    (twittering-mode       . "TW")
    (org-mode              . "OG")
    (org-agenda-mode       . "OG-A")
    (org-capture-mode      . "OG-C")
    (dired-mode            . "DD")
    (emacs-lisp-mode       . "EL")
    (lisp-interaction-mode . "LI")
    (markdown-mode         . "MD")
    (mew-summary-mode      . "MW-S")
    (mew-draft-mode        . "MW-D")
    (mew-message-mode      . "MW-M")
    (mew-virtual-mode      . "MW-V")
    (nxml-mode             . "XM")
    (python-mode           . "PY")
    (ruby-mode             . "RB")
    (sh-mode               . "SH")
    (text-mode             . "TX")
    (shell-mode            . "SM")
    (term-mode             . "TM")
    (log-view-mode         . "LV")))
(defun clean-mode-line ()
  (interactive)
  (loop for (mode . mode-str) in mode-line-cleaner-alist
        do
        (let ((old-mode-str (cdr (assq mode minor-mode-alist))))
          (when old-mode-str
            (setcar old-mode-str mode-str))
          ;; major mode
          (when (eq mode major-mode)
            (setq mode-name mode-str)))))

(add-hook 'after-change-major-mode-hook 'clean-mode-line)
