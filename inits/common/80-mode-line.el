(setq line-number-mode nil)
(setq column-number-mode nil)
(setq display-time-24hr-format t)
(setq display-time-mail-string "")
(setq display-time-default-load-average nil)
(display-time-mode t)

(defface mode-line-modified nil nil)
(defface mode-line-read-only nil nil)
(defface mode-line-read-only-modified nil nil)

(defun truncate-buffer-name (buf-name max-display-len)
  ""
  (let ((base-name (file-name-base buf-name))
        (ext-name (file-name-extension buf-name)))
    (cl-labels ((truncate-display-string
                 (base ext truncated)
                 (if (<= (+ (string-width base)
                            (if (stringp ext)
                                (1+ (string-width ext)) 0)
                            (if (bound-and-true-p truncated) 2 0))
                         max-display-len)
                     (concat base
                             (when (bound-and-true-p truncated) "..")
                             (when (stringp ext) (concat "." ext)))
                   (truncate-display-string (substring base 0 (1- (length base)))
                                            ext
                                            t))))
      (truncate-display-string base-name ext-name nil))))

(setq-default mode-line-format
              '(" "
                mode-line-mule-info
                (:eval
                 (let* ((raw-str (truncate-buffer-name (buffer-name) 24))
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
                mode-name
                ;; disable showing minor modes
                ;; minor-mode-alist
                (projectile-mode (:eval (projectile-short-mode-line)))))
