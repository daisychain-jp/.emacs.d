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
                mode-name
                minor-mode-alist))
