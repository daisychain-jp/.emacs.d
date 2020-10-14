(use-package eww
  :delight " EW"
  :bind (:map eww-mode-map
              ("C-M-m" . hydra-lazy-control/body))
  :custom
  ;; set enough large column number to prevent from inserting line break
  (shr-width 10000)
  :config
  (bind-keys :map eww-mode-map
             ("C-j" . eww-follow-link)
             ("["   . eww-back-url)
             ("]"   . eww-next-url)
             ("T"   . eww-goto-title-heading)
             ("L"   . counsel-eww-headings)
             :map eww-bookmark-mode-map
             ("C-j" . eww-bookmark-browse))
  (setq shr-use-fonts nil)
  (setq shr-image-animate nil)
  (add-hook 'eww-mode-hook
            (lambda ()
              (buffer-face-set 'readable)
              (whitespace-mode -1)
              (eww-lazy-control)))
  (add-hook 'eww-after-render-hook
            (lambda ()
              (visual-line-mode 1)
              (eww-goto-contents)))
  (add-hook 'eww-after-render-hook #'eww-rename-buffer)
  (setq eww-header-line-format nil))

(defun eww-goto-contents ()
  (let ((url (eww-current-url)))
    (when (string-match (concat (regexp-quote
                                 "https://eow.alc.co.jp/search?q=") "\\([^&]+\\)") url)
      (let* ((search-word-escape (match-string 1 url))
             (search-word (org-link-unescape search-word-escape)))
        (search-forward (format "* %s" search-word))
        (beginning-of-line 1)
        (recenter-top-bottom 0)))
    (when (string-match-p (regexp-quote "https://www.weblio.jp/content/") url)
      (forward-line 44))))

(defun eww-lazy-control ()
  "Lazy control in EWW."
  (setq-local hlc/beginning-func 'eww-goto-title-heading)
  (setq-local hlc/forward-paragraph-func
              (lambda ()
                (interactive)
                (forward-paragraph 2)
                (backward-paragraph 1)
                (forward-line 1)
                (recenter-top-bottom 0)))
  (setq-local hlc/backward-paragraph-func
              (lambda ()
                (interactive)
                (backward-paragraph 2)
                (forward-paragraph 1)
                (backward-paragraph 1)
                (forward-line 1)
                (recenter-top-bottom 0)))
  (setq-local hlc/next-thing-func 'shr-next-link)
  (setq-local hlc/previous-thing-func 'shr-previous-link)
  (setq-local hlc/readable-func 'eww-readable)
  (setq-local hlc/external-func 'eww-browse-with-external-browser)
  (setq-local hlc/enter-func 'eww-follow-link)
  (setq-local hlc/backward-page-func 'eww-back-url)
  (setq-local hlc/forward-page-func 'eww-forward-url))

(defun open-in-new-buffer (orig-fun &rest args)
  "If this function is added as an advice to ORIG-FUN,
a new buffer will be created and
the original function will be called in it.

ARGS will be passed to the original function."
  (with-temp-buffer
    (apply orig-fun args)))

;; control site color
(defvar eww-disable-colorize t)
(defun shr-colorize-region--disable (orig start end fg &optional bg &rest _)
  (unless eww-disable-colorize
    (funcall orig start end fg bg)))
(advice-add 'shr-colorize-region :around 'shr-colorize-region--disable)
(defun eww-disable-color ()
  "disable original site color"
  (interactive)
  (setq-local eww-disable-colorize t)
  (eww-reload))
(defun eww-enable-color ()
  "enable original site color"
  (interactive)
  (setq-local eww-disable-colorize nil)
  (eww-reload))

;; control displaying images
(defvar eww-display-images t)
(defun eww-toggle-images ()
  (interactive)
  (setq eww-display-images (not eww-display-images))
  (if eww-display-images
      (setq-local shr-put-image-function 'shr-put-image)
    (setq-local shr-put-image-function 'shr-put-image-alt))
  (eww-reload))
(defun shr-put-image-alt (spec alt &optional flags)
  (insert alt))

(defun eww-headings-dom ()
  "Return heading list as a dom from xml."
  (let ((source (plist-get eww-data :source))
        (dom nil))
    (with-temp-buffer
      (let ((source-file (make-temp-file "source-"))
            (coding-system-for-write 'utf-8-unix))
        (insert source)
        (write-region (point-min) (point-max) source-file nil)
        (erase-buffer)
        (call-process "extract_headings" source-file t)
        (delete-file source-file)
        (libxml-parse-xml-region (point-min) (point-max))))))

(defun eww-rename-buffer ()
  "Rename EWW mode buffer.

If associated HTML file have a title tag, use title as a buffer name.
Otherwise, use a current URL."
  (let ((title (plist-get eww-data :title))
        (url (file-name-base (eww-current-url))))
    (rename-buffer (format "eww: %s" (or (if (and title (> (length title) 0))
                                             title nil)
                                         url "")) t)))

(defun eww-goto-title-heading ()
  "Set point to a line which contaings the possible heading."
  (interactive)
  (when-let* ((headings-dom (eww-headings-dom))
              (possible-heading (cl-reduce (lambda (node-a node-b)
                                             (if (not (bound-and-true-p node-a))
                                                 (if (not (bound-and-true-p node-b))
                                                     nil
                                                   node-b)
                                               (if (>= (string-to-number (dom-attr node-a 'proximity))
                                                       (string-to-number (dom-attr node-b 'proximity)))
                                                   node-a node-b)))
                                           (dom-children headings-dom)
                                           :initial-value nil))
              (possible-text (dom-text possible-heading))
              (match-pos (or (re-search-forward (format "^*?[[:blank:]]*%s[[:blank:]]*$" (regexp-quote possible-text)) nil t 1)
                             (re-search-backward (format "^*?[[:blank:]]*%s[[:blank:]]*$" (regexp-quote possible-text)) nil t 1))))
    (beginning-of-line)
    (recenter-top-bottom 0)))

(defun counsel-eww-headings ()
  "Go to selected heading line."
  (interactive)
  (lexical-let ((headings-root (eww-headings-dom))
                (cur-buf (current-buffer)))
    (ivy-read "Heading : "
              (mapcar (lambda (heading-node)
                        (when-let* ((heading (dom-text heading-node))
                                    (tag (symbol-name (dom-tag heading-node)))
                                    (match-pos (string-match "h\\([1-6]\\{1\\}\\)" tag))
                                    (indent (string-to-number (match-string 1 tag))))
                          (format "%s%s"
                                  (apply 'concat (make-list indent " "))
                                  heading)))
                      (dom-children headings-root))
              :action (lambda (candidate)
                        (when-let ((heading (string-trim candidate))
                                   (match-pos (or (re-search-forward (format "*?[[:blank:]]*%s[[:blank:]]*$" (regexp-quote heading)) nil t 1)
                                                  (re-search-backward (format "*?[[:blank:]]*%s[[:blank:]]*$" (regexp-quote heading)) nil t 1))))
                          (when (bufferp cur-buf)
                            (switch-to-buffer cur-buf)
                            (beginning-of-line)
                            (recenter-top-bottom 0)))))))
