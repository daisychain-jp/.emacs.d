(el-get-bundle helm)
(use-package helm
  :bind (("C-;"     . helm-mini)
         ("C-M-;"   . helm-for-files)
         ("M-x"     . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-c C-b" . helm-bookmarks)
         ("M-y"     . helm-show-kill-ring)
         ("C-M-SPC" . helm-all-mark-rings)
         ("C-c i"   . helm-imenu)
         ("C-M-/"   . helm-resume)
         ("C-x r j" . helm-register)
         ("C-M-h"   . helm-apropos)
         ("C-c C-i" . helm-info)
         ("C-c w"   . helm-web-suggest))
  :custom
  (helm-mode-fuzzy-match t)
  (helm-locate-project-list
   (file-expand-wildcards (concat env-proj-dir "/*")))
  :config
  (setq helm-truncate-lines nil)
  (setq helm-split-window-default-side 'same)
  (setq helm-for-files-preferred-list
        '(helm-source-buffers-list
          helm-source-recentf
          helm-source-bookmarks
          helm-source-file-cache
          helm-source-files-in-current-dir
          helm-source-emacs-commands
          helm-source-bookmark-set
          helm-source-locate))
  (bind-keys :map helm-map
             ("C-h" . delete-backward-char)
             ("C-j" . helm-maybe-exit-minibuffer)
             ("C-t" . helm-toggle-truncate-line)
             :map helm-find-files-map
             ("C-h" . delete-backward-char)
             ("TAB" . helm-execute-persistent-action)
             :map helm-read-file-map
             ("TAB" . helm-execute-persistent-action)
             :map isearch-mode-map
             ("C-o" . helm-occur-from-isearch)
             ("C-m" . helm-multi-occur-from-isearch))
  (add-hook 'helm-major-mode-hook
            (lambda ()
              (buffer-face-set 'selecting))))

(defun helm-web-suggest (arg)
  "More powerful 'helm-mini search web.
With prefix ARG, this command searches japanese source."
  (interactive "p")
  (let ((helm-for-files-preferred-list
         '(helm-source-google-suggest
           helm-source-wikipedia-suggest))
        (helm-google-suggest-default-browser-function 'eww-browse-url)
        (browse-url-browser-function 'eww-browse-url))
    (cl-case arg
      (4 (let ((helm-search-suggest-action-wikipedia-url
                "https://en.wikipedia.org/wiki/Special:Search?search=%s")
               (helm-wikipedia-suggest-url
                "https://en.wikipedia.org/w/api.php?action=opensearch&search=%s")
               (helm-wikipedia-summary-url
                "https://en.wikipedia.org/w/api.php?action=parse&format=json&prop=text&section=0&page=%s"))
           (helm-for-files)))
      (t (let ((helm-search-suggest-action-wikipedia-url
                "https://ja.wikipedia.org/wiki/Special:Search?search=%s")
               (helm-wikipedia-suggest-url
                "https://ja.wikipedia.org/w/api.php?action=opensearch&search=%s")
               (helm-wikipedia-summary-url
                "https://ja.wikipedia.org/w/api.php?action=parse&format=json&prop=text&section=0&page=%s"))
           (helm-for-files))))))

(defvar helm-source-emacs-commands
  (helm-build-sync-source "Emacs commands"
    :candidates
    (lambda ()
      (let ((cmds))
        (mapatoms
         (lambda (elt) (when (commandp elt) (push elt cmds))))
        cmds))
    :coerce #'intern-soft
    :action #'command-execute)
  "A simple helm source for Emacs commands.")

(defvar helm-source-emacs-commands-history
  (helm-build-sync-source "Emacs commands history"
    :candidates
    (lambda ()
      (let ((cmds))
        (dolist (elem extended-command-history)
          (push (intern elem) cmds))
        cmds))
    :coerce #'intern-soft
    :action #'command-execute)
  "Emacs commands history")
