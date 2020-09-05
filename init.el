;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;; multilingual
(set-language-environment "Japanese")
(set-buffer-file-coding-system 'utf-8-unix)
(set-file-name-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-clipboard-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(setq locale-coding-system 'utf-8-unix)
(setq system-time-locale "C")

(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))
(setq package-enable-at-startup nil)
(package-initialize t)

;; set user emacs directory
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

;; switch default package directory to "package/xx.x.x/(elpa|el-get)"
(let ((pkg-emacs-ver-dir
       (locate-user-emacs-file
        (concat "package/" emacs-version))))
  (add-to-list 'load-path (setq el-get-dir (expand-file-name "el-get" pkg-emacs-ver-dir)))
  (add-to-list 'load-path (setq package-user-dir (expand-file-name "elpa" pkg-emacs-ver-dir)))
  (add-to-list 'load-path (expand-file-name "elisp" pkg-emacs-ver-dir)))

;; Set path for source code of 3rd party packages
(mapc (lambda (new-path)
        (when (file-directory-p new-path)
          (add-to-list 'load-path new-path)))
      (append
       (list (concat (file-name-as-directory user-emacs-directory) "site-lisp" ))
       (file-expand-wildcards
        (format "%s/site-lisp/*" user-emacs-directory))))

;; install El-Get by using package system
(add-to-list 'load-path (concat el-get-dir "/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;; install straight.el
(setq straight-recipes-gnu-elpa-use-mirror t)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 4))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; install use-package via straight.el
(straight-use-package 'use-package)

;; install dependency packages for use-package using straight.el
(use-package diminish :straight t)
(use-package delight :straight t)
(use-package bind-key :straight t)
(use-package use-package-ensure-system-package :straight t)

(let ((coding-system-for-write 'utf-8))
  (mapc (lambda (dir-name)
          (mapc #'load-file
                (directory-files
                 (format "%s/inits/%s" user-emacs-directory dir-name)
                 t ".+\\.el\\(\\.gpg\\)?" nil)))
        `("default"
          ,(cond
            ((string= system-type "gnu/linux") "linux")
            ((string= system-type "darwin") "darwin"))
          "common" "personal")))
