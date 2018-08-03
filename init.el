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

;; install El-Get by using package system
(add-to-list 'load-path (concat el-get-dir "/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;; install essential packages first
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(use-package diminish :ensure t)
(use-package bind-key :ensure t)
(use-package use-package-ensure-system-package :ensure t)

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
(straight-use-package 'use-package)

(let ((coding-system-for-write 'utf-8))
  ;; load default .el files
  (mapc (lambda (file)
          (load-file file))
        (let ((default-dir (concat user-emacs-directory "/inits/default/")))
          (file-expand-wildcards
           (concat default-dir "*.el"))))
  ;; load .el files depends on 'system-type
  (mapc (lambda (file)
          (load-file file))
        (let ((system-type-dir (concat user-emacs-directory "/inits/system-type/"))
              (system-type-short
               (cond
                ((string= system-type "gnu/linux") "linux")
                ((string= system-type "darwin") "darwin"))))
          (file-expand-wildcards
           (concat system-type-dir system-type-short "*.el"))))
  ;; load .el files depends on 'system-name
  (mapc (lambda (file)
          (load-file file))
        (let ((system-name-dir (concat user-emacs-directory "/inits/system-name/")))
          (append
           (file-expand-wildcards
            (concat system-name-dir (system-name) "*.el"))
           (make-list 1 (concat system-name-dir "common.el")))))
  ;; load rest of all .el files
  (mapc (lambda (file)
          (load-file file))
        (let ((init-dir (concat user-emacs-directory "/inits/")))
          (sort
           (file-expand-wildcards (concat init-dir "*.el")) 'string<))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hydra-is-helpful nil)
 '(org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar")
 '(package-selected-packages
   (quote
    (avy-migemo avy use-package-ensure-system-package diminish use-package)))
 '(send-mail-function (quote smtpmail-send-it))
 '(shackle-default-rule (quote (:same t)))
 '(shackle-rules
   (quote
    (((quote undo-tree-visualizer-mode)
      :popup t)
     ((quote sql-interactive-mode)
      :popup t)
     ((quote magit-diff-mode)
      :below t)
     ("*shell*" :same t)
     ("*Help*" :same t)
     ("*Apropos*" :same t)
     ("*el-get packages*" :same t)
     ("*pry*" :same t)
     ("j-*" :regexp t :same t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
