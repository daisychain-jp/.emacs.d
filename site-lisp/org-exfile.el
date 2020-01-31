;;; org-exfile.el --- org link type for files at removable directory -*- lexical-binding: t -*-

;; Copyright (C) 2020  Takayuki inamori

;; Author: Takayuki inamori
;; Maintainer: Takayuki inamori
;; Version: 0.1.0
;; Package-Requires: (org)
;; Homepage: https://github.com/daisychain-jp
;; Keywords: org-link


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; commentary

;;; Code:

(require 'org)

(defgroup org-exfile nil
  "Customization group for org-exfile."
  :prefix "org-exfile-"
  :group 'external)

(defcustom org-exfile-org-open-file-custom-function nil
  "A function which `org-exfile-open' use internally to open file.
If nil `org-open-file' would be used instead."
  :group 'org-exfile)

(defcustom org-exfile-dirs-alist nil
  "Association list holds base directory paths with labels."
  :group 'org-exfile
  :type 'alist)

;; Install the link type
(org-link-set-parameters "exfile"
                         :follow #'org-exfile-open
                         :complete #'org-exfile-complete-link
                         :export #'org-exfile-export-link)

(org-link-set-parameters "exfile+emacs"
                         :follow (lambda (path)
                                   (org-exfile-open path 'emacs))
                         :complete (lambda ()
                                     (org-exfile-complete-link nil t))
                         :export #'org-exfile-export-link)

(org-link-set-parameters "exfile+sys"
                         :follow (lambda (path)
                                   (org-exfile-open path 'system))
                         :complete (lambda ()
                                     (org-exfile-complete-link nil 'system))
                         :export #'org-exfile-export-link)

(defun org-exfile-dir (label)
  "Return directory path associated with `LABEL'."
  (let ((cons-dir (assoc label org-exfile-dirs-alist)))
    (if (consp cons-dir)
        (let ((abs-dir (expand-file-name (cdr cons-dir))))
          (if (file-directory-p abs-dir)
              (file-name-as-directory abs-dir)
            nil))
      nil)))

(defun org-exfile-expand-link (link)
  "docstring"
  (when (string-match "^\\([^:]+\\):\\(\\(?:.+/\\)*[^/]*\\)$" link)
    (concat (org-exfile-dir (match-string 1 link))
            (match-string 2 link))))

(defun org-exfile-open (path &optional in-emacs)
  "Open file described as `PATH' in exfile org link type.

If optional argument `IN-EMACS' is `C-u' or 'emacs, try to open in Emacs.
If `C-u C-u' or 'system, try to open in external application.

When `org-exfile-org-open-file-custom-function' is set, use it instead of org-open-file."
  (let ((abs-path (org-exfile-expand-link path))
        line-search)
    (cond
     ((string-match "::\\([0-9]+\\)\\'" abs-path)
      (setq line-search (list (string-to-number (match-string 1 abs-path))))
      (setq abs-path (substring abs-path 0 (match-beginning 0))))
     ((string-match "::\\(.+\\)\\'" abs-path)
      (setq line-search (list nil (match-string 1 abs-path)))
      (setq abs-path (substring abs-path 0 (match-beginning 0)))))
    (apply (or org-exfile-org-open-file-custom-function
               #'org-open-file)
           abs-path
           in-emacs
           line-search)))

(defun org-exfile-complete-link (&optional arg in-emacs)
  "Create a exfile link using completion"
  (let* ((loc-label (completing-read "Location Label: " (mapcar #'car org-exfile-dirs-alist)))
         (loc (cdr (assoc loc-label org-exfile-dirs-alist)))
         (fpath (read-file-name "Target file/directory: " loc loc)))
    (format "%s:%s:%s"
            (concat "exfile"
                    (cond
                     ((member in-emacs '((16) system))
                      "+sys")
                     (in-emacs "+emacs")))
            loc-label (file-relative-name fpath loc))))

(defun org-exfile-store-link-cp (&optional arg)
  "Copy file/dir to exfile dir and store a link to it."
  (interactive "P")
  (let* ((file (expand-file-name (read-file-name "File: ")))
         (label (completing-read "Target dir label: " (mapcar #'car org-exfile-dirs-alist)))
         (base-dir (org-exfile-dir label))
         (target-dir (read-directory-name "Copy destination:" base-dir))
         (dest-file nil))
    (case (car arg)
      (4 (if (file-directory-p file)
             (call-process-shell-command
              (format "tar -c -C %s %s | gpg -e --default-recipient-self -o %s"
                      (file-name-directory (directory-file-name file))
                      (file-name-nondirectory (directory-file-name file))
                      (setf dest-file (concat (file-name-as-directory target-dir)
                                              (file-name-nondirectory (directory-file-name file))
                                              ".tar.gpg"))))
           (call-process-shell-command
            (format "gpg -e --default-recipient-self -o \"%s\" \"%s\""
                    (setf dest-file (concat (file-name-as-directory target-dir)
                                            (file-name-nondirectory file)
                                            ".gpg"))
                    file))))
      (t (if (file-directory-p file)
             (copy-directory file (setf dest-file (concat (file-name-as-directory target-dir)
                                                          (file-name-nondirectory (directory-file-name file)))))
           (copy-file file (setf dest-file (concat (file-name-as-directory target-dir)
                                                   (file-name-nondirectory file)))))))
    (setq org-stored-links
          (cons (list (format "exfile:%s:%s" label (file-relative-name dest-file base-dir))
                      (file-name-nondirectory dest-file))
                org-stored-links))))

(defun org-exfile-export-link (link description format)
  "Translate exfile LINK from Org mode format to exported FORMAT.
Also includes the DESCRIPTION of the link in the export."
  (save-excursion
    (let* ((abs-path (org-exfile-expand-link link))
           desc)
      (print abs-path)
      (cond
       ((string-match "::\\([0-9]+\\)\\'" abs-path)
        (setq abs-path (substring abs-path 0 (match-beginning 0))))
       ((string-match "::\\(.+\\)\\'" abs-path)
        (setq abs-path (substring abs-path 0 (match-beginning 0)))))
      (setq desc (or description link))
      (pcase format
        (`html (format "<a target=\"_blank\" href=\"%s\">%s</a>" abs-path desc))
        (`latex (format "\\href{%s}{%s}" abs-path desc))
        (`texinfo (format "@uref{%s,%s}" abs-path desc))
        (`ascii (format "%s (%s)" desc abs-path))
        (`md (format "[%s](%s)" desc abs-path))
        (_ path)))))

(defun org-exfile-detect-broken-link ()
  )


(provide 'org-exfile)
;;; org-exfile.el ends here
