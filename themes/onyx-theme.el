;;; onyx-theme.el --- An extreme high contrast color theme

;; Copyright (C) 2016 Takayuki INAMORI <t.inamori@daisychain.jp>

;; Author: Takayuki INAMORI <t.inamori@daisychain.jp>
;; URL: https://github.com/daisychain-jp/onyx-theme
;; Version: 1.0.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This theme is extremely high contrast mainly for the low vision
;;
;; For details and screenshots visit the project page on Github:
;;
;;     https://github.com/daisychain-jp/onyx-theme
;;
;; To use this theme put the following in your startup file:
;;
;;     (load-theme 'onyx t)
;;

;;; Install:

;; Put this file on your Emacs-Lisp load path and add the following in
;; your Emacs startup file:
;;
;;     (load-theme 'onyx t)

;;; Code:

(deftheme onyx "Onix color theme")

(let ((class '((class color) (min-colors 89)))
      ;; Onyx palette
      (onyx-fg       "#dcdccc")
      (onyx-bg-2     "#111111")
      (onyx-bg-1     "#2b2b2b")
      (onyx-bg-05    "#383838")
      (onyx-bg       "#000000")
      (onyx-bg+1     "#4f4f4f")
      (onyx-bg+2     "#5f5f5f")
      (onyx-bg+3     "#6f6f6f")
      (onyx-red+1    "#dca3a3")
      (onyx-red      "#ff0000")
      (onyx-red-1    "#8b0000")
      (onyx-red-2    "#8b0000")
      (onyx-red-3    "#9c6363")
      (onyx-red-4    "#8c5353")
      (onyx-red-5    "#7F073F")
      (onyx-pink     "#ff69b4")
      (onyx-pink-1   "#ff1493")
      (onyx-pink-2   "#cd1076")
      (onyx-orange-2 "#FF6400")
      (onyx-orange-1 "#ff8c00") ;; DarkOrange
      (onyx-orange   "#ffa500")
      (onyx-yellow   "#ffff00")
      (onyx-yellow-1 "#FBDE2D")
      (onyx-yellow-2 "#d0bf8f")
      (onyx-yellow-3 "#D8FA3C")
      (onyx-yellow-4 "#E9C062")
      (onyx-yellow-5 "#ffd700")
      (onyx-green-2  "#006400")
      (onyx-green-1  "#2e8b57")
      (onyx-green    "#00ff00")
      (onyx-green+1  "#61CE3C")
      (onyx-green+2  "#9fc59f")
      (onyx-green+3  "#afd8af")
      (onyx-green+4  "#bfebbf")
      (onyx-cyan     "#93e0e3")
      (onyx-sky-blue-1 "#87ceeb")
      (onyx-deep-sky-blue-1 "#00bfff")
      (onyx-slate-blue "#6a5acd")
      (onyx-blue+1   "#94bff3")
      (onyx-blue     "#0000ff")    ;; blue
      (onyx-blue-1   "#7b68ee")  ;; medium slate blue
      (onyx-blue-2   "#6a5acd")  ;; slate blue
      (onyx-blue-3   "#add8e6")  ;; light blue
      (onyx-blue-4   "#b2dfee")  ;; LightBlue2
      (onyx-blue-5   "#4c83ff")
      (onyx-blue-6   "#96CBFE")
      (onyx-blue-7   "#00ffff")
      (onyx-blue-8   "#4F94CD")
      (onyx-midnight-blue   "#191970")
      (onyx-magenta  "#dc8cc3")
      (onyx-black    "#000000")
      (onyx-black-2  "#0C1021")
      (onyx-black-3  "#0A0A0A")
      (onyx-gray     "#d3d3d3")
      (onyx-gray-2   "#8B8989")
      (onyx-gray-3   "#919191")
      (onyx-darkgray "#4D4D4D")
      (onyx-white    "#ffffff")
      (onyx-white-2  "#F8F8F8")
      (onyx-white-3  "#fffafa"))

  (custom-theme-set-faces
   'onyx
   ;;; basic coloring
   `(default ((,class (:foreground ,onyx-gray :background ,onyx-black))))
   '(cursor ((t (:foreground "blue4"))))

   ;; highlight
   `(region              ((,class (:background ,onyx-midnight-blue))))
   `(secondary-selection ((,class (:background ,onyx-blue))))
   `(highlight           ((,class (:foreground ,onyx-yellow-3 :background ,onyx-black :box (:line-width 1 :color ,onyx-yellow-3 :style released-button)))))

   ;; mode line
   '(mode-line ((t (:foreground "gray95" :background "gray10"
                                :box (:line-width 1 :color "#000000" :style released-button)))))
   '(mode-line-buffer-id ((t (:foreground nil :background nil))))
   '(mode-line-inactive ((t (:foreground "#BCBCBC" :background "#333333"
                                         :box (:line-width 1 :color "#333333")))))
   ; defined in local
   `(mode-line-modified ((,class (:foreground ,onyx-orange))))
   `(mode-line-read-only ((,class (:foreground nil :underline t))))
   `(mode-line-read-only-modified ((,class (:inherit 'mode-line-read-only :foreground ,onyx-orange))))

   ;; highlight
   '(highlight ((t (:foreground "#000000" :background "#C4BE89"))))
   '(hl-line ((t (:background "gray7"))))

   ;;; font lock
   `(font-lock-builtin-face              ((,class (:foreground ,onyx-blue-5))))
   `(font-lock-comment-face              ((,class (:foreground ,onyx-gray-2 :italic t))))
   `(font-lock-comment-delimiter-face    ((,class (:foreground ,onyx-gray-3))))
   `(font-lock-constant-face             ((,class (:foreground ,onyx-blue-6))))
   `(font-lock-doc-face                  ((,class (:foreground ,onyx-yellow-1))))
   `(font-lock-function-name-face        ((,class (:foreground ,onyx-orange-1))))
   `(font-lock-keyword-face              ((,class (:foreground ,onyx-deep-sky-blue-1))))
   `(font-lock-negation-char-face        ((,class (:foreground ,onyx-fg))))
   `(font-lock-preprocessor-face         ((,class (:foreground ,onyx-gray-3))))
   `(font-lock-string-face               ((,class (:foreground ,onyx-green+1))))
   `(font-lock-type-face                 ((,class (:foreground ,onyx-green+3))))
   `(font-lock-variable-name-face        ((,class (:foreground ,onyx-pink))))
   `(font-lock-warning-face              ((,class (:foreground ,onyx-pink))))
   `(font-lock-reference-face            ((,class (:foreground ,onyx-gray))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,onyx-yellow-4))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,onyx-red))))

   ;; parens
   '(show-paren-match-face ((t (:background "orange" :foreground "white"))))
   '(paren-face ((t (:foreground "#A6E22A" :background nil))))

   ;; dired
   '(dired-directory ((t (:foreground "#A6E22A"))))
   '(dired-symlink ((t (:foreground "purple1"))))
   '(dired-flagged ((t (:background "red" :foreground "#eee" :underline t :weight bold))))
   '(dired-marked ((t (:underline t :weight bold))))

   ;; isearch
   `(isearch        ((,class (:foreground "white" :background "red4"))))
   `(isearch-fail   ((,class (:foreground "yellow" :background "red4"))))
   `(lazy-highlight ((,class (:background "gray20" :box (:line-width 2 :style none)))))

   ;; avy
   `(avy-lead-face   ((,class (:foreground "black" :background "yellow3"))))
   `(avy-lead-face-0 ((,class (:foreground "black" :background "red2"))))

   ;; smartparens
   `(sp-show-pair-match-face ((,class (:background "gray15" :box (:line-width 1 :color "green1" :style none)))))
   '(sp-pair-overlay-face
     ((t
       (:inherit nil
                 :background nil
                 :foreground "#7cfc00"))))
   '(sp-wrap-overlay-face
     ((t
       (:inherit nil
                 :background nil
                 :foreground "#ff4500"))))
   '(sp-wrap-tag-overlay-face
     ((t
       (:inherit nil
                 :background nil
                 :foreground "#ff1493"))))
   '(sp-show-pair-enclosing
     ((t
       (:inherit nil
                 :foreground "#000000"
                 :background "#ff6347"))))

   ;; helm
   `(helm-header           ((,class (:foreground ,onyx-green :background ,onyx-bg :underline nil :box nil))))
   `(helm-source-header    ((,class (:foreground ,onyx-yellow :background ,onyx-bg-1 :underline nil :weight bold :box (:line-width -1 :style released-button)))))
   `(helm-selection        ((,class (:background ,onyx-bg+1 :underline nil))))
   `(helm-selection-line   ((,class (:background ,onyx-bg+1))))
   `(helm-visible-mark     ((,class (:foreground ,onyx-bg :background ,onyx-yellow-2))))
   `(helm-candidate-number ((,class (:foreground ,onyx-green+4 :background ,onyx-bg-1))))

   ;; org-mode
   `(org-document-title        ((,class (:foreground ,onyx-blue-3 :background ,onyx-black :weight bold))))
   `(org-document-info         ((,class (:foreground ,onyx-blue-3 :background ,onyx-black :weight bold))))
   `(org-document-info-keyword ((,class (:foreground ,onyx-gray-2 :background ,onyx-black))))
   `(org-agenda-date-today     ((,class (:foreground ,onyx-orange-2 :slant italic :weight bold))) t)
   `(org-agenda-structure      ((,class (:inherit font-lock-comment-face))))
   `(org-archived              ((,class (:foreground ,onyx-gray-2 :slant italic))))
   `(org-checkbox              ((,class (:background ,onyx-black :foreground ,onyx-yellow))))
   `(org-date                  ((,class (:foreground ,onyx-blue-7 :underline t))))
   `(org-warning               ((,class (:bold t :weight bold :box (:line-width 2 :style none)))))
   `(org-todo                  ((,class (:bold t :weight bold))))
   `(org-done                  ((,class (:underline t))))
   `(org-level-1               ((,class (:foreground ,onyx-blue-7 :height 1.0))))
   `(org-level-2               ((,class (:foreground ,onyx-blue-6))))
   `(org-level-3               ((,class (:foreground ,onyx-white))))
   `(org-level-4               ((,class (:foreground ,onyx-white))))
   `(org-level-5               ((,class (:foreground ,onyx-white))))
   `(org-level-6               ((,class (:foreground ,onyx-white))))
   `(org-level-7               ((,class (:foreground ,onyx-white))))
   `(org-level-8               ((,class (:foreground ,onyx-white))))
   `(org-link                  ((,class (:foreground ,onyx-blue-6 :underline t))))
   `(org-tag                   ((,class (:bold t :weight bold))))
   `(org-column                ((,class (:background ,onyx-black :foreground ,onyx-white))))
   `(org-column-title          ((,class (:background ,onyx-bg-1 :underline t :weight bold))))
   `(org-block                 ((,class (:background ,onyx-bg))))
   `(org-block-begin-line      ((,class (:foreground "#008ED1" :background ,onyx-black-2))))
   `(org-block-end-line        ((,class (:foreground "#008ED1" :background ,onyx-black-2))))

   ;; `(org-deadline-announce ((,class (:foreground ,onyx-red-1))))
   ;; `(org-scheduled ((,class (:foreground ,onyx-green+4))))
   ;; `(org-scheduled-previously ((,class (:foreground ,onyx-red-4))))
   ;; `(org-scheduled-today ((,class (:foreground ,onyx-blue+1))))
   ;; `(org-special-keyword ((,class (:foreground ,onyx-yellow-1))))
   ;; `(org-table ((,class (:foreground ,onyx-green+2))))
   ;; `(org-time-grid ((,class (:foreground ,onyx-orange))))
   ;; `(org-upcoming-deadline ((,class (:inherit font-lock-keyword-face))))
   ;; `(org-warning ((,class (:bold t :foreground ,onyx-red :weight bold :underline nil))))
   ;; `(org-formula ((,class (:foreground ,onyx-yellow-2))))
   ;; `(org-headline-done ((,class (:foreground ,onyx-green+3))))
   ;; `(org-hide ((,class (:foreground ,onyx-bg-1))))

   ;;; mew
   `(mew-face-header-subject   ((,class (:foreground ,onyx-green))))
   `(mew-face-header-from      ((,class (:foreground ,onyx-yellow))))
   `(mew-face-header-date      ((,class (:foreground ,onyx-green))))
   `(mew-face-header-to        ((,class (:foreground ,onyx-red))))
   `(mew-face-header-key       ((,class (:foreground ,onyx-green))))
   `(mew-face-header-private   ((,class (:foreground ,onyx-green))))
   `(mew-face-header-important ((,class (:foreground ,onyx-blue))))
   `(mew-face-header-marginal  ((,class (:foreground ,onyx-fg :weight bold))))
   `(mew-face-header-warning   ((,class (:foreground ,onyx-red))))
   `(mew-face-header-xmew      ((,class (:foreground ,onyx-green))))
   `(mew-face-header-xmew-bad  ((,class (:foreground ,onyx-red))))
   `(mew-face-body-url         ((,class (:foreground ,onyx-orange))))
   `(mew-face-body-comment     ((,class (:foreground ,onyx-fg :slant italic))))
   `(mew-face-body-cite1       ((,class (:foreground ,onyx-green))))
   `(mew-face-body-cite2       ((,class (:foreground ,onyx-blue))))
   `(mew-face-body-cite3       ((,class (:foreground ,onyx-orange))))
   `(mew-face-body-cite4       ((,class (:foreground ,onyx-yellow))))
   `(mew-face-body-cite5       ((,class (:foreground ,onyx-red))))
   `(mew-face-mark-review      ((,class (:foreground ,onyx-blue))))
   `(mew-face-mark-escape      ((,class (:foreground ,onyx-green))))
   `(mew-face-mark-delete      ((,class (:foreground ,onyx-red))))
   `(mew-face-mark-unlink      ((,class (:foreground ,onyx-yellow))))
   `(mew-face-mark-refile      ((,class (:foreground ,onyx-green))))
   `(mew-face-mark-unread      ((,class (:foreground ,onyx-blue))))
   `(mew-face-eof-message      ((,class (:foreground ,onyx-green))))
   `(mew-face-eof-part         ((,class (:foreground ,onyx-yellow))))

   ;;; elfeed
   `(elfeed-search-title-face  ((,class (:foreground ,onyx-darkgray))))
   `(elfeed-search-unread-title-face  ((,class (:foreground ,onyx-white))))
   `(elfeed-search-unchecked-title-face  ((,class (:foreground "yellow"))))
   `(elfeed-search-checked-title-face  ((,class (:foreground "DarkGreen"))))
   )

   ;;; custom theme variables
  (custom-theme-set-variables
   'onyx
   `(ansi-color-names-vector [,onyx-bg
                              ,onyx-red-2
                              ,onyx-green
                              ,onyx-orange
                              ,onyx-blue-1
                              ,onyx-magenta
                              ,onyx-cyan
                              ,onyx-fg])
   ;; fill-column-indicator
   `(fci-rule-color ,onyx-bg-05)
   )
  )


(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'onyx)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; onyx-theme.el ends here
