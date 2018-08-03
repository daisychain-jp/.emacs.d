(el-get-bundle ddskk)

;; HACK: straight.el does not support arbitrary build command (make) yet.
;;       this section will be activated when this feature is implemented.
;; (use-package ddskk
;;   :straight t
;;   :defer t
;;   :bind (("C-x C-j" . skk-mode)
;;          ("C-x j" . skk-mode)))

(require 'skk-autoloads)

(bind-keys ("C-x C-j" . skk-mode)
           ("C-x j" . skk-mode))

(setq default-input-method "japanese-skk")

(setq skk-kakutei-key (kbd "C-i"))
(setq skk-byte-compile-init-file t)

;; tutorial
(setq skk-tut-file (concat el-get-dir "/ddskk/etc/SKK.tut"))

;; cursor color
(setq skk-use-color-cursor t)
(setq skk-cursor-hiragana-color "orange")
(setq skk-cursor-katakana-color "SpringGreen3")
(setq skk-cursor-latin-color "DodgerBlue3")
(setq skk-cursor-jisx0201-color "purple3")

;; mode line string
(setq skk-latin-mode-string "A")
(setq skk-hiragana-mode-string "ひ")
(setq skk-katakana-mode-string "カ")
(setq skk-jisx0201-mode-string "ｶ")
(setq skk-jisx0208-latin-mode-string "Ａ")

;; AZIK
(setq skk-use-azik t)
(setq skk-azik-keyboard-type 'us101)
(add-hook 'skk-azik-load-hook
          (lambda ()
            (setq skk-rom-kana-rule-list
                  (append skk-rom-kana-rule-list
                          '(("xka" nil ("ヵ" . "ヵ"))
                            ("xke" nil ("ヶ" . "ヶ"))
                            ("n" nil nil)
                            ("nn" nil ("ナノ" . "なの"))
                            ("nm" nil ("ノミ" . "のみ"))
                            ("ks" nil ("コソ" . "こそ"))
                            ("ym" nil ("ヤマ" . "やま"))
                            ("tga" nil ("タガ" . "たが"))
                            ("vj" nil ("ヴン" . "ぶん")))))
            (dolist (key '("kA" "kE" "tU" "wA"))
              (setq skk-rom-kana-rule-list
                    (skk-del-alist key skk-rom-kana-rule-list)))))

;; conversion
(setq skk-egg-like-newline t)
(setq skk-henkan-strict-okuri-precedence t)
(setq skk-check-okurigana-on-touroku t)
(setq skk-show-candidates-always-pop-to-buffer t)
(setq skk-henkan-show-candidates-rows 2)
(setq skk-show-annotation t)
;; C-q for hankaku-kana input mode
(setq skk-use-jisx0201-input-method t)

;; dynamic conversion
(setq skk-dcomp-activate t)
(setq skk-dcomp-multiple-activate nil)

; jisyo setting

(setq skk-jisyo-dir (concat env-data-dir "/skk"))
(setq skk-var-dir (concat env-var-dir "/lib/skk"))

(setq skk-server-host "localhost")
(setq skk-server-portnum nil)

;; copy user jisyo data from master
(add-hook 'skk-load-hook
          (lambda ()
            (copy-file (concat skk-jisyo-dir "/skk-jisyo.euc-jp")
                       (concat skk-jisyo-dir "/skk-jisyo-ddskk.euc-jp") t)))

;; variable definition
(setq skk-get-jisyo-directory
      (concat skk-var-dir "/get-jisyo"))
(setq skk-user-jisyo-directory
      (concat skk-var-dir "/user-jisyo"))
(setq skk-record-file (concat skk-var-dir "/record"))
(setq skk-emacs-id-file (concat skk-var-dir "/emacs-id"))

;; local jisyo file
(setq skk-jisyo (concat skk-jisyo-dir "/skk-jisyo-ddskk.euc-jp"))
(setq skk-backup-jisyo (concat skk-jisyo-dir "/skk-jisyo.bak"))
(setq skk-large-jisyo (concat skk-get-jisyo-directory "/SKK-JISYO.L"))
(setq skk-extra-jisyo-file-list
      (append
       (file-expand-wildcards (concat skk-get-jisyo-directory "/SKK-JISYO.*"))
       (file-expand-wildcards (concat skk-user-jisyo-directory "/SKK-JISYO.*"))))

;; study
(require 'skk-study)
(setq skk-study-file (concat skk-var-dir "/study"))
(setq skk-study-backup-file (concat skk-var-dir "/study.bak"))

;; jisyo settings
(setq skk-share-private-jisyo t)
(setq skk-save-jisyo-instantly t)
