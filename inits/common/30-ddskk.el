(use-package ddskk
  :straight t
  :defer t
  :bind (("C-x C-j" . skk-mode)
         ("C-x j" . skk-mode)
         ("C-c j" . skk-mode))
  :custom
  (default-input-method "japanese-skk")
  (skk-kakutei-key (kbd "C-m"))
  (skk-user-directory (format "%s/ddskk" user-emacs-directory))
  (skk-byte-compile-init-file t)
  (skk-tut-file (concat el-get-dir "/ddskk/etc/SKK.tut"))
  ;; cursor color
  (skk-use-color-cursor t)
  (skk-cursor-hiragana-color "orange")
  (skk-cursor-katakana-color "SpringGreen3")
  (skk-cursor-latin-color "DodgerBlue3")
  (skk-cursor-jisx0201-color "purple3")
  ;; mode line string
  (skk-latin-mode-string "A")
  (skk-hiragana-mode-string "あ")
  (skk-katakana-mode-string "ア")
  (skk-jisx0201-mode-string "ｱ")
  (skk-jisx0208-latin-mode-string "Ａ")
  ;; AZIK
  (skk-use-azik t)
  (skk-azik-keyboard-type 'us101)
  ;; conversion
  (skk-egg-like-newline t)
  (skk-henkan-strict-okuri-precedence t)
  (skk-check-okurigana-on-touroku t)
  (skk-show-candidates-always-pop-to-buffer t)
  (skk-henkan-number-to-display-candidates 10)
  (skk-show-annotation nil)
  ;; C-q for hankaku-kana input mode
  (skk-use-jisx0201-input-method t)
  ;; dynamic conversion
  (skk-dcomp-activate nil)
  (skk-dcomp-multiple-activate nil)
  ;; config file
  (skk-record-file (format "%s/record" skk-data-dir))
  (skk-emacs-id-file (format "%s/emacs-id" skk-data-dir))
  ;; jisyo
  (skk-share-private-jisyo t)
  (skk-save-jisyo-instantly t)
  ;; jisyo file/directory
  (skk-get-jisyo-directory (format "%s/get-jisyo" skk-data-dir))
  (skk-user-jisyo-directory (format "%s/user-jisyo" skk-data-dir))
  (skk-jisyo (format "%s/private-jisyo/skk-jisyo.euc-jp" skk-data-dir))
  (skk-backup-jisyo (format "%s/private-jisyo/skk-jisyo.bak" skk-data-dir))
  (skk-large-jisyo (concat skk-get-jisyo-directory "/SKK-JISYO.L"))
  (skk-extra-jisyo-file-list
   (append
    (file-expand-wildcards (concat skk-get-jisyo-directory "/SKK-JISYO.*"))
    (file-expand-wildcards (concat skk-user-jisyo-directory "/SKK-JISYO.*"))))
  ;; jisyo server
  (skk-server-host "localhost")
  (skk-server-portnum nil)
  :config
  (require 'skk-autoloads)
  (add-hook 'skk-azik-load-hook
            (lambda ()
              (dolist (key '("kA" "kE" "tU" "wA"))
                (setq skk-rom-kana-rule-list
                      (skk-del-alist key skk-rom-kana-rule-list)))))
  ;; study
  (require 'skk-study)
  (setf skk-study-file (format "%s/study" skk-data-dir))
  (setf skk-study-backup-file (format "%s/study.bak" skk-data-dir)))
