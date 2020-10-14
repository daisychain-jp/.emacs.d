(use-package ddskk
  :straight t
  :defer t
  :bind (("C-x C-j" . skk-mode)
         ("C-x j" . skk-mode)
         ("C-c j" . skk-mode))
  :custom
  (default-input-method "japanese-skk")
  (skk-kakutei-key (kbd "C-m"))
  (skk-user-directory (expand-file-name "ddskk" user-emacs-directory))
  (skk-byte-compile-init-file t)
  (skk-tut-file
   (expand-file-name "etc/SKK.tut" (straight--repos-dir "ddskk")))
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
  (skk-record-file (expand-file-name "record" skk-data-dir))
  (skk-emacs-id-file (expand-file-name "emacs-id" skk-data-dir))
  ;; jisyo
  (skk-share-private-jisyo t)
  (skk-compare-jisyo-size-when-saving nil)
  (skk-save-jisyo-instantly t)
  ;; jisyo file/directory
  (skk-jisyo (expand-file-name "jisyo" skk-data-dir))
  (skk-backup-jisyo (expand-file-name "jisyo.bak" skk-data-dir))
  (skk-large-jisyo (expand-file-name "large-jisyo" skk-data-dir))
  ;; jisyo server
  (skk-server-host "localhost")
  (skk-server-portnum 1178)
  (skk-server-inhibit-startup-server t)
  ;; study
  (skk-study-file (expand-file-name "study" skk-data-dir))
  (skk-study-backup-file (expand-file-name "study.bak" skk-data-dir)))
