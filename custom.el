(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(avy-migemo-function-names
   (quote
    ((counsel-clj :around avy-migemo-disable-around)
     (counsel-grep :around counsel-grep-migemo-around)
     counsel-grep-function-migemo counsel-grep-occur-migemo
     (counsel-git-occur :around counsel-git-occur-migemo-around)
     (counsel-find-file-occur :around counsel-find-file-occur-migemo-around)
     swiper--make-overlays-migemo
     (swiper--re-builder :around swiper--re-builder-migemo-around)
     (ivy--regex :around ivy--regex-migemo-around)
     (ivy--regex-ignore-order :around ivy--regex-ignore-order-migemo-around)
     (ivy--regex-plus :around ivy--regex-plus-migemo-around)
     ivy--highlight-default-migemo ivy-occur-revert-buffer-migemo ivy-occur-press-migemo avy-migemo-goto-char avy-migemo-goto-char-2 avy-migemo-goto-char-in-line avy-migemo-goto-char-timer avy-migemo-goto-subword-1 avy-migemo-goto-word-1 avy-migemo-isearch avy-migemo-org-goto-heading-timer avy-migemo--overlay-at avy-migemo--overlay-at-full)))
 '(enable-remote-dir-locals t)
 '(org-plantuml-jar-path (format "%s/lib/plantuml/plantuml.jar" env-var-dir))
 '(send-mail-function (quote smtpmail-send-it))
 '(shackle-default-rule (quote (:same t)))
 '(smtpmail-default-smtp-server "smtp.daisychain.jp"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
