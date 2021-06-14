(use-package auth-source
  :custom
  (auth-source-gpg-encrypt-to `(,user-mail-address))
  (auth-sources '("~/.authinfo.gpg" "~/.netrc.gpg")))

(use-package auth-source-pass
  :config
  (auth-source-pass-enable))

(use-package password-store
  :straight t
  :custom
  (password-store-time-before-clipboard-restore 25))

(use-package pass
  :straight t
  :config
  (bind-keys :map pass-mode-map
             ("C-j" . pass-view)))
