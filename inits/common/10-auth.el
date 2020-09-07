(use-package auth-source
  :custom
  (auth-source-gpg-encrypt-to user-mail-address)
  (auth-sources '("~/.authinfo.gpg" "~/.netrc.gpg")))

(use-package auth-source-pass
  :config
  (auth-source-pass-enable))
