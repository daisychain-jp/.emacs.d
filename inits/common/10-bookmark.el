(use-package bookmark
  :custom
  (bookmark-default-file
   (expand-file-name ".bookmarks.gpg" user-emacs-directory))
  (bookmark-watch-bookmark-file 'silent)
  :config
  (delight 'bookmark-bmenu-mode " BB"))
