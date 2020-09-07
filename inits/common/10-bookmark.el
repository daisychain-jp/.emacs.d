(use-package bookmark
  :custom
  (bookmark-default-file (concat (directory-file-name user-emacs-directory) "/.bookmarks.gpg"))
  (bookmark-watch-bookmark-file 'silent)
  :config
  (delight 'bookmark-bmenu-mode " BB"))
