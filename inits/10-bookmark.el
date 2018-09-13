(use-package bookmark
  :custom
  (bookmark-default-file
   (concat env-emacs-data-dir "/bookmarks"))
  :config
  (delight 'bookmark-bmenu-mode " BB"))
