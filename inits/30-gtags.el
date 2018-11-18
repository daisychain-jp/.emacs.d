(use-package ggtags
  :straight t
  :diminish ((ggtags-mode . "gg"))
  :custom
  (ggtags-executable-directory env-local-usr-bin-dir)
  (ggtags-extra-args `("--gtagsconf" ,(format "%s/share/gtags/gtags.conf" env-local-usr-dir) "--gtagslabel=pygments")))
