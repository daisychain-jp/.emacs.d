(use-package ggtags
  :straight t
  :diminish ((ggtags-mode . "gg"))
  :custom
  (ggtags-executable-directory (expand-file-name "~/usr/bin"))
  (ggtags-extra-args `("--gtagsconf" ,(format "%s/share/gtags/gtags.conf" (expand-file-name "~/usr")) "--gtagslabel=pygments")))
