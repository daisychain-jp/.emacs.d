(use-package abbrev
  :diminish abbrev-mode
  :custom
  (save-abbrevs t)
  :config
  (quietly-read-abbrev-file))

(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill))
(global-set-key (kbd "M-/") 'hippie-expand)
