(diminish 'abbrev-mode)
(setq abbrev-file-name
      (concat env-emacs-data-dir "/abbrev_defs"))
(setq save-abbrevs t)
(quietly-read-abbrev-file)

(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill))
(global-set-key (kbd "M-/") 'hippie-expand)
