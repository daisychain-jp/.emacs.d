(require 'time-stamp)

(add-hook 'before-save-hook 'time-stamp)
(setq time-stamp-active t)
(setq time-stamp-start "[lL]ast[ -][uU]pdated[ \t]*:[ \t]*<")
(setq time-stamp-format "%:y-%02m-%02d %02H:%02M")
(setq time-stamp-end ">")
(setq time-stamp-line-limit 10)
