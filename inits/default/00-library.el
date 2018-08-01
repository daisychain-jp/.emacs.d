;; Library for managing strings
;; https://github.com/magnars/s.el
(el-get-bundle s)
(use-package s)

;; Library for managing files
;; https://github.com/rejeep/f.el
(el-get-bundle f)
(use-package f)

;; Library for managing hash tables
;; https://github.com/Wilfred/ht.el
(el-get-bundle ht)
(use-package ht)

;; asynchronous processing
;; README: https://github.com/kiwanami/emacs-deferred/blob/master/README.ja.markdown
(el-get-bundle deferred)
(require 'deferred)
