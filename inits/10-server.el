(use-package server
  :config
  ;; start server for emacsclient
  (unless (server-running-p)
    (server-start))
  )
