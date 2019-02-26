(defvar env-local-usr-dir (expand-file-name "~/usr") "Local bin directory of Unix System Resource.")
(defvar env-local-usr-bin-dir (format "%s/bin" env-local-usr-dir))

(defvar env-mew-prog-ssl "/usr/bin/stunnel")
(defvar env-mew-prog-est "/usr/bin/estcmd")
(defvar env-mu4e-mu-binary (format "%s/bin/mu" env-local-usr-dir))
(defvar env-epdfinfo-prog "/usr/bin/epdfinfo")
