(unless (fboundp 'system-alarm)
  (defun system-alarm () nil))

(unless (boundp 'env-temp-dir)
  (defvar env-temp-dir "/tmp/" "Directory for temporary files"))
