;; override play-sound to play various type of sound file
(defun play-sound (sound)
  (apply 'call-process
         `(,(car play-sound-external-command) nil nil nil
           ,@(cdr play-sound-external-command)
           ,(file-truename (plist-get (cdr sound) :file)))))
