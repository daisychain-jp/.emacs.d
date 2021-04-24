(cond
 ((string= window-system "x")
  (setf x-alt-keysym  'alt
        x-meta-keysym 'meta))
 ((string= window-system "ns")
  ;; IME inline patch
  (setf mac-use-input-method-on-system nil)
  (setf mac-control-modifier       'control
        mac-command-modifier       'meta
        mac-option-modifier        'super
        mac-right-option-modifier  'alt
        mac-right-control-modifier 'super
        mac-function-modifier      'hyper)))
