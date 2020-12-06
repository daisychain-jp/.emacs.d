(use-package lilypond-mode
  :delight (LilyPond-mode " LP")
  :mode ("\\.ly$" . LilyPond-mode)
  :custom
  (LilyPond-pdf-command "firefox")
  (LilyPond-midi-command "fluidsynth -a alsa -m alsa_seq -l"))

(use-package ob-lilypond
  :after (org lilypond-mode)
  :custom
  ;; (org-babel-lilypond-commands '("lilypond" "firefox" "fluidsynth -a alsa -m alsa_seq -l"))
  (org-babel-lilypond-commands '("lilypond" "firefox" "xdg-open"))
  (org-babel-lilypond-arrange-mode t))
