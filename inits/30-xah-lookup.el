(use-package xah-lookup
  :straight t
  :config
  (require 'eww)
  (setq xah-lookup-browser-function 'eww)
  (setq xah-lookup-dictionary-browser-function 'eww)
  (setq
   xah-lookup-dictionary-list
   [
    "http://www.google.com/search?q=define:+word02051"
    ] )
  ;; customize existing funcs
  (put 'xah-lookup-google 'xah-lookup-browser-function 'eww)
  (put 'xah-lookup-wikipedia 'xah-lookup-url "http://ja.wikipedia.org/wiki/word02051")
  (put 'xah-lookup-wikipedia 'xah-lookup-browser-function 'eww))

(defun xah-lookup-duckduckgo (&optional *word)
  (interactive)
  (xah-lookup-word-on-internet
   *word
   "https://duckduckgo.com/html/?kl=jp-jp&k1=-1&kc=1&kf=-1&q=word02051"))
(defun xah-lookup-eijiro (&optional *word)
  "Lookup definition of current word or text selection in URL `http://eow.alc.co.jp'"
  (interactive)
  (xah-lookup-word-on-internet
   *word
   "http://eow.alc.co.jp/search?q=word02051&ref=sa"))
(defun xah-lookup-weblio (&optional *word)
  "Lookup definition of current word or text selection in URL `http://www.weblio.jp/'"
  (interactive)
  (xah-lookup-word-on-internet
   *word
   "http://www.weblio.jp/content/word02051"))
(defun xah-lookup-youtube (&optional *word)
  (interactive)
  (xah-lookup-word-on-internet
   *word
   "https://www.youtube.com/results?search_query=word02051"))
(defun xah-lookup-amazon (&optional *word)
  (interactive)
  (xah-lookup-word-on-internet
   *word
   "http://www.amazon.co.jp/gp/search/?field-keywords=word02051"))
