(use-package xah-lookup
  :straight t
  :after (hydra)
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
  (put 'xah-lookup-wikipedia 'xah-lookup-browser-function 'eww)
  (defhydra hydra-lookup
    (global-map "C-c l"
                :color blue)
    "Lookup"
    ("t" google-translate-enen-or-jaen)
    ("g" lookup-google)
    ("w" lookup-wikipedia-ja)
    ("W" lookup-wikipedia-en)
    ("a" lookup-amazon "amazon.co.jp")
    ("d" lookup-duckduckgo "duckduckgo")
    ("e" lookup-eijiro "eijiro")
    ("l" lookup-weblio "weblio")
    ("y" lookup-youtube "youtube")
    ("q" nil "quit")))

(defun lookup-word-on-internet (url &optional word)
  "Lookup WORD on URL."
  (let* ((raw-word (cond
                    (current-prefix-arg
                     (read-string "Words to lookup: "))
                    ((stringp word) word)
                    ((use-region-p)
                     (buffer-substring (region-beginning) (region-end)))
                    ((stringp (word-at-point)) (word-at-point))
                    (t "")))
         (word-to-lookup (replace-regexp-in-string
                          "\n"
                          (if (string-match-p "[^[:ascii:]]" raw-word)
                              "" " ")
                          raw-word)))
    (kill-new word-to-lookup)
    (advice-add 'eww :around #'open-in-new-buffer)
    (xah-lookup-word-on-internet word-to-lookup url)
    (advice-remove 'eww #'open-in-new-buffer)))

(defun lookup-google ()       "Lookup on google." (interactive) (lookup-word-on-internet "http://www.google.com/search?q=word02051"))
(defun lookup-wikipedia-en () "Lookup on wikipedia(en)." (interactive) (lookup-word-on-internet "http://en.wikipedia.org/wiki/word02051"))
(defun lookup-wikipedia-ja () "Lookup on wikipedia(ja)." (interactive) (lookup-word-on-internet "http://ja.wikipedia.org/wiki/word02051"))
(defun lookup-amazon ()       "Lookup on amazon." (interactive) (lookup-word-on-internet "http://www.amazon.co.jp/gp/search/?field-keywords=word02051"))
(defun lookup-duckduckgo ()   "Lookup on dockduckgo." (interactive) (lookup-word-on-internet "https://duckduckgo.com/html/?kl=jp-jp&k1=-1&kc=1&kf=-1&q=word02051"))
(defun lookup-eijiro ()       "Lookup on eijiro." (interactive) (lookup-word-on-internet "http://eow.alc.co.jp/search?q=word02051&ref=sa"))
(defun lookup-weblio ()       "Lookup on weblio." (interactive) (lookup-word-on-internet "http://www.weblio.jp/content/word02051"))
(defun lookup-youtube ()      "Lookup on youtube." (interactive) (lookup-word-on-internet "https://www.youtube.com/results?search_query=word02051"))
