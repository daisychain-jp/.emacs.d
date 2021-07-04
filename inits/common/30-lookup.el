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

(with-eval-after-load 'hydra
  (defhydra hydra-lookup (global-map "C-c l"
                                     :color blue)
    "Lookup"
    ("g" lookup-google)
    ("w" lookup-wikipedia-ja)
    ("W" lookup-wikipedia-en)
    ("a" lookup-amazon "amazon.co.jp")
    ("d" lookup-duckduckgo "duckduckgo")
    ("l" lookup-weblio "weblio")
    ("y" lookup-youtube "youtube")
    ("e" my/define-word)
    ("j" (lambda ()
           (interactive)
           (let ((define-word-default-service 'weblio))
             (my/define-word)))
     "japanese translation")
    ("J" lookup-eijiro)
    ("s" synosaurus-lookup "synosaurus-lookup")
    ("S" synosaurus-choose-and-replace "synosaurus-replace")
    ("'" utl-say)
    ("q" nil "quit")))

(defun lookup-word-on-internet (url &optional word)
  "Issue query searching WORD to URL.

If some word is selected in region, use it as WORD.
Otherwise ask user to input WORD."
  (let* ((raw-word (cond
                    ((stringp word) word)
                    ((use-region-p)
                     (buffer-substring (region-beginning) (region-end)))
                    (t
                     (read-string "Words to lookup: "))))
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
(defun lookup-eijiro ()       "Lookup on eijiro." (interactive) (lookup-word-on-internet "http://eow.alc.co.jp/search?q=word02051"))
(defun lookup-weblio ()       "Lookup on weblio." (interactive) (lookup-word-on-internet "http://www.weblio.jp/content/word02051"))
(defun lookup-youtube ()      "Lookup on youtube." (interactive) (lookup-word-on-internet "https://www.youtube.com/results?search_query=word02051"))

(use-package synosaurus
  :straight t
  :custom
  (synosaurus-choose-method 'ivy))

(use-package define-word
  :straight t
  :custom
  (define-word-displayfn-alist
    '((wordnik . my/define-word--display-in-buffer)
      (openthesaurus . my/define-word--display-in-buffer)
      (webster . my/define-word--display-in-buffer)
      (weblio . my/define-word--display-in-buffer)))
  (define-word-default-service 'wordnik)
  :config
  (setf (cdr (assoc 'wordnik define-word-services))
        '("http://wordnik.com/words/%s" my/define-word--parse-wordnik-all))
  (push '(weblio "https://ejje.weblio.jp/content/%s"
                 my/define-word--parse-weblio)
        define-word-services))

(defun my/define-word ()
  "docstring"
  (interactive)
  (if (use-region-p)
      (call-interactively #'define-word-at-point)
    (call-interactively #'define-word)))

(defun my/define-word--display-in-buffer (definition)
  "docstring"
  (let* ((buf-name "*DEFINE WORD*")
         (buffer (get-buffer-create buf-name))
         (display-buffer-alist
          `((,buf-name
             (display-buffer-reuse-window display-buffer-below-selected display-buffer-at-bottom)
             (window-height . 0.75)))))
    (with-current-buffer buffer
      (read-only-mode -1)
      (erase-buffer)
      (insert definition)
      (goto-char (point-min))
      (save-excursion (xml-parse-string))
      (read-only-mode 1))
    (display-buffer buffer)
    (switch-to-buffer-other-window buffer)))

(defun my/define-word--parse-wordnik-example ()
  (save-excursion
    (let (beg
          results
          (define-word-limit 10))
      (re-search-forward "<h2>Examples" nil t)
      (save-excursion
        (setq beg (re-search-forward "<div class=\"relatedWords-module\">" nil t)))
      (while (re-search-forward "<p class=\"text\">\\(.*\\)</p>" beg t)
        (push "---" results)
        (push (match-string 1) results))
      (when (setq results (nreverse results))
        (define-word--convert-html-tag-to-face (define-word--join-results results))))))

(defun my/define-word--parse-wordnik-related-word ()
  "docstring"
  (save-excursion
    (save-match-data
      (seq-reduce
       (lambda (accum group)
         (let (results
               (define-word-limit 20))
           (if (re-search-forward (format "<h3>%s" group) nil t)
               (progn (save-excursion
                        (setq beg (re-search-forward "<div class=\"related-group-header clearfix\">" nil t)))
                      (push (concat group ":") results)
                      (while (re-search-forward "<span data-definition-for=\"\\([^\"]*\\)\"" beg t)
                        (push (concat "- " (match-string 1)) results)
                        (re-search-forward "</span>"))
                      (concat (when accum (concat accum "\n\n"))
                              (when (setq results (nreverse results))
                                (define-word--convert-html-tag-to-face (define-word--join-results results)))))
             accum)))
       '("antonym" "equivalents" "hypernyms" "hyponyms" "same context")
       nil))))

(defun my/define-word--parse-wordnik-all ()
  ""
  (concat "Definitions:\n" (funcall #'define-word--parse-wordnik) "\n\n"
          (funcall #'my/define-word--parse-wordnik-related-word) "\n\n"
          "Examples:\n" (funcall #'my/define-word--parse-wordnik-example)))

(defun my/define-word--parse-weblio ()
  (save-excursion
    (let (beg
          results
          (define-word-limit 10))
      (re-search-forward "<td><b class=\"squareCircle description\">主な意味</b></td>" nil t)
      (save-excursion
        (setq beg (re-search-forward "<span class=\"element-block\"><b class=\"squareCircle\">音節</b><span class=\"syllableEjje\">" nil t)))
      (while (re-search-forward "<td[^>]*>\\([^<]*\\)</td>" beg t)
        (push (match-string 1) results))
      (when (setq results (nreverse results))
        (define-word--convert-html-tag-to-face (define-word--join-results results))))))
