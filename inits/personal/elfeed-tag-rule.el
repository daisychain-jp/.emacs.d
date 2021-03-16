;; elfeed dynamic tagging rules

(with-eval-after-load 'elfeed
  ;; for shizuoka-shinbun
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :feed-url (rx "www.at-s.com")
                                :entry-link
                                (rx (or "news/article/social/shizuoka"
                                        "news/article/politics/shizuoka"
                                        "news/article/topics/shizuoka"
                                        "news/article/culture/shizuoka"
                                        "news/article/local/west"
                                        "news/article/local/central"
                                        "news/article/local/east"))
                                :add 'regional))
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :feed-url (rx "www.at-s.com")
                                :entry-link
                                (rx (or "sports/article/shizuoka"
                                        "sports/article/national"
                                        "sports/article/soccer/national"))
                                :add 'sports))
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :feed-url (rx "www.at-s.com")
                                :entry-link
                                (rx "news/article/science")
                                :add 'science))
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :feed-url (rx "www.at-s.com")
                                :entry-link
                                (rx (or "news/article/topics/national"
                                        "news/article/culture/national"
                                        "news/article/politics/national"
                                        "news/article/social/national"
                                        "news/article/economy/national"
                                        "news/article/health/national"
                                        "news/article/international"
                                        "sports/article/sumo"
                                        "sports/article/golf"
                                        "sports/article/baseball"))
                                :remove 'unread))
  ;; workaround for bug
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :feed-url (rx "www.city.shimada.shizuoka.jp")
                                :remove 'checked))
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :feed-url (rx "www.city.shimada.shizuoka.jp")
                                :add 'unread))
  ;; for favorite entries
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :feed-url (rx "pc.watch.impress.co.jp")
                                :entry-title
                                (rx "【山田祥平のRe:config.sys】")
                                :add 'valuable))
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :feed-title (rx "デモクラシータイムス.")
                                :entry-title
                                (rx "WeN")
                                :add 'valuable))
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :feed-title (rx "Jリーグ ニュース")
                                :entry-title
                                (rx (or "入籍" "子が誕生"))
                                :remove 'unread))
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :feed-title (rx "BSAsahi")
                                :entry-title
                                (rx "町山智浩のアメリカの今を知るTV")
                                :add 'unread)
            20)
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :feed-title (rx "BSAsahi")
                                :remove 'unread)
            10)
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :feed-title (rx "DAZN Japan")
                                :entry-title
                                (rx (or "明治安田生命J1リーグ"
                                        "藤枝"
                                        "磐田"))
                                :add 'unread)
            20)
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :feed-title (rx "DAZN Japan")
                                :remove 'unread)
            10))
