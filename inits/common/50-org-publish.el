(setq org-publish-project-alist
      '(("wiki-src"
         :base-directory "~/proj/doc/web/wiki"
         :base-extension "org"
         :publishing-directory "/ssh:t.inamori@svr01:~/public_html/site/wiki"
         :recursive t
         :publishing-function org-html-publish-to-html
         :auto-preamble t
         :auto-sitemap t)
        ("wiki-res"
         :base-directory "~/proj/doc/web/wiki/res"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|mp4"
         :publishing-directory "/ssh:t.inamori@svr01:~/public_html/site/wiki/res"
         :recursive t
         :publishing-function org-publish-attachment)
        ("web" :components ("wiki-src" "wiki-res"))
        ("wiki-local"
         :base-directory "~/proj/doc/web/wiki"
         :base-extension "org"
         :publishing-directory "~/proj/doc/web/site/wiki"
         :recursive t
         :publishing-function org-html-publish-to-html
         :auto-preamble t
         :auto-sitemap t)))
