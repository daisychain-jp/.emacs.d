(setq org-publish-project-alist
      `(("recipe"
         :base-directory ,(format "%s/publish" env-doc-dir)
         :base-extension "org"
         :publishing-directory "/ssh:root@sv03:/var/www/html/wiki"
         :recursive t
         :publishing-function org-html-publish-to-html
         :auto-preamble t
         :auto-sitemap t)))
