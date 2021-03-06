
(create-fontset-from-ascii-font "TakaoGothic" nil "default")
(create-fontset-from-ascii-font "TakaoExGothic" nil "variable")
(create-fontset-from-ascii-font "Gen Shin Gothic Monospace Light" nil "dense")

(set-face-attribute 'default
                    nil
                    :font "fontset-default"
                    :height (my-adjust-font-size 660))
(set-face-attribute 'variable-pitch
                    nil
                    :font "fontset-variable"
                    :height (my-adjust-font-size 660))

(use-package face-remap
  :diminish buffer-face-mode
  :config
  (buffer-face-mode 1))

(use-package modus-themes
  :straight (modus-themes
             :host github
             :repo "protesilaos/modus-themes"
             :branch "main")
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-slanted-constructs t
        modus-themes-bold-constructs nil
        modus-themes-region 'no-extend)
  ;; Load the theme files before enabling a theme
  (modus-themes-load-themes)
  :config
  (modus-themes-load-vivendi))

(with-eval-after-load 'elfeed
  (set-face-foreground
   'elfeed-search-title-face "#4D4D4D")
  (set-face-foreground
   'elfeed-search-unchecked-title-face "cornflowerblue")
  (set-face-foreground
   'elfeed-search-checked-title-face "darkblue"))
