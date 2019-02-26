(use-package view
  :diminish view-mode "vw"
  :config
  (bind-keys :map global-map
             ("s-v" . view-mode)
             :map view-mode-map
             ("RET" . nil)
             ("q"   . nil)
             ("SPC" . View-scroll-half-page-forward)
             ("z"   . View-scroll-to-buffer-end)
             ("t"   . View-scroll-title-forward)
             ("f"   . View-scroll-line-forward)
             ("e"   . View-scroll-line-backward)
             ("w"   . View-scroll-half-page-backward)
             ("r"   . View-scroll-chunk-backward)
             ("g"   . View-scroll-chunk-forward)))

(defun View-scroll-title-forward ()
  (interactive)
  (if (eq major-mode 'eww-mode)
      (eww-goto-top)
    (next-line)))

(defun View-scroll-chunk-forward ()
  (interactive)
  (if (eq major-mode 'twittering-mode)
      (twittering-goto-next-status-and-top)
    (progn
      (forward-paragraph 2)
      (backward-paragraph 1)
      (forward-line 1)
      (recenter-top-bottom 0))))

(defun View-scroll-chunk-backward ()
  (interactive)
  (if (eq major-mode 'twittering-mode)
      (twittering-goto-previous-status-and-top)
    (progn
      (backward-paragraph 2)
      (forward-paragraph 1)
      (backward-paragraph 1)
      (forward-line 1)
      (recenter-top-bottom 0))))
