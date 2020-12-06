(add-hook 'post-command-hook 'highlight-narrowed-buffer)
(defun highlight-narrowed-buffer ()
  "Highlight narrowed buffer.

Narrowed buffer have a fringe with highlighting color.
Plus text size in the buffer becomes a little bit larger."
  (unless (or (member last-command
                      '(text-scale-adjust))
              (eq major-mode 'Info-mode)
              (string-match-p (rx (seq line-start "CAPTURE-" (one-or-more ascii) line-end))
                              (buffer-name)))
    (let ((hl-fringe-color "DeepPink")
          (hl-text-scale (exp 1)))
      (set-face-attribute
       'fringe nil :background (if (buffer-narrowed-p)
                                   (progn
                                     (text-scale-set hl-text-scale)
                                     hl-fringe-color)
                                 (progn
                                   (when (= text-scale-mode-amount hl-text-scale)
                                     (text-scale-set 0))
                                   nil))))))

(unbind-key "C-x n d")
(unbind-key "C-x n n")
(unbind-key "C-x n p")
(bind-key* "C-x n" 'toggle-narrow-dwim)
(defun toggle-narrow-dwim (arg)
  "If the buffer is narrowed, it widens. Otherwise, it narrows intelligently.
Intelligently means: region, org-src-block, org-subtree, or defun,
whichever applies first.
Narrowing to org-src-block actually calls `org-edit-src-code'.

With prefix ARG, don't widen, just narrow even if buffer is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not arg))
         (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((and (boundp 'org-src-mode) org-src-mode (not arg))
         (org-edit-src-exit))
        ((derived-mode-p 'org-mode)
         (cond ((org-in-src-block-p)
                (org-edit-src-code))
               ((org-at-block-p)
                (org-narrow-to-block))
               ((org-at-heading-p)
                (org-narrow-to-subtree))
               (t (org-narrow-to-element))))
        (t (narrow-to-defun))))
