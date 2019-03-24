(use-package multiple-cursors
  :straight t
  :after (hydra)
  :custom
  (mc/list-file (format "%s/mc-lists.el" user-emacs-directory))
  (mc/insert-numbers-default 1)
  :config
  (defhydra hydra-multiple-cursors
    (global-map "C-c n"
                :color red)
    "multiple-cursors-hydra"
    ("n"    mc/mark-next-like-this)
    ("N"    mc/skip-to-next-like-this)
    ("M-n"  mc/unmark-next-like-this)
    ("p"    mc/mark-previous-like-this)
    ("P"    mc/skip-to-previous-like-this)
    ("M-p"  mc/unmark-previous-like-this)
    ("e"    mc/edit-lines)
    ("|"    mc/vertical-align)
    ("#"    mc/insert-numbers)
    ("$"    my-mc/insert-numbers)
    ("s"    mc/sort-regions)
    ("S"    mc/reverse-regions)
    ("m"    mc/mark-more-like-this-extended)
    ("a"    mc/mark-all-like-this :exit t)
    ("r"    mc/mark-all-in-region-regexp :exit t)
    ("q"    nil)))

;; insert specific serial number
(defvar my-mc/insert-numbers-hist nil)
(defvar my-mc/insert-numbers-inc 1)
(defvar my-mc/insert-numbers-pad "%01d")

(defun my-mc/insert-numbers (start inc pad)
  "Insert increasing numbers for each cursor specifically."
  (interactive
   (list (read-number "Start from: " 0)
         (read-number "Increment by: " 1)
         (read-string "Padding (%01d): " nil my-mc/insert-numbers-hist "%01d")))
  (setq mc--insert-numbers-number start)
  (setq my-mc/insert-numbers-inc inc)
  (setq my-mc/insert-numbers-pad pad)
  (mc/for-each-cursor-ordered
   (mc/execute-command-for-fake-cursor
    'my-mc--insert-number-and-increase
    cursor)))

(defun my-mc--insert-number-and-increase ()
  (interactive)
  (insert (format my-mc/insert-numbers-pad mc--insert-numbers-number))
  (setq mc--insert-numbers-number (+ mc--insert-numbers-number my-mc/insert-numbers-inc)))
