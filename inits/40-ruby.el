(el-get-bundle ruby-mode)
(el-get-bundle inf-ruby)
(el-get-bundle yaml-mode)

;; ruby-mode
(use-package ruby-mode
  :mode (("\\.rb$"   . ruby-mode)
         ("Capfile$" . ruby-mode)
         ("Gemfile$" . ruby-mode))
  :interpreter (("ruby"    . ruby-mode)
                ("rbx"     . ruby-mode)
                ("jruby"   . ruby-mode))
  :config
  (setq ruby-indent-level 2)
  (setq ruby-insert-encoding-magic-comment nil)
  (add-hook 'ruby-mode-hook
            '(lambda ()
               (setq-local flycheck-checker 'ruby-rubocop)
               (flycheck-mode 1)
               (setq-local helm-dash-docsets helm-dash-docsets-ruby-mode)))
  (add-to-list 'hs-special-modes-alist
               `(ruby-mode
                 ,(rx (or "def" "class" "module" "do" "if" "{" "[")) ; Block start
                 ,(rx (or "}" "]" "end"))                       ; Block end
                 ,(rx (or "#" "=begin"))                        ; Comment start
                 ruby-forward-sexp nil)))

(use-package inf-ruby
  :config
  (setq inf-ruby-default-implementation "pry")
  (add-to-list 'inf-ruby-implementations '("pry" . "pry"))
  (setq inf-ruby-eval-binding "Pry.toplevel_binding")
  (setq inf-ruby-first-prompt-pattern "^\\[[0-9]+\\] pry\\((.*)\\)> *")
  (setq inf-ruby-prompt-pattern "^\\[[0-9]+\\] pry\\((.*)\\)[>*\"'] *"))

(el-get-bundle yaml-mode)
