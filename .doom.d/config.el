;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(setq user-full-name "mneuss"
      user-mail-address "mneuss@no-reply.com")

(setq doom-theme 'doom-one)

(setq org-directory "~/Notes/")
(after! org-capture
  (setq org-capture-templates
      '(("h" "Haskell Roadmap" entry
         (file+headline "programming_roadmap.org" "Haskell Roadmap")
         "* TODO %?")
        ("a" "Agda Roadmap" entry
         (file+headline "programming_roadmap.org" "Agda Roadmap")
         "* TODO %?")
        ("p" "Programming Roadmap" entry
         (file+headline "programming_roadmap.org" "Programming Roadmap")
         "* TODO %?")
        ("e" "Emacs Roadmap" entry
         (file+headline "programming_roadmap.org" "Emacs Roadmap")
         "* TODO %?")
        ("m" "Movies list" entry
         (file+headline "movies.org" "Movies Roadmap")
         "* TODO %?")
        ("g" "Games list" entry
         (file+headline "games.org" "Games Roadmap")
         "* TODO %?")
        ))
  )

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

(setq projectile-project-search-path '("~/Programming/"))

;; (use-package lsp-haskell
;;  :ensure t
;;  :config
;;  (setq lsp-haskell-process-path-hie "haskell-language-server-wrapper")
;;  ;; Comment/uncomment this line to see interactions between lsp client/server.
;;  ;;(setq lsp-log-io t)
;; )

(use-package dante
  :ensure t
  :after haskell-mode
  :commands 'dante-mode
  :init
  (add-hook 'haskell-mode-hook 'flycheck-mode)
  (add-hook 'haskell-mode-hook 'company-mode)
  (add-hook 'haskell-mode-hook 'dante-mode)
  :config
  (setq-default dante-repl-command-line
                '("cabal" "new-repl" dante-target "--builddir=dist-newstyle/dante"))
  (flycheck-add-next-checker 'haskell-dante '(warning . haskell-hlint))
  )
