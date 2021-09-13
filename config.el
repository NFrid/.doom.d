;;; config.el -*- lexical-binding: t; -*-

(setq user-full-name "Nick Friday"
      user-mail-address "nfriday@ya.ru")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Iosevka nf" :size 18)
      doom-variable-pitch-font (font-spec :family "Arial" :size 20))

(setq doom-theme 'doom-dracula)

(setq org-directory "~/Documents/org/")

(setq display-line-numbers-type t)

(setq tab-width 2)
(setq standard-indent 2)
(setq evil-shift-width 2)

(setq calendar-week-start-day 1)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


(use-package! tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  (pushnew! tree-sitter-major-mode-language-alist
            '(scss-mode . css))
  )

(use-package! rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  )

(use-package! highlight-indent-guides
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-responsive 'stack)
  )

(use-package! company
  :config
  (setq company-show-quick-access t)
  (define-key company-active-map (kbd "C-SPC") #'company-abort)
  )

(use-package! company-tabnine
  :config
  (add-to-list company-backends #'company-tabnine)
  )

(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.5))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.4))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.3))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.2))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.1))))
 )

(use-package! reverse-im
  :config
  (setq reverse-im-input-methods '("russian-computer"))
  (reverse-im-mode)
  )

;; mappings

(map!
 :i "C-h" 'backward-delete-char
 (:mode org-mode
  :i "C-h" 'backward-delete-char
  :i "C-l" 'recenter-top-bottom
  )
 )

(map! :n "C-h" '+tabs:previous-or-goto
      :n "C-l" '+tabs:next-or-goto
      :n "SPC ESC" 'centaur-tabs-toggle-groups
      (:mode (c-mode c++-mode cpp-mode)
       :n "C-h" '+tabs:previous-or-goto
       :n "C-l" '+tabs:next-or-goto
       )
      )

(map! :n "Q" 'kill-this-buffer)

(map! :leader
      :desc "Next workspace" :n "j" '+workspace:switch-next
      :desc "Prev workspace" :n "k" '+workspace:switch-previous
      )

(map! :n "s" nil)
(map! :prefix "s"
      :desc "next lines" :mn "j" 'evilem-motion-next-line
      :desc "prev lines" :mn "k" 'evilem-motion-previous-line
      :desc "next WORDS" :mn "l" 'evilem-motion-forward-WORD-begin
      :desc "prev WORDS" :mn "h" 'evilem-motion-backward-WORD-begin

      :desc "prev words" :mn "b" 'evilem-motion-backward-word-begin
      :desc "next words" :mn "w" 'evilem-motion-forward-word-begin
      :desc "next words end" :mn "e" 'evilem-motion-forward-word-end
      :desc "prev WORDS" :mn "B" 'evilem-motion-backward-WORD-begin
      :desc "next WORDS" :mn "W" 'evilem-motion-forward-WORD-begin
      :desc "next WORDS end" :mn "e" 'evilem-motion-forward-WORD-end

      :desc "words" :mn "SPC" 'evil-avy-goto-word-0
      :desc "2 chars" :mn "s" 'evil-avy-goto-char-2
      :desc "lines" :mn "g" 'evil-avy-goto-line
      :desc "chars" :mn "/" 'evil-avy-goto-char-timer

      :desc "find char" :mn "f" 'evilem-motion-find-char
      :desc "find char to" :mn "t" 'evilem-motion-find-char-to
      :desc "find char back" :mn "F" 'evilem-motion-find-char-backward
      :desc "find char back to" :mn "T" 'evilem-motion-find-char-backward-to
      )
