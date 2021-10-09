;; [[file:config.org::+BEGIN_SRC emacs-lisp][No heading:1]]
;; -*- lexical-binding: t -*-
;; No heading:1 ends here

;; [[file:config.org::*Git ls][Git ls:1]]
(defun my/git-ls (&optional path)
  "Find file in the current Git repository."
  (interactive)
  (let* ((temp default-directory)
         (default-directory (if (equal path nil)
                                (locate-dominating-file
                                 default-directory ".git")
                              path))
         (cands (split-string
                 (shell-command-to-string
                  "git ls-files --full-name --")
                 "\n"))
         (file (completing-read "Find file: " cands)))
    (when file
      (if (equal file "")
          (setq default-directory temp)
        (find-file file))
      )))

(map! :leader :desc "Find in git ls-files"
      :n "fg" 'my/git-ls)

(defun my/git-ls-dot ()
  (interactive)
  (my/git-ls "~/"))

(map! :leader :desc "Find in git ls-files"
      :n "fG" 'my/git-ls-dot)
;; Git ls:1 ends here

;; [[file:config.org::*Me][Me:1]]
(setq user-full-name "Nick Friday"
      user-mail-address "nfriday@ya.ru")
;; Me:1 ends here

;; [[file:config.org::*Look and feel][Look and feel:1]]
(defun my-font (size) (font-spec :family "Iosevka nf" :width 'expanded :size size))
(setq doom-font (my-font 18)
      doom-big-font (my-font 24)
      doom-variable-pitch-font (font-spec :family "Arial" :size 20))

(setq doom-theme 'doom-dracula)
;; Look and feel:1 ends here

;; [[file:config.org::*Stuff][Stuff:1]]
(setq display-line-numbers-type t)

(setq-default tab-width 2)
(setq-default standard-indent 2)
(setq-default evil-shift-width 2)

(setq calendar-week-start-day 1)
;; Stuff:1 ends here

;; [[file:config.org::*Basics][Basics:1]]
(setq org-directory (concat (getenv "HOME") "/Documents/org/"))
;; Basics:1 ends here

;; [[file:config.org::*Basics][Basics:2]]
(defun my/org/hook ()
  (interactive)
  (+company/toggle-auto-completion))
;; Basics:2 ends here

;; [[file:config.org::*Basics][Basics:3]]
(add-hook 'org-mode-hook 'my/org/hook)
;; Basics:3 ends here

;; [[file:config.org::*Heading font sizes][Heading font sizes:1]]
(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.3))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.2))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.1))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))
;; Heading font sizes:1 ends here

;; [[file:config.org::*Subtree to report][Subtree to report:1]]
(defun my/format/org-to-report (str)
  (replace-regexp-in-string
   "^\\+ "
   "* "
   (replace-regexp-in-string
    "^\\*+ "
    "\n"
    (replace-regexp-in-string
     "\\[\\[.+\\]\\[\\(.+\\)\\]\\]"
     "\\1"
     str))))

(defun my/org-subtree-to-report ()
  (interactive)
  (org-copy-subtree)
  (kill-new (my/format/org-to-report
    (substring-no-properties (car kill-ring)))))

(map! :localleader :desc "Copy org heading to report text" :mode org-mode
      :n "zr" 'my/org-subtree-to-report)
;; Subtree to report:1 ends here

;; [[file:config.org::*Sync by rclone][Sync by rclone:1]]
;; (defun my/org/sync-to ()
;;   (interactive)
;;   (if (string-match (concat org-directory ".*/*\\.org") buffer-file-name)
;;       (start-process "rclone-org" nil "rclone" "sync" "--include" "*.org" org-directory "d:org")))
;; (add-hook 'after-save-hook 'my/org/sync-to)
;; Sync by rclone:1 ends here

;; [[file:config.org::*JS][JS:1]]
(defun my/prettify/js-hook ()
    (interactive)
  (setq prettify-symbols-alist '()))
(add-hook 'js-mode-hook 'my/prettify/js-hook)
;; JS:1 ends here

;; [[file:config.org::*Org][Org:1]]
(defun my/prettify/org-hook ()
    (interactive)
    (setq prettify-symbols-alist '(
                                   ("#+begin_src" . "")
                                   ("#+end_src" . "―")
                                   ("#+BEGIN_SRC" . "")
                                   ("#+END_SRC" . "―")
                                   ("#+begin_quote" . "")
                                   ("#+end_quote" . "")
                                   ("#+BEGIN_QUOTE" . "")
                                   ("#+END_QUOTE" . "")
                                   (":PROPERTIES:" . "")
                                   (":END:" . "―")
                                   ("#+STARTUP:" . "")
                                   ("#+TITLE:" . "")
                                   ("#+RESULTS:" . "")
                                   ("#+NAME:" . "")
                                   ("#+ROAM_TAGS:" . "")
                                   ("#+FILETAGS:" . "")
                                   ("#+HTML_HEAD:" . "")
                                   ("#+SUBTITLE:" . "")
                                   ("#+AUTHOR:" . "")
                                   ("SCHEDULED:" . "")
                                   ("DEADLINE:" . "")))
    (prettify-symbols-mode 1))
;; (add-hook 'org-mode-hook 'my/prettify/org-hook)
;; Org:1 ends here

;; [[file:config.org::*Roam][Roam:1]]
(use-package! org-roam
  :bind
  ("C-c i" . org-roam-node-insert)
)
;; Roam:1 ends here

;; [[file:config.org::*Treesitter][Treesitter:1]]
(use-package! tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  (pushnew! tree-sitter-major-mode-language-alist
            '(scss-mode . css)))
;; Treesitter:1 ends here

;; [[file:config.org::*Rainbow delimiters][Rainbow delimiters:1]]
(use-package! rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))
;; Rainbow delimiters:1 ends here

;; [[file:config.org::*Indent guides][Indent guides:1]]
(use-package! highlight-indent-guides
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))
;; Indent guides:1 ends here

;; [[file:config.org::*Company +tabnine][Company +tabnine:1]]
(use-package! company
  :config
  (setq company-show-quick-access t)
  (define-key company-active-map (kbd "C-SPC") #'company-abort))

(use-package! company-tabnine
  :config
  (add-to-list 'company-backends #'company-tabnine))
;; Company +tabnine:1 ends here

;; [[file:config.org::*Reverse IM][Reverse IM:1]]
(use-package! reverse-im
  :config
  (setq reverse-im-input-methods '("russian-computer"))
  (reverse-im-mode))
;; Reverse IM:1 ends here

;; [[file:config.org::*Org link mode][Org link mode:1]]
(define-globalized-minor-mode org-link-global-mode org-link-minor-mode
  (lambda () (org-link-minor-mode 1)))

(org-link-global-mode 1)
;; Org link mode:1 ends here

;; [[file:config.org::*Vterm][Vterm:1]]
;; (use-package! vterm
;;   :config
;;   (map! :mode vterm-mode
;;         :g "C-c C-d" 'vterm-send-C-d))
;; Vterm:1 ends here

;; [[file:config.org::*Idk mappings][Idk mappings:1]]
(map! :desc "Next workspace" :n "C-l" '+workspace:switch-next
      :desc "Prev workspace" :n "C-h" '+workspace:switch-previous)

(defun my/c/map-hook ()
  (interactive)
  (map! :map c-mode-map
      :desc "Next workspace" :n "C-l" '+workspace:switch-next
      :desc "Prev workspace" :n "C-h" '+workspace:switch-previous))
(add-hook 'c-mode-hook 'my/c/map-hook)

(global-auto-composition-mode -1)
(map! :leader :desc "Toggle character composition (laggy for big text)"
      :n "td" 'auto-composition-mode)

(map!
 :i "C-h" 'backward-delete-char)

(map! :n "Q" 'kill-this-buffer)

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
      :desc "find char back to" :mn "T" 'evilem-motion-find-char-backward-to)
;; Idk mappings:1 ends here
