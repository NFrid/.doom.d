;; ########################################################################## ;;
;; ########################### The `config.el' 🗿 ########################### ;;
;; ########################################################################## ;;

;; TODO: organize this whole thing

;; ------------------------------- muh stuff -------------------------------- ;;

(defun ~/magit-process-environment (env)
  "Add GIT_DIR and GIT_WORK_TREE to ENV when in a special directory.
https://github.com/magit/magit/issues/460 (@cpitclaudel)."
  (let ((default (file-name-as-directory (expand-file-name default-directory)))
        (home (expand-file-name "~/")))
    (when (string= default home)
      (let ((gitdir (expand-file-name "~/.dot/")))
        (push (format "GIT_WORK_TREE=%s" home) env)
        (push (format "GIT_DIR=%s" gitdir) env))))
  env)

(advice-add 'magit-process-environment
            :filter-return #'~/magit-process-environment)


(defun my/commentitle (text comin char comout)
  (let (tlen nchar)
    (setq tlen (string-width text))
    (setq nchar (- fill-column tlen 4
                   (+ (string-width comin)
                      (string-width comout))))

    (insert (concat comin " "
                    (make-string (/ nchar 2) char)
                    " " text " "
                    (make-string (+ (/ nchar 2) (% nchar 2)) char)
                    " " comout))))


(defun my/commentitle/c-dashes (text)
  (interactive "s(C-dashes) Text: ")
  (my/commentitle text "/*" ?- "*/")
  (insert "\n"))

(defun my/commentitle/c-hashes (text)
  (interactive "s(C-hashes) Text: ")
  (my/commentitle text "/*" ?# "*/")
  (insert "\n"))

(defun my/commentitle/c-box (text)
  (interactive "s(C-box) Text: ")
  (insert (concat "/* " (make-string (- fill-column 6) ?#) " */\n"))
  (my/commentitle text "/*" ?# "*/")
  (insert (concat "/* " (make-string (- fill-column 6) ?#) " */\n")))

(map! :map c-mode-map
      :i "C-c C-c -" 'my/commentitle/c-dashes
      :i "C-c C-c #" 'my/commentitle/c-hashes
      :i "C-c C-c b" 'my/commentitle/c-box)


(defun my/commentitle/lisp-dashes (text)
  (interactive "s(Lisp-dashes) Text: ")
  (my/commentitle text ";;" ?- ";;")
  (insert "\n"))

(defun my/commentitle/lisp-hashes (text)
  (interactive "s(Lisp-hashes) Text: ")
  (my/commentitle text ";;" ?# ";;")
  (insert "\n"))

(defun my/commentitle/lisp-box (text)
  (interactive "s(Lisp-box) Text: ")
  (insert (concat ";; " (make-string (- fill-column 6) ?#) " ;;\n"))
  (my/commentitle text ";;" ?# ";;")
  (insert (concat "\n;; " (make-string (- fill-column 6) ?#) " ;;\n")))

(map! :map (lisp-mode-map emacs-lisp-mode-map)
      :i "C-c C-c -" 'my/commentitle/lisp-dashes
      :i "C-c C-c #" 'my/commentitle/lisp-hashes
      :i "C-c C-c b" 'my/commentitle/lisp-box)

(defun my/git-ls (&optional path flags)
  "Find file in the current Git repository."
  (interactive)
  (let* ((temp default-directory)
         (default-directory (if (equal path nil)
                                (locate-dominating-file
                                 default-directory ".git")
                              path))
         (cands (split-string
                 (shell-command-to-string
                  (format
                   "git %s ls-files --full-name --"
                   (if (equal flags nil) "" flags)))
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
  (my/git-ls "~/" "--work-tree=. --git-dir=./.dot"))

(map! :leader :desc "Find in git ls-files"
      :n "fG" 'my/git-ls-dot)

(map! :leader :desc "Git Status for .dot"
      :n "gd" (cmd! (magit-status (expand-file-name "~/"))))


;; --------------------------------- basics --------------------------------- ;;

(setq user-full-name "Nick Friday"
      user-mail-address "nfriday@ya.ru")


(defun my-font (size) (font-spec :family "Iosevka nf" :width 'expanded :size size))
(setq doom-font (my-font 18)
      doom-big-font (my-font 24)
      doom-variable-pitch-font (font-spec :family "Arial" :size 20))

(set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)

(setq doom-theme 'doom-dracula)


(setq display-line-numbers-type t)

(setq-default tab-width 2)
(setq-default standard-indent 2)
(setq-default evil-shift-width 2)

(setq calendar-week-start-day 1)
(setq org-agenda-start-on-weekday 1)

(use-package! russian-holidays
  :config
  (setq calendar-holidays russian-holidays))


;; ---------------------------------- org? ---------------------------------- ;;

(setq org-directory (concat (getenv "HOME") "/Documents/org/"))


(defun my/org/hook ()
  (interactive)
  (+company/toggle-auto-completion))


(add-hook 'org-mode-hook 'my/org/hook)


(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.3))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.2))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.1))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))


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


;; (defun my/org/sync-to ()
;;   (interactive)
;;   (if (string-match (concat org-directory ".*/*\\.org") buffer-file-name)
;;       (start-process "rclone-org" nil "rclone" "sync" "--include" "*.org" org-directory "d:org")))
;; (add-hook 'after-save-hook 'my/org/sync-to)
;; Sync by rclone:1 ends here


(defun my/prettify/js-hook ()
  (interactive)
  (setq prettify-symbols-alist '()))
(add-hook 'js-mode-hook 'my/prettify/js-hook)


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


(use-package! org-roam
  :bind
  ("C-c i" . org-roam-node-insert)
  )


(setq +zen-mixed-pitch-modes '())


;; ------------------------------- some stuff ------------------------------- ;;

(use-package! tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  (pushnew! tree-sitter-major-mode-language-alist
            '(scss-mode . css)))


(use-package! rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))


(use-package! highlight-indent-guides
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))


(use-package! company
  :config
  (setq company-show-quick-access t)
  (define-key company-active-map (kbd "C-SPC") #'company-abort))

(use-package! company-tabnine
  :config
  (add-to-list 'company-backends #'company-tabnine))


(use-package! reverse-im
  :config
  (setq reverse-im-input-methods '("russian-computer"))
  (reverse-im-mode))


;; ----------------------------- grammar stuff ------------------------------ ;;

(setq langtool-default-language "ru-RU")
(setq ispell-personal-dictionary "~/.doom.d/.spelldic")


;; (define-globalized-minor-mode org-link-global-mode org-link-minor-mode
;;   (lambda () (org-link-minor-mode 1)))

;; (org-link-global-mode 1)


;; (use-package! vterm
;;   :config
;;   (map! :mode vterm-mode
;;         :g "C-c C-d" 'vterm-send-C-d))


;; ----------------------------- chad dashboard ----------------------------- ;;

(setq fancy-splash-image "~/Images/Emojiz/boomer.png")

(setq +doom-dashboard-menu-sections
      '(("(z) Last session"
         :icon (all-the-icons-octicon "history" :face 'doom-dashboard-menu-title)
         :when (cond ((featurep! :ui workspaces)
                      (file-exists-p (expand-file-name persp-auto-save-fname persp-save-dir)))
                     ((require 'desktop nil t)
                      (file-exists-p (desktop-full-file-name))))
         :face (:inherit (doom-dashboard-menu-title bold))
         :action doom/quickload-session)
        ("(a) Org-agenda"
         :icon (all-the-icons-octicon "calendar" :face 'doom-dashboard-menu-title)
         :when (fboundp 'org-agenda)
         :action org-agenda)
        ("(r) Recent files"
         :icon (all-the-icons-octicon "file-text" :face 'doom-dashboard-menu-title)
         :action recentf-open-files)
        ("(p) Projectiles"
         :icon (all-the-icons-octicon "briefcase" :face 'doom-dashboard-menu-title)
         :action projectile-switch-project)
        ("(b) Bookmarks"
         :icon (all-the-icons-octicon "bookmark" :face 'doom-dashboard-menu-title)
         :action bookmark-jump)
        ("(c) Private config"
         :icon (all-the-icons-octicon "tools" :face 'doom-dashboard-menu-title)
         :when (file-directory-p doom-private-dir)
         :action doom/open-private-config)
        ("(.) Dotfiles"
         :icon (all-the-icons-octicon "settings" :face 'doom-dashboard-menu-title)
         :action my/git-ls-dot)
        ("(q) Kill workspace"
         :icon (all-the-icons-octicon "x" :face 'doom-dashboard-menu-title)
         :action +workspace/delete)))

(map! :map +doom-dashboard-mode-map
      :ne "z" #'doom/quickload-session
      :ne "a" #'org-agenda
      :ne "r" #'recentf-open-files
      :ne "p" #'projectile-switch-project
      :ne "c" #'doom/open-private-config
      :ne "b" #'bookmark-jump
      :ne "." #'my/git-ls-dot
      :ne "q" #'+workspace/delete)


(map! :nv "j" 'evil-next-visual-line
      :nv "k" 'evil-previous-visual-line)


(defun my/doom-dashboard-draw-ascii-banner-fn ()
  (let* ((banner
          '("  +oo+-+ss/      /osso:       /oss+:      -+sss+-      -+sss+-      :osso/     oo/ oo/ oo+"
            "  MMMNNNMMMy   sNMMNNMMm:   yNMMNNMMm:  -hMMNNMMMh   :dMMNNMMMh   +mMMNNMMNo  +MM /MM :MMy"
            " +MMMs--dMMM  yMMN+--mMMm  dMMm/--hmho  NMMd: /MMMo :MMMy: :mdh: +MMMy   MMM  dMM yMM sMM:"
            " dMMm   dMMm  MMMo   hMMm /MMM:        sMMM    MMMo hMMm         NMMMdddmMMM  MMd NMm NMN "
            " MMMo   MMMs oMMM    MMMs yMMN         mMMd   +MMM- MMMs        -MMMNmmmmmmh +MM /MMs MMy "
            "+MMM   +MMM- sMMM: -dMMN  hMMN:  ody+  NMMd  /NMMh  MMMh  -hho: :MMMy        dMM hMM yMM: "
            "dMMd   dMMm   dMMMMMMMd-   mMMMMMMMN+  /NMMMMMMMy   +NMMMMMMMh-  sMMMMMMMMd- mMMdNMMdMMN  "
            "ooo:   ooo:    :osys+-      :osyso:      /oyys/       /osys+       /osys+-   -ss- ss/oo/  "))
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat
                 line (make-string (max 0 (- longest-line (length line)))
                                   32)))
               "\n"))
     'face 'doom-dashboard-banner)))

(setq +doom-dashboard-ascii-banner-fn 'my/doom-dashboard-draw-ascii-banner-fn)


;; ------------------------------- workspaces ------------------------------- ;;

(map! :desc "Next workspace" :n "C-l" '+workspace:switch-next
      :desc "Prev workspace" :n "C-h" '+workspace:switch-previous)

(defun my/c/map-hook ()
  (interactive)
  (map! :map c-mode-map
        :desc "Next workspace" :n "C-l" '+workspace:switch-next
        :desc "Prev workspace" :n "C-h" '+workspace:switch-previous))
(add-hook 'c-mode-hook 'my/c/map-hook)

;; ------------------------------- basic maps ------------------------------- ;;

(map!
 :i "C-h" 'backward-delete-char)

(map! :n "Q" 'kill-this-buffer)

;; ------------------------------- easymotion ------------------------------- ;;

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

;; --------------------------------- magit ---------------------------------- ;;

(map! :map magit-mode-map
      :nv "x" 'magit-discard)

;; ---------------------------------- etc? ---------------------------------- ;;

(global-auto-composition-mode -1)
(map! :leader :desc "Toggle character composition (laggy for big text)"
      :n "td" 'auto-composition-mode)


;; ---------------------------------- mail ---------------------------------- ;;

(set-email-account! "gogle"
  '((mu4e-sent-folder       . "/ncraftertm@gmail.com/[Gmail].Sent Mail")
    (mu4e-drafts-folder     . "/ncraftertm@gmail.com/[Gmail].Drafts")
    (mu4e-trash-folder      . "/ncraftertm@gmail.com/[Gmail].Bin")
    (mu4e-refile-folder     . "/ncraftertm@gmail.com/[Gmail].All mail")
    (smtpmail-smtp-user     . "ncraftertm@gmail.com")
    (mu4e-compose-signature . "---\nNick Friday\na.k.a. undefined"))
  t)

(set-email-account! "ya.ru"
  '((mu4e-sent-folder       . "/nfriday@yandex.ru/Sent")
    (mu4e-drafts-folder     . "/nfriday@yandex.ru/Drafts")
    (mu4e-trash-folder      . "/nfriday@yandex.ru/Trash")
    (mu4e-refile-folder     . "/nfriday@yandex.ru")
    (smtpmail-smtp-user     . "nfriday@ya.ru")
    (mu4e-compose-signature . "---\nNick Friday\na.k.a. undefined")
    (+mu4e-personal-addresses . '("nfriday@ya.ru" "nfriday@yandex.ru")))
  t)

(setq +mu4e-header--maildir-colors '(("ncraftertm@gmail.com" . all-the-icons-blue-alt)
                                     ("nfriday@yandex.ru" . all-the-icons-yellow)))

(setq mu4e-bookmarks
      '(( :name  "Favs"
          :query "flag:flagged"
          :key ?b)
        ( :name  "Unread"
          :query "flag:unread AND NOT flag:trashed AND NOT maildir:/\.Spam$/"
          :key ?u)
        ( :name "Today"
          :query "date:today..now AND NOT maildir:/\.Spam$/"
          :key ?t)
        ( :name "Week"
          :query "date:7d..now AND NOT maildir:/\.Spam$/"
          :hide-unread t
          :key ?w)
        ( :name "Docs"
          :query "mime:application/* AND NOT maildir:/\.Spam$/"
          :key ?d)
        ( :name "Images"
          :query "mime:image/* AND NOT maildir:/\.Spam$/"
          :key ?i)
        ( :name "Spam"
          :query "maildir:/\.Spam$/"
          :key ?s)))
