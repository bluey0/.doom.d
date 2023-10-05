;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "blueywoons"
      user-mail-address "lcd359.khoa@gmail.com")


;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(defvar org-directory-hub "~/org-files/")
(setq org-directory (concat org-directory-hub "notes/")
      org-agenda-files (list (concat org-directory "inbox.org")
                             (concat org-directory "notes.org")
                             (concat org-directory "tickler.org")))


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

;; set the theme
(setq doom-theme 'doom-gruvbox)

;; set the font
(setq doom-font (font-spec :family "Sarasa Mono J" :size 18)
      doom-variable-pitch-font doom-font
      doom-unicode-font doom-font)

;; Windows specific configs
(if (eq system-type 'windows-nt)
    ;; Window's clipboard can now encode non-ascii characters properly
    (and (set-selection-coding-system 'utf-16-le)
         ;; Set $HOME as default directory regardless of where emacs is placed
         (setq default-directory "~/")))

;; org-capture
(after! org
  (setq +org-capture-notes-file "inbox.org"
        org-capture-templates
        '(("n" "Inbox" entry
           (file+headline +org-capture-notes-file "Inbox")
           "* %u %?" :prepend t)
          ("N" "Tickler" entry
           (file+headline "tickler.org" "Tickler")
           "* %^t %?" :time-prompt t :prepend t))))

;; elfeed
(map! :leader
      (:prefix ("z" . "applications")
       :desc "Open Elfeed (rss)"
       "f" #'=rss))
;; automatically update feed when opening elfeed
(add-hook! 'elfeed-search-mode-hook 'elfeed-update)
(map! :map elfeed-search-mode-map
      :n "c" #'elfeed-search-clear-filter)


;; better mouse scrolling
(setq mouse-wheel-scroll-amount '(0.07)
      mouse-wheel-progressive-speed nil)


;; org-roam
(setq org-roam-directory (concat org-directory-hub "roam/")
      org-roam-capture-templates
      '(("n" "default" plain "%?" :if-new
         (file+head "%<%Y%m%d%H%M>.org" "#+title: ${title}\n")
         :unnarrowed t)))
;; search within roam notes using doom's search function
(defun org-roam-search (query)
  "Perform a text search on `org-roam-directory'."
  (interactive
   (list (if (doom-region-active-p)
             (buffer-substring-no-properties
              (doom-region-beginning)
              (doom-region-end))
           "")))
  (require 'org-roam)
  (+default/search-project-for-symbol-at-point
   query org-roam-directory))
;; map search function to SPC n r S
(map! :leader
      (:prefix ("n" . "notes")
       (:when (modulep! :lang org +roam2)
        (:prefix ("r" . "roam")
         :desc "Search all notes"
         "S" #'org-roam-search))))


;; showing diff between init and example
(defun doom/ediff-init-and-example ()
  "ediff the current `init.el' with the example in doom-emacs-dir"
  (interactive)
  (ediff-files (concat doom-user-dir "init.el")
               (concat doom-emacs-dir "templates/init.example.el")))
(define-key! help-map
  "di"   #'doom/ediff-init-and-example)


;; org-roam-ui
(use-package! websocket
  :after org-roam)
(use-package! org-roam-ui
  :after org-roam ;; or :after org
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))


;; company does not automatically hook on org-mode
(setq company-global-modes '(not org-mode))
