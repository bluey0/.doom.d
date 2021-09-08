;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "blueywoons"
      user-mail-address "lcd359.khoa@gmail.com")

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory-hub "~/org-files/")
(setq org-directory (concat org-directory-hub "notes/"))
(setq org-agenda-files (list org-directory))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

;; set the theme
(setq doom-theme 'doom-gruvbox)

;; set the font
(setq doom-font (font-spec :family "Sarasa Fixed J" :size 18)
      doom-variable-pitch-font doom-font
      doom-unicode-font doom-font)

;; elfeed
(map! :leader
      (:prefix ("a" . "applications")
      :desc "Open Elfeed (rss)"
      "f" #'=rss))
;; automatically update feed when opening elfeed
(add-hook! 'elfeed-search-mode-hook 'elfeed-update)
(map! :map elfeed-search-mode-map
      :n "c" #'elfeed-search-clear-filter)

;; scrolling
;; better mouse scrolling
(setq mouse-wheel-scroll-amount '(0.07))
(setq mouse-wheel-progressive-speed nil)
;; scrolling doesn't fuck up the cursor

;; org-roam
(setq org-roam-directory (concat org-directory-hub "roam/"))
(setq org-roam-capture-templates
      '(("n" "default" plain "%?" :if-new
         (file+head "%<%Y%m%d%H%M>.org" "#+title: ${title}\n")
         :unnarrowed t)))
;; search within roam notes using doom's search function
(defun org-roam-search ()
  "Perform a text search on `org-roam-directory'."
  (interactive)
  (require 'org-roam)
  (let ((default-directory org-roam-directory))
    (+default/search-project-for-symbol-at-point "")))
;; map search function to SPC n r S
(map! :leader
       (:prefix ("n" . "notes")
        (:when (featurep! :lang org +roam2)
         (:prefix ("r" . "roam")
          :desc "Search all notes"
          "S" #'org-roam-search))))

;; showing diff between init and example
(defun doom/ediff-init-and-example ()
  "ediff the current `init.el' with the example in doom-emacs-dir"
  (interactive)
  (ediff-files (concat doom-private-dir "init.el")
               (concat doom-emacs-dir "init.example.el")))

(define-key! help-map
  "di"   #'doom/ediff-init-and-example
  )
