;;;; .Emacs -- Kenneth's emacs config

;; Copyright (c) 2005-2022 Kenneth Bowen <kenneth@kennethbowen.com>
;;
;; Based on Phil Hollenback's (philiph@pobox.com) .emacs, and others.

;; meta: This is a rework of my old emacs file that Phil, Peter, and others at
;; Lutris helped me construct sometime around '99-2000. I've recently migrated
;; from XEmacs to GNU Emacs, and have made no effort to see if this is
;; compatible with XEmacs.
;;
;; Notice the section headers - they look like that for use in outline-mode.
;; Type C-c @ C-t to get a listing of all the headings, type the usual motion
;; keys to move among them, & type C-c @ C-a to see all the text again. A
;; "Local Variables" section at the end of this file sets up outline mode
;; appropriately.
;;
;; Private information, like user-name and email address, are kept in
;; ~/.private.el. This makes it easy to distribute this file.

;;;; Global Settings ;;;;

;; load a private.el file IF we have one
(load (expand-file-name "~/.private.el") 'noerror)
(setq inhibit-startup-message t) ;we're all sick of it
(blink-cursor-mode 0)
(auto-fill-mode 1)
(setq default-major-mode 'text-mode)
(setq-default fill-column 120)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(setq-default tab-width 4)
(global-set-key "\C-x\C-b" 'electric-buffer-list)
(setq-default visible-bell 0) ;no freakin noise
(line-number-mode 1)
(column-number-mode 1)
(display-time)
;; no tool-bar, menubar, scrollbar, or other clutter
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-font-lock-mode 1)
(setq case-fold-search 1)
(setq current-language-environment "English")
(setq safe-local-variable-values '((sh-indent-comment . t) (line-move-ignore-invisible . t)))
(show-paren-mode 1)
(transient-mark-mode 1)
;; prevent custom set vars from ending up in init.el
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)
;; This is cool. Auto updates the buffer if the file changes.
(global-auto-revert-mode 1)
(add-to-list 'completion-styles 'substring)

;; start the server
(server-start)

;; keep track of recent files
(recentf-mode 1)

;; Turn off back up files
(setq make-backup-files nil)
(setq-default abbrev-mode nil)
;; I prefer spaces to tabs
(setq-default indent-tabs-mode nil)
; enable useful functions
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)


;; window title settings
(setq frame-title-format
      '(buffer-file-name
        "%f" (dired-directory dired-directory "%b")))
;; wheel-mouse
(require 'mwheel)
(mwheel-install)
(setq imwheel-scroll-interval 2)

;; keyboard mods not specific to any mode.  note the use of kbd function to
;; make these work in emacs or xemacs.
(global-set-key (kbd "C-c g") 'goto-line)
;; avoid the alt key
(global-set-key (kbd "\C-x\C-m") 'execute-extended-command)
(global-set-key (kbd "\C-c\C-m") 'execute-extended-command)
;; trying these out from a Steve Yegge article
(global-set-key (kbd "\C-w") 'backward-kill-word)
(global-set-key (kbd "\C-x\C-k") 'kill-region)
(global-set-key (kbd "\C-c\C-k") 'kill-region)

;;;;; Global Functions ;;;;;
;; functions useful for any mode, so put them at the top of this file.
;; Get rid of that annoying prompt that requires typing Y-E-S and then
;; pressing the friggin enter key to confirm. Thanks Phil.
(defun yes-or-no-p (PROMPT)
  (beep)
  (y-or-n-p PROMPT))
(setq efs-generate-anonymous-password nil)
;; On a mac, aspell is installed via homebrew
(if (eq system-type 'darwin)
    (setq ispell-program-name "/opt/homebrew/bin/aspell")
  (setq ispell-program-name "/usr/bin/aspell"))

;; GNU/FSF Emacs doesn't come with this useful function
(unless (fboundp 'prefix-region)
  (defun prefix-region
      (prefix) "Add a prefix string to each line between mark and point."
      (interactive "sPrefix string: ")
      (if prefix (let ((count (count-lines (mark) (point))))
                   (goto-char (min (mark) (point)))
                   (while (> count 0) (setq count (1- count))
                          (beginning-of-line 1) (insert prefix)
                          (end-of-line 1) (forward-char 1))))))

;; insert nicely formatted date
(defun insert-date ()
  (interactive)
  (insert (format-time-string "%d %B %Y")))

(defun sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
    Prefixed with negative \\[universal-argument], sorts in reverse.

    The variable `sort-fold-case' determines whether alphabetic case
    affects the sort order.

    See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" beg end))

;;;; outline/outline-minor ;;;;
;; set up outline-minor mode for syntax highlighting
(add-hook 'outline-minor-mode-hook
          (lambda ()
            (font-lock-add-keywords nil outline-font-lock-keywords)
            (font-lock-mode 1)))

(require 'epa-file)
(epa-file-enable)
(setenv "GPG_AGENT_INFO" nil)

;;;; MELPA stable packages ;;;;
;; M-x package-list-package to get to the package listing ;;
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;;;; use-package ;;;;
;; install use package if needed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; enable
(eval-when-compile
  (require 'use-package))
(require 'bind-key)

;; sundry packages to install that don't have a config section
(use-package diminish :ensure t)
(use-package adoc-mode :ensure t)
(use-package yaml-mode :ensure t)
(use-package base16-theme :ensure t)
(use-package ample-theme :ensure t)
(use-package ample-zen-theme :ensure t)
(use-package dockerfile-mode :ensure t)

;;;; org-roam ;;;;
;; config from https://systemcrafters.net/build-a-second-brain-in-emacs/getting-started-with-org-roam/
;; TODO set up dailies and key bindings
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (file-truename "~/notes/roam-notes"))
  (org-roam-db-location (file-truename "~/notes/roam-notes/org-roam.db"))
  (org-roam-completion-everywhere t)

  ;; *** Show title + tags in org-roam-node-find candidates ***
  (org-roam-node-display-template
   (concat "${title:*} "
           (propertize "${tags:20}" 'face 'org-tag)))

  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i"    . completion-at-point))
  :config
  (org-roam-db-autosync-mode))


;;;; org-drill ;;;;
;; https://orgmode.org/worg/org-contrib/org-drill.html
(use-package org-drill
  :ensure t
  :after org
  :custom
  (org-drill-cram-hours .15)
  :config
  (defun org-drill-time-to-inactive-org-timestamp (time)
    "Convert TIME into org-mode timestamp."
    (format-time-string
     (concat "[" (cdr org-time-stamp-formats) "]")
     time)))

;;;; toml-mode ;;;;
;; https://github.com/dryman/toml-mode.el
(use-package toml-mode
  :ensure t
  :mode ("\\.toml\\'"))

;;;; web-mode ;;;;
;; http://web-mode.org/ multi mode for web files ;;
(use-package web-mode
  :ensure t
  :mode ("\\.html\\'" "\\.js\\'" "\\.css\\'" "\\.php\\'")
  :hook (web-mode . my-web-mode-hook)
  :config
  (defun my-web-mode-hook ()
    "config hooks for Web mode"
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    ))

;;;; markdown-mode ;;;;
;; http://jblevins.org/projects/markdown-mode/ ;;
(use-package markdown-mode
  :ensure t
  :custom
  (markdown-reference-location 'end))

;;;; shell mode ;;;;
;; Fix junk characters in shell mode
(add-hook 'shell-mode-hook
          'ansi-color-for-comint-mode-on)

;;;; ruby mode ;;;;
(use-package ruby-mode
  :ensure t
  :mode ("\\.rb\\'")
  :interpreter "ruby")

;;;; nxml ;;;;
(add-to-list 'auto-mode-alist (cons (concat "\\." (regexp-opt '("xml" "xsd"
             "sch" "rng" "xslt" "svg" "rss" "xhtml") t) "\\'")
                                    'nxml-mode))

;;;; groovy ;;;;
(use-package groovy-mode
  :ensure t
  :config
  (defun my-groovy-mode-hook ()
    (setq groovy-indent-offset 2))
  :hook (groovy-mode . my-groovy-mode-hook)
  :mode ("\\.groovy\\'" "Jenkinsfile\\'"))

;;;; colors/theme ;;;;
(load-theme `base16-atelier-sulphurpool-light t)

;;;; Puppet ;;;;
;;  puppet-mode from Puppet Labs Github
;;
(use-package puppet-mode
  :ensure t
  :mode "\\.pp\\'")

;;;; Emacs Local ;;;;

;; Local Variables:
;; mode: outline-minor
;; line-move-ignore-invisible: t
;; outline-regexp: ";;;;+"
;; page-delimiter: "^;;;;"
;; End:

;;; thus ends my .emacs
