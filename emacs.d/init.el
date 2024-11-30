q;;; .Emacs -- Kenneth's emacs config

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

; commenting out for now...loading everything I need with use-package
; load emacs files/hacks from .emacs.d/lisp
;(let ((default-directory  "~/.emacs.d/lisp"))
;  (normal-top-level-add-subdirs-to-load-path))
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

;; start the server
(server-start)

;; Turn off back up files
(setq make-backup-files nil)
(setq-default abbrev-mode nil)
;; I prefer spaces to tabs
(setq-default indent-tabs-mode nil)
(put 'downcase-region 'disabled nil)

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
(setq ispell-program-name "/usr/bin/aspell")

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
             '("melpa-stable" . "http://melpa.org/packages/") t)
(package-initialize)

;;;; use-package ;;;;
;; install use package if needed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; enable
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; sundry packages to install that don't have a config section
(use-package adoc-mode
  :ensure t)
(use-package yaml-mode
  :ensure t)
(use-package base16-theme
  :ensure t)
(use-package ample-theme
  :ensure t)
(use-package ample-zen-theme
  :ensure t)

;;;; org-roam ;;;;
;; config from https://systemcrafters.net/build-a-second-brain-in-emacs/getting-started-with-org-roam/
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/notes/roam-notes")
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i"    . completion-at-point))
  :config
  (org-roam-setup))

;;;; web-mode ;;;;
;; http://web-mode.org/ multi mode for web files ;;
(use-package web-mode
  :ensure t)
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
)
(add-hook 'web-mode-hook  'my-web-mode-hook)
(add-to-list 'auto-mode-alist
             (cons (concat "\\." (regexp-opt '("js" "css"
                                  "html" "php") t) "\\'") 'web-mode))

;;;; markdown-mode ;;;;
;; http://jblevins.org/projects/markdown-mode/ ;;
(use-package markdown-mode
  :ensure t)
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq markdown-reference-location 'end)

;;;; shell mode ;;;;
;; Fix junk characters in shell mode
(add-hook 'shell-mode-hook
          'ansi-color-for-comint-mode-on)

;;;; ruby mode ;;;;
(require 'ruby-mode)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

;;;; nxml ;;;;
(add-to-list 'auto-mode-alist (cons (concat "\\." (regexp-opt '("xml" "xsd"
             "sch" "rng" "xslt" "svg" "rss" "xhtml") t) "\\'")
                                    'nxml-mode))

;;;; web-mode ;;;;
;; web mode customizations
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2))
(add-hook 'web-mode-hook  'my-web-mode-hook)
(add-to-list 'auto-mode-alist (cons (concat "\\." (regexp-opt '("html") t) "\\'")
                                    'web-mode))


;;;; groovy ;;;;
(use-package groovy-mode
  :ensure t)
(defun my-groovy-mode-hook ()
  (setq groovy-indent-offset 2))
(add-hook 'groovy-mode-hook `my-groovy-mode-hook)
(add-to-list 'auto-mode-alist '("Jenkinsfile\\'" . groovy-mode))


;;;; Calendar and Diary ;;;;

;; I keep a collection of GTD files under version control, including my diary
;; file.
(setq cal-tex-diary t
      diary-file "~/gtd/diary")

;; ;;;; BBDB ;;;;
(require 'bbdb)
;; bbdb file is kept with gtd stuff
(setq bbdb-file "~/gtd/bbdb")
(bbdb-initialize)

;;;; colors/theme ;;;;
;; emacs 24 has built in themes
(load-theme `base16-atelier-sulphurpool-light t)

;;;; Puppet ;;;;
;;  puppet-mode from Puppet Labs Github
;;
(use-package puppet-mode
  :ensure t)
(require 'puppet-mode)
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))


;;;; Custom Set Shit ;;;;
;; TODO get rid of these, where possible
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(case-fold-search t)
 '(current-language-environment "English")
 '(custom-safe-themes
   '("e48fc060df74469f0813aaeca2d3556693c26c8a3d7d6d80c9eb7bd43a087d06" "53f32cec91beb4ef88bd2a796718c41c3c21ebf19a23fa823de712b9f72941ff" "36ca8f60565af20ef4f30783aa16a26d96c02df7b4e54e9900a5138fb33808da" "c54024629d99a07dca9318af62f81a42e12449ace38e88850a54e697c011274d" "b0c5c6cc59d530d3f6fbcfa67801993669ce062dda1435014f74cafac7d86246" "36282815a2eaab9ba67d7653cf23b1a4e230e4907c7f110eebf3cdf1445d8370" "146061a7ceea4ccc75d975a3bb41432382f656c50b9989c7dc1a7bb6952f6eb4" "78c1c89192e172436dbf892bd90562bc89e2cc3811b5f9506226e735a953a9c6" "bc4c89a7b91cfbd3e28b2a8e9e6750079a985237b960384f158515d32c7f0490" "1d079355c721b517fdc9891f0fda927fe3f87288f2e6cc3b8566655a64ca5453" "cea3ec09c821b7eaf235882e6555c3ffa2fd23de92459751e18f26ad035d2142" "3380a2766cf0590d50d6366c5a91e976bdc3c413df963a0ab9952314b4577299" "7559ac0083d1f08a46f65920303f970898a3d80f05905d01e81d49bb4c7f9e39" "4feee83c4fbbe8b827650d0f9af4ba7da903a5d117d849a3ccee88262805f40d" "2a998a3b66a0a6068bcb8b53cd3b519d230dd1527b07232e54c8b9d84061d48d" "16dd114a84d0aeccc5ad6fd64752a11ea2e841e3853234f19dc02a7b91f5d661" "26d49386a2036df7ccbe802a06a759031e4455f07bda559dcf221f53e8850e69" default))
 '(ispell-dictionary nil)
 '(org-agenda-files '("~/dev/11ty/cultureofcode.com/TODO.org"))
 '(package-selected-packages
   '(org-roam apache-mode async bar-cursor bm boxquote browse-kill-ring color-theme-modern company csv-mode diminish eproject folding graphviz-dot-mode helm helm-core htmlize initsplit markdown-mode popup session tabbar gnu-elpa-keyring-update feature-mode base16-theme dockerfile-mode adoc-base16 groovy-markdown mode-mode moe-package puppet-theme theme-use web-yaml))
 '(safe-local-variable-values '((sh-indent-comment . t) (line-move-ignore-invisible . t)))
 '(show-paren-mode t nil (paren))
 '(text-mode-hook '(turn-on-auto-fill text-mode-hook-identify))
 '(transient-mark-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)

;;;; Emacs Local ;;;;

;; Local Variables:
;; mode: outline-minor
;; line-move-ignore-invisible: t
;; outline-regexp: ";;;;+"
;; page-delimiter: "^;;;;"
;; End:

;;; thus ends my .emacs
