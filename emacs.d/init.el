;;; .Emacs -- Kenneth's emacs config

;; Copyright (c) 2005 Kenneth Bowen <kenneth.bowen@pobox.com>
;;
;; Based on Phil Hollenback's (philiph@pobox.com) .emacs, and others

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

; load emacs files/hacks from .emacs.d/lisp
(let ((default-directory  "~/.emacs.d/lisp"))
  (normal-top-level-add-subdirs-to-load-path))
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
(setq-default auto-save-mode -1)
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

;;;; outline/outline-minor ;;;;
;; set up outline-minor mode for syntax highlighting
(add-hook 'outline-minor-mode-hook
          (lambda ()
            (font-lock-add-keywords nil outline-font-lock-keywords)
            (font-lock-mode 1)))

(require 'epa-file)
(epa-file-enable)
(setenv "GPG_AGENT_INFO" nil)

;;;; MELP packaging ;;;;
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-initialize)

;;;; markdown-mode ;;;;
;; http://jblevins.org/projects/markdown-mode/ ;;
(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)

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
             "sch" "rng" "xslt" "svg" "rss" "xhtml" "html") t) "\\'")
             'nxml-mode))

;;;; groovy ;;;;
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
(load-theme `misterioso)


;;;; php ;;;;
(require 'php-mode)
(add-hook 'php-mode-user-hook 
          (lambda ()
            (turn-on-font-lock)
            (turn-on-auto-fill)))

;;;; html ;;;;

;; css mode from http://www.emacswiki.org/cgi-bin/emacs/css-mode.el
(autoload 'css-mode "css-mode" "Mode for editing CSS files" t)
(setq auto-mode-alist
      (append '(("\\.css$" . css-mode))
	      auto-mode-alist))
(setq cssm-indent-function #'cssm-c-style-indenter)

;;;; JavaScript ;;;;
;; I use the JavaScript mode from the XEmacs distro, javascript-mode.el
(require 'javascript-mode)
(setq auto-mode-alist
      (append '(("\\.js$" . javascript-mode))
	      auto-mode-alist))

;;;; Puppet ;;;;
;;  puppet-mode from Puppet Labs Github
;;
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
 '(safe-local-variable-values (quote ((sh-indent-comment . t) (line-move-ignore-invisible . t))))
 '(show-paren-mode t nil (paren))
 '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify)))
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
