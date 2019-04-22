;;; -*- lexical-binding: t -*-

;;; Init file which only uses built-in packages.  For when I want
;;; something simple to work with.
;;;
;;; To start emacs with this configuration, run,
;;;     $ emacs -q --eval '(load-file "~/.emacs.simple.d/init.el")'


(setq user-full-name "Ramana Nagasamudram"
      user-mail-address "rnagasam@stevens.edu"
      user-emacs-directory "~/.emacs.simple.d/")

					; General
(setq transient-mark-mode nil)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(show-paren-mode 1)
(global-subword-mode 1)
(column-number-mode 1)
(size-indication-mode 1)

					; Custom
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(add-hook 'after-init-hook
	  (lambda ()
	    (when (file-exists-p custom-file)
	      (load custom-file))))

					; Backups
(setq backup-directory-alist '(("." . "~/.backup")))
(setq make-backup-files t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2)

					; Disabled commands
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

					; Dired
(setq dired-dwim-target t
      wdired-allow-to-change-permissions t)

					; Buffers
(define-key global-map (kbd "C-x C-b") 'ibuffer)

					; Windows
(define-key global-map (kbd "C-'") 'other-window)

(winner-mode 1)
(define-key global-map (kbd "<f7>") 'winner-undo)
(define-key global-map (kbd "<f8>") 'winner-redo)

(windmove-default-keybindings)

					; Emacs Lisp
(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (electric-pair-local-mode)))

					; Version Control
(setq version-control t)

					; Imenu
(define-key global-map (kbd "C-c .") 'imenu)

					; Ido
(require 'ido)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t
      ido-everywhere t
      ido-use-filename-at-point 'guess
      ido-use-url-at-point t
      ido-case-fold t
      ido-enable-regexp t)
(ido-mode 1)

(define-key ido-file-dir-completion-map (kbd "C-l")
  'ido-delete-backward-word-updir)

					; Text mode
(add-hook 'text-mode-hook
	  (lambda ()
	    (setq fill-column 74)
	    (turn-on-auto-fill)))

					; Man
(require 'man)
(setq Man-notify-method 'pushy)

					; C
(defun setup-c-mode ()
  (setq c-default-style "bsd"
	c-basic-offset 4
 	tab-width 4
	indent-tabs-mode t
	show-trailing-whitespace t)
  (display-line-numbers-mode t)
  (electric-pair-mode t)
  (local-set-key (kbd "M-*") 'pop-tag-mark)
  (local-set-key (kbd "C-c o") 'ff-find-other-file))
(add-hook 'c-mode-common-hook 'setup-c-mode)

					; sendmail
(setq mail-user-agent 'gnus-user-agent
      read-mail-command 'gnus
      send-mail-function 'message-sent-mail-with-sendmail
      message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program "msmtp"
      message-sendmail-extra-arguments '("-a" "default"))

					; TRAMP
(require 'tramp)
(setq tramp-debug-buffer t
      tramp-verbose 10
      tramp-default-method "ssh")

