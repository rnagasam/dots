;;; -*- lexical-binding: t -*-

(setq user-full-name "Ramana Nagasamudram"
      user-mail-address "rnagasam@stevens.edu")

					; Custom
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(add-hook 'after-init-hook
	  (lambda ()
	    (when (file-exists-p custom-file)
	      (load custom-file))))

					; General
(defalias 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-screen t
      inhibit-splash-screen t
      visual-bell 1
      ring-bell-function 'ignore)

(setq transient-mark-mode nil)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(show-paren-mode 1)
(global-subword-mode 1)
(column-number-mode 1)
(global-auto-revert-mode 1)
(electric-pair-mode)


(setq language-environment "UTF-8")

					; `suspend-frame'
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(global-set-key (kbd "M-z") 'zap-up-to-char)

					; Dired
(setq dired-dwim-target t
      wdired-allow-to-change-permissions t)

					; Buffers
(global-set-key (kbd "C-x C-b") 'bs-show)

					; Windows
(global-set-key (kbd "C-'") 'other-window)
