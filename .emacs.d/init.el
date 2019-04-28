;;; -*- lexical-binding: t -*-

(setq user-full-name "Ramana Nagasamudram"
      user-mail-address "rnagasam@stevens.edu")

					; GC options
(let ((initial-gc-threshold 800000)
      (startup-gc-threshold (* 20 1024 1024)))
  (setq gc-cons-threshold startup-gc-threshold)
  (add-hook 'after-init-hook
	    (lambda () (setq gc-cons-threshold initial-gc-threshold))))
(add-hook 'focus-out-hook 'garbage-collect)

					; Custom and load path
(if (not (boundp 'user-emacs-directory))
    (setq user-emacs-directory "~/.emacs.d/"))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(add-hook 'after-init-hook
	  (lambda ()
	    (when (file-exists-p custom-file)
	      (load custom-file))))

(add-to-list 'load-path (expand-file-name "site-lisp/" user-emacs-directory))

					; Packages
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
		    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives
	       (cons "melpa" (concat proto "://melpa.org/packages/")) t))
(package-initialize)

(setq rmn/package-list
      '(ace-window
	auctex
	bbdb
	color-theme-modern
	company
	company-coq
	crux
	docker
	dockerfile-mode
	elpy
	erlang
	haskell-mode
	helm
	ido-vertical-mode
	imenu-anywhere
	magit
	paredit
	pdf-tools
	proof-general
	sicp
	slime
	smex
	tuareg
	yaml-mode
	yasnippet-snippets))

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package rmn/package-list)
  (unless (package-installed-p package)
    (package-install package)))


					; Server
(require 'server)
(if (not (server-running-p))
    (server-start))

(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs))

					; Windows
(defun rmn/load-windows ()
  (let ((cf (expand-file-name "init-windows.el"
			      user-emacs-directory)))
    (if (file-exists-p cf)
	(load cf))))

(when (eq system-type 'windows-nt)
  (add-hook 'after-init-hook #'rmn/load-windows))

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
(size-indication-mode 1)
(global-auto-revert-mode 1)

(setq language-environment "UTF-8")
(setq reb-re-syntax 'string)

(add-hook 'before-save-hook 'whitespace-cleanup)

					; `suspend-frame'
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

					; Terminal encoding settings
(when (eq system-type 'darwin)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8))
(prefer-coding-system 'utf-8)

					; Movement & Editing
(define-key global-map (kbd "M-z") 'zap-up-to-char)

(let ((beg -1)
      (end -1)
      (prev-mid -1))
  (defun backward-binary ()
    (interactive)
    (if (/= prev-mid (point))
	(setq beg -1 end -1)
      (setq end prev-mid))
    (if (< beg 0) (setq beg (line-beginning-position)
			end (point)))
    (setq prev-mid (/ (+ beg end) 2))
    (goto-char prev-mid)
    (set-transient-map rmn/movement-map))
  (defun forward-binary ()
    (interactive)
    (if (/= prev-mid (point))
	(setq beg -1 end -1)
      (setq beg prev-mid))
    (if (< end 0) (setq beg (point)
			end (line-end-position)))
    (setq prev-mid (/ (+ beg end) 2))
    (goto-char prev-mid)
    (set-transient-map rmn/movement-map)))

(defvar rmn/movement-map (make-sparse-keymap)
  "Keymap for bindings related to movements.")

(defmacro rmn/movement-persistent (f)
  `(lambda ()
     (interactive)
     (funcall ,f)
     (set-transient-map rmn/movement-map)))

(defmacro rmn/movement-define-key (k f)
  `(define-key rmn/movement-map ,k (rmn/movement-persistent ,f)))

(define-key global-map (kbd "C-c m") rmn/movement-map)

					; Repeatable keybindings
(rmn/movement-define-key (kbd "n") #'forward-binary)
(rmn/movement-define-key (kbd "p") #'backward-binary)
(rmn/movement-define-key (kbd "}") #'forward-paragraph)
(rmn/movement-define-key (kbd "{") #'backward-paragraph)

					; vim like "Change In"
(defun seek-backward-to-char (chr)
  "Seek backwards to a character"
  (interactive "cSeek back to char: ")
  (while (not (= (char-after) chr))
    (forward-char -1)))

(defun delete-between-pair (char)
  "Delete in between the given pair.  Handles opening and closing
brackets."
  (interactive "cDelete between char: ")
  (let ((pairs (mapcar (lambda (x)
			 (cons (string-to-char (car x))
			       (string-to-char (cdr x))))
		       '(("<" . ">")
			 ("{" . "}")
			 ("(" . ")")
			 ("[" . "]"))))
	endch)
    (setq endch (or (cdr (assoc char pairs))
		    char))
    (seek-backward-to-char char)
    (forward-char 1)
    (zap-to-char 1 endch)
    (insert endch)
    (forward-char -1)))

(define-key global-map (kbd "C-c i") 'delete-between-pair)

					; Crux
(require 'crux)
(crux-reopen-as-root-mode)
(define-key emacs-lisp-mode-map (kbd "C-c e") 'crux-eval-and-replace)
(define-key global-map (kbd "C-c D") 'crux-delete-file-and-buffer)
(define-key global-map (kbd "C-c TAB")
  'crux-indent-rigidly-and-copy-to-clipboard)

					; EXWM
(setq rmn/use-exwm nil)

(when rmn/use-exwm
  (require 'exwm)
  (require 'exwm-config)
  (require 'exwm-systemtray)
  (exwm-enable)
  (exwm-config-default)
  (exwm-systemtray-enable))

					; Comint
(require 'comint)
(setq comint-buffer-maximum-size 2048
      comint-prompt-read-only t)
(define-key global-map (kbd "C-c M-o") 'comint-clear-buffer)

					; Info
(require 'info)
(add-hook 'Info-mode-hook
	  (lambda ()
	    (add-to-list 'Info-directory-list
			 (expand-file-name "~/info"))))

					; Dired
(load-library "dired-x")
(setq dired-dwim-target t
      wdired-allow-to-change-permissions t)

					; Shell
(require 'shell)
(define-key shell-mode-map (kbd "SPC") 'comint-magic-space)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

					; "M-x shell RET" in current buffer
(add-to-list 'display-buffer-alist
	     '("^\\*shell\\*$" . (display-buffer-same-window)))

					; Exec path from shell
(when (eq system-type 'darwin)
  (exec-path-from-shell-initialize))

					; Handling windows
(winner-mode 1)
(define-key global-map (kbd "<f7>") 'winner-undo)
(define-key global-map (kbd "<f8>") 'winner-redo)

(require 'ace-window)
(ace-window-display-mode 1)
(define-key global-map (kbd "C-'") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
      aw-scope 'frame)

					; Finding files
(require 'ffap)
(ffap-bindings)
(setq ffap-require-prefix t)

(setq rmn/use-ido t)
(setq rmn/use-helm (not rmn/use-ido))

					; Ido
(when rmn/use-ido
  (require 'ido)
  ; (require 'ido-vertical-mode)
  (setq ido-enable-flex-matching t
	ido-use-virtual-buffers t
	ido-everywhere t
	ido-use-filename-at-point 'guess
	ido-use-url-at-point t
	;; ido-enable-tramp-completion t ; can make performance slow
	ido-case-fold t
	ido-enable-regexp t)
  (ido-mode 1)
  ; (ido-vertical-mode 1)
  ; (setq ido-vertical-define-keys 'C-n-and-C-p-only)

  (define-key ido-file-dir-completion-map (kbd "C-l")
    'ido-delete-backward-word-updir)
					; M-x -- smex
  (require 'smex)
  (setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
  (smex-initialize)
  (define-key global-map (kbd "M-x") 'smex)
  (define-key global-map (kbd "M-X") 'smex-major-mode-commands)
  (define-key global-map (kbd "C-c C-c M-x") 'execute-extended-command)

					; smex hyphen on space
  (defadvice smex (around space-inserts-hyphen activate compile)
    (let ((ido-cannot-complete-command
	   `(lambda ()
	      (interactive)
	      (if (string= " " (this-command-keys))
		  (insert ?-)
		(funcall ,ido-cannot-complete-command)))))
      ad-do-it)))


					; Helm
(when rmn/use-helm
  (require 'helm)
  (require 'helm-config)
 (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
  ;; rebind tab to run persistent action
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  ;; make this work in a terminal
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
  ;; list actions using C-z
  (define-key helm-map (kbd "C-z") 'helm-select-action)
  ;; global-map settings
  (define-key global-map (kbd "M-x") 'helm-M-x)
  (define-key global-map (kbd "M-y") 'helm-show-kill-ring)
  (define-key global-map (kbd "C-x b") 'helm-mini)
  (define-key global-map (kbd "C-x C-f") 'helm-find-files)
  (define-key global-map (kbd "C-c h o") 'helm-occur)
  (define-key global-map (kbd "C-c h g") 'helm-google-suggest)
  ;; apperance
  (setq helm-split-window-in-side-p t ; helm buffer in current window
	helm-move-to-line-cycle-in-source t
	helm-ff-search-library-in-sexp t
	helm-scroll-amount 8
	helm-ff-file-name-history-use-recentf t
	helm-echo-input-in-header-line t
	helm-autoresize-max-height 0
	helm-autoresize-min-height 20)
  ;; general
  (setq helm-M-x-fuzzy-match t
	helm-buffers-fuzzy-matching t
	helm-recentf-fuzzy-match t
	helm-semantic-fuzzy-match t
	helm-imenu-fuzzy-match t
	helm-apropos-fuzzy-match t
	helm-lisp-fuzzy-completion t)
  (helm-autoresize-mode 1)
  (helm-mode 1))

					; Handle buffers
(define-key global-map (kbd "C-x C-b") 'bs-show)

					; Protect buffers from being killed
(defun rmn/protect-buffers ()
  (let ((protected '("*scratch*" "*Messages*")))
    (dolist (buf protected)
      (with-current-buffer buf
	(emacs-lock-mode 'kill)))))

(add-hook 'after-init-hook #'rmn/protect-buffers)


					; Disabled commands
(put 'narrow-to-region 'disabled nil)
(put 'narrowprg-to-region 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

					; Backups
(setq backup-directory-alist '(("." . "~/.backup")))
(setq make-backup-files t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

					; Mails
(setq mail-user-agent 'gnus-user-agent
      read-mail-command 'gnus)

(setq send-mail-function 'message-send-mail-with-sendmail
      message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program "msmtp"
      message-sendmail-extra-arguments '("-a" "default"))

					; Man
(require 'man)
(setq Man-notify-method 'pushy)

					; Imenu
(define-key global-map (kbd "C-c .") 'imenu-anywhere)

					; Text mode
(add-hook 'text-mode-hook
	  (lambda ()
	    (setq fill-column 74)
	    (turn-on-auto-fill)))

					; Compile
(define-key global-map (kbd "C-c p") 'compile)

					; LaTeX
(setq TeX-auto-save t)
(setq TeX-parse-self t)

					; PDF files
(pdf-loader-install)

					; Magit
(define-key global-map (kbd "C-x g") 'magit-status)
(define-key global-map (kbd "C-c M-g") 'magit-dispatch-popup)
(setq magit-git-executable "git")

					; Company
(require 'company)
(setq company-global-modes '(not shell-mode))
(global-company-mode 1)
(setq company-idle-delay 0)

					; Paredit
(require 'paredit)
(autoload 'enable-paredit-mode "paredit"
  "Pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook #'enable-paredit-mode)

					; Yasnippet
(require 'yasnippet)
(yas-global-mode 1)

					; GDB
(require 'gud)
(setq gdb-show-main t)
(add-hook 'gud-mode-hook #'gdb-many-windows)

					; AucTeX
(setq TeX-parse-self t)
(setq TeX-auto-save t)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

					; LSP
(require 'lsp)
(add-hook 'python-mode-hook #'lsp)

(require 'company-lsp)
(push 'company-lsp company-backends)

					; ATS
(when (file-exists-p (concat (getenv "HOME") "/ats2"))
  (setenv "PATSHOME" (concat (getenv "HOME") "/ats2"))
  (add-to-list 'load-path "~/ats2/utils/emacs/")
  (require 'ats2-mode)
  (require 'flymake-ats2)
  (add-hook 'ats-mode-hook 'flymake-mode))

(defun rmn/setup-ats-mode ()
  (electric-pair-mode t)
  (show-trailing-whitespace t)
  (setq fill-column 76)
  (turn-on-auto-fill))
(add-hook 'ats-mode-hook #'rmn/setup-ats-mode)

					; C
(defun rmn/setup-c-mode ()
  (setq c-default-style "bsd"
	c-basic-offset 4
	tab-width 4
	indent-tabs-mode t
	show-trailing-whitespace t)
  ; (display-line-numbers-mode t)
  (electric-pair-mode t)
  (local-set-key (kbd "M-*") 'pop-tag-mark)
  (local-set-key (kbd "C-c o") 'ff-find-other-file))
(add-hook 'c-mode-common-hook #'rmn/setup-c-mode)

					; Common Lisp
(require 'slime)
(setq inferior-lisp-program "sbcl")

					; Coq
(require 'proof-general)

(defun rmn/setup-coq-mode ()
  (tuareg-opam-update-env "default")
  (turn-on-auto-fill)
  (setq company-coq-disabled-features '(prettify-symbols))
  (company-coq-mode)
  (setq fill-column 78
	show-trailing-whitespace t
	coq-compile-before-require t
	;; Proof General
	proof-auto-raise-buffers nil
	proof-three-window-enable t
	proof-splash-enable nil
	proof-follow-mode 'followdown
	PA-one-command-per-line nil))
(add-hook 'coq-mode-hook #'rmn/setup-coq-mode)

					; Erlang
(require 'erlang)
(add-hook 'erlang-mode-hook #'company-erlang-init)

					; Haskell
(require 'haskell-mode)
(defun rmn/setup-haskell-mode ()
  (setq indent-tabs-mode nil)
  (interactive-haskell-mode 1))
(add-hook 'haskell-mode-hook #'rmn/setup-haskell-mode)

					; OCaml
(require 'tuareg)
(let ((opam-share (ignore-errors
		    (car (process-lines "opam" "config" "var" "share")))))
  (when (and opam-share (file-directory-p opam-share))
    (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
    (autoload 'merlin-mode "merlin" nil t nil)
    (add-hook 'tuareg-mode-hook 'merlin-mode t)
    (add-hook 'caml-mode-hook 'merlin-mode t)))

(defun rmn/setup-tuareg-mode ()
  (tuareg-opam-update-env "default")
  (setq fill-column 78)
  (turn-on-auto-fill)
  (setq show-trailing-whitespace t)
  (when (functionp 'prettify-symbols-mode)
    (prettify-symbols-mode)))
(add-hook 'tuareg-mode-hook #'rmn/setup-tuareg-mode)
(add-hook 'tuareg-mode-hook 'merlin-mode)

					; Merlin and Company
(with-eval-after-load 'company
  (add-to-list 'company-backends 'merlin-company-backend))

					; Python
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")
(require 'elpy)
(elpy-enable)
(defun rmn/setup-elpy-mode ()
  (setq indent-tabs-mode t
	tab-width 4
	python-indent-offset 4)
  (highlight-indentation-mode -1))
(add-hook 'python-mode-hook #'rmn/setup-elpy-mode)
(add-hook 'elpy-mode-hook #'rmn/setup-elpy-mode)
;; Activate default environment
(pyvenv-activate (concat (getenv "HOME") "/pyenv/base"))

					; Scheme
(load-library "xscheme")

					; YAML
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))


					; Org mode
(require 'org)
(require 'org-agenda)
(require 'ox-beamer)
(require 'ox-latex)

(setq org-log-done 'time
      org-enforce-todo-dependencies t
      org-enforce-todo-checkbox-dependencies t)

(setq org-agenda-files '("~/.org/tasks.org")
      org-agenda-window-setup 'current-window
      org-agenda-ndays 7
      org-agenda-show-all-dates t
      org-agenda-start-on-weekday nil
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-use-fast-todo-selection t
      org-use-speed-commands t)

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(t)" "WAITING(w@/!)" "HOLD(h@/!)"
		  "MEETING(m)" "PHONE(p)" "|" "DONE(d)")))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/.org/tasks.org" "Tasks")
	 "** TODO %?\n%i   %u" :empty-lines 1)
	("T" "Todo with link" entry (file+headline "~/.org/tasks.org" "Tasks")
	 "** TODO %?\n%i   %u\n   %A" :empty-lines 1)
	("m" "Meeting" entry (file+headline "~/.org/tasks.org" "Events")
	 "** MEETING %?\n   SCHEDULED: %^T" :empty-lines 1)
	("p" "Phone call" entry (file+headline "~/.org/tasks.org" "Events")
	 "** PHONE %?\n   SCHEDULED: %^T" :empty-lines 1)))

(define-key org-mode-map (kbd "C-'") nil) ; originally `org-cycle-agenda-files'
(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "<f12>") 'org-agenda)
(define-key global-map (kbd "<f9> r") 'org-capture)

(defun rmn/setup-org-mode ()
  (setq fill-column 78)
  (turn-on-auto-fill))
(add-hook 'org-mode-hook #'rmn/setup-org-mode)

(require 'ob)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (emacs-lisp . t)
   (scheme . t)
   (ditaa . t)
   (dot . t)
   (java . t)
   (calc . t)
   (octave . t)
   (makefile . t)))

(setq org-confirm-babel-evaluate nil
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-src-preserve-indentation t
      org-src-window-setup 'current-window)

					; Tramp
(require 'tramp)
(setq tramp-debug-buffer t)
(setq tramp-verbose 10)
(setq tramp-default-method "ssh")

(require 'tramp-cache)
(setq tramp-persistency-file-name
      (expand-file-name "tramp" user-emacs-directory))

(unless (version< emacs-version "26.1")
  (connection-local-set-profile-variables
   'remote-bash
   '((explicit-shell-file-name . "/bin/bash")
     (explicit-bash-args . ("-i"))))

  (connection-local-set-profiles
   '(:application tramp :protocol "ssh" :machine "res")
   'remote-bash))

					; BBDB
(require 'bbdb)
(bbdb-initialize 'gnus 'message)
(setq bbdb/mail-auto-create-p t)
(setq bbdb/news-auto-create-p t)
(setq bbdb-file (expand-file-name ".bbdb" user-emacs-directory)
      bbdb-offer-to-create 'auto
      bbdb-complete-mail-allow-cycling t)

					; EasyPG
					; Use Emacs pinentry
(setenv "GPG_AGENT_INFO" nil)
(setq epa-file-cache-passphrase-for-symmetric-encryption t)

					; Chess
(setq chess-images-default-size 60)

					; Docker
(require 'docker)
(define-key global-map (kbd "C-c d") 'docker)

					; Dockerfile
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

					; Miscellaneous
(defun rmn/change-aws-res-hostname (arg)
  "Change hostname of a particular AWS machine in `.ssh/config'.  This
is required when the ec2 instance is restarted.  The machine can be
accessed by going to `/ssh:res:<dir>'"
  (interactive "MNew Hostname: ")
  (let ((ssh-config-file "~/.ssh/config"))
    (with-current-buffer (find-file ssh-config-file)
      (barf-if-buffer-read-only)
      (save-excursion
	(goto-char (point-min))
	(search-forward "Host res")
	(forward-line)
	(kill-whole-line)
	(insert "     HostName " arg "\n")))))
