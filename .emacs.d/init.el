;;; -*- lexical-binding: t -*-

;;;* User Details
(setq user-full-name "Ramana Nagasamudram"
      user-mail-address "rnagasam@stevens.edu")

;;;* Initialization -- Garbage Collection
(let ((initial-gc-threshold 800000)
      (startup-gc-threshold (* 20 1024 1024)))
  (setq gc-cons-threshold startup-gc-threshold)
  (add-hook 'after-init-hook
            (lambda () (setq gc-cons-threshold initial-gc-threshold))))
(add-hook 'focus-out-hook 'garbage-collect)

;;;* Emacs directory and Custom
;; A simple way to experiment with multiple configs is to change
;; `user-emacs-directory' to point to a location with an alternate
;; `init.el'.  Of course, additional files might have to be copied.
(if (not (boundp 'user-emacs-directory))
    (setq user-emacs-directory "~/.emacs.d/"))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(add-hook 'after-init-hook
	  (lambda ()
	    (when (file-exists-p custom-file)
	      (load custom-file))))

;;;* Packages
(let ((default-directory "~/.emacs.d/site-lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(require 'package)

;; [08/12/2019] "incomprehensible buffer" error with `elpa' is avoided
;; by switching to `http' from `https'.
(setq package-archives
      '(("melpa" . "http://melpa.milkbox.net/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

;;;* Aquamacs
;; [08/12/2019] Not used anymore.
(when (featurep 'aquamacs)
  (one-buffer-one-frame-mode -1)
  (setq special-display-regexps nil)
  (setq aquamacs-scratch-file nil
        initial-major-mode 'emacs-lisp-mode)
  (setq ns-use-mac-modifier-symbols nil
        select-enable-clipboard t))

;;;* Emacs Server
;; [08/12/2019] Some of this might not be required, considering that I
;; usually start emacs by `emacs --daemon'.
;; Take a look at https://www.emacswiki.org/emacs/EmacsAsDaemon.
(require 'server)
(when (not (server-running-p))
  (server-start))

(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs))

;;;* Interface and General settings
(defalias 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-screen t
      inhibit-splash-screen t
      visual-bell 1
      ring-bell-function 'ignore)

(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
(tool-bar-mode -1)

(show-paren-mode 1)
(global-subword-mode 1)
(column-number-mode 1)
(global-auto-revert-mode 1)
(delete-selection-mode 1)

(setq enable-recursive-minibuffers nil)

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; [08/17/2019]
;; Snippet from https://github.com/technomancy/better-defaults
(setq save-interprogram-paste-before-kill t
      apropos-do-all t
      load-prefer-newer t)

;; [08/21/2019] Use emacs-mac port by Yamamoto Mitsuharu
;; Additionally, {M-x customize-group RET mac RET}
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'meta
        mac-command-modifier 'super
        ;; ``Cmd+Left mouse'' as mouse-2
        mac-emulate-three-button-mouse 'reverse)
  (define-key global-map (kbd "s-n") 'make-frame-command)
  (define-key global-map (kbd "s-w") 'delete-frame)
  (define-key global-map (kbd "s-l") 'toggle-frame-maximized)
  (define-key global-map (kbd "<s-right>") 'other-frame)
  (define-key global-map (kbd "<s-left>") (lambda ()
                                            (interactive)
                                            (other-frame -1))))

;; [08/28/2019] Enable icomplete
(defvar rn/use-icomplete nil)
(when rn/use-icomplete
  (setq icomplete-hide-common-prefix t)  
  (icomplete-mode 1)
  (add-hook 'icomplete-minibuffer-setup-hook
            (lambda () (setq-local max-mini-window-height 3))))

;;;** Locate
;; [08/25/2019] See https://www.emacswiki.org/emacs/MacOSTweaks#toc7
;; and mdfind(1)
(setq locate-command "mdfind")

;;;** Indentation (prefer spaces over tabs)
(setq-default indent-tabs-mode nil)
(setq tab-width 4)

;;;** Modeline
;; This snippet sets up the modeline to only show the major mode.
;; Does not seem to work in `org-agenda'.
(setq mode-line-modes
      (mapcar (lambda (elem)
                (pcase elem
                  (`(:propertize (,_ minor-mode-alist . ,_) . ,_)
                   "")
                  (_ elem)))
              mode-line-modes))

;;;** Disabled Commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;;;* Backups
;; I prefer to have a single directory where all backups are stored.
;; Otherwise, the current directory is cluttered.
(setq backup-directory-alist '(("." . "~/.EmacsBackups")))
(setq backup-by-copying t
      delete-old-versions t
      version-control t
      kept-new-versions 5
      kept-old-versions 2)

;;;* Theming
;; I find it easier to use `customize-themes' to change the current
;; theme.  `rn/toggle-theme' is used to toggle between Emacs' default
;; light and dark themes.
(defun rn/toggle-theme ()
  (interactive)
  (let ((togls '(("black" . "white")
		 ("white" . "black")))
	(fg (downcase (face-attribute 'default :foreground)))
	(bg (downcase (face-attribute 'default :background))))
    (set-foreground-color (cdr (assoc fg togls)))
    (set-background-color (cdr (assoc bg togls)))))

;;;* Movement and Editing
;;;** General
(require 'misc)
(define-key global-map (kbd "M-z") 'zap-up-to-char)

;; [08/17/2019]
;; Snippet from https://github.com/technomancy/better-defaults/
(define-key global-map (kbd "C-s") 'isearch-forward-regexp)
(define-key global-map (kbd "C-r") 'isearch-backward-regexp)
(define-key global-map (kbd "C-M-s") 'isearch-forward)
(define-key global-map (kbd "C-M-r") 'isearch-backward)

;; [08/15/2019] Better binding for a command I know is useful but
;; rarely use.
(define-key global-map (kbd "C-c r") 'query-replace-regexp)

;;;** Binary movement
;; Snippet from https://www.emacswiki.org/emacs/EmacsNiftyTricks
;; [08/12/2019] This doesn't get used as much so consider removing it
;; or changing the keybindings.
(let ((beg -1)
      (end -1)
      (prev-mid -1))

  (defun backward-binary ()
    (interactive)
    (if (/= prev-mid (point))
        (setq beg -1
              end -1)
      (setq end prev-mid))
    (if (< beg 0)
        (setq beg (line-beginning-position)
              end (point)))
    (setq prev-mid (/ (+ beg end) 2))
    (goto-char prev-mid))

  (defun forward-binary ()
    (interactive)
    (if (/= prev-mid (point))
        (setq beg -1
              end -1)
      (setq beg prev-mid))
    (if (< end 0)
        (setq beg (point)
              end (line-end-position)))
    (setq prev-mid (/ (+ beg end) 2))
    (goto-char prev-mid)))

;;;** User movement map
;; The idea is to have a persistent map.  This is set to the
;; keybinding `C-c m'.  After `C-c m', any binding defined using
;; `rn/movement-define-key' will continue to persist (until a key not
;; in `rn/movement-map' or `C-g' is pressed).
(defvar rn/movement-map (make-sparse-keymap)
  "Keymap for bindings related to movement and editing.")

(defmacro rn/movement-persistent (f)
  `(lambda ()
     (interactive)
     (funcall ,f)
     (set-transient-map rn/movement-map)))

(defmacro rn/movement-define-key (k f)
  "Define a keybinding for F and make it \"persistent\".  It is best
when K is a single keyboard character.  K can be used repeatedly."
  `(define-key rn/movement-map ,k (rn/movement-persistent ,f)))

(define-key global-map (kbd "C-c m") rn/movement-map)

;;;*** Movement and Editing functions
(defun rn/seek-backward-to-char (chr)
  "Seek backward to CHR."
  (interactive "cSeek backward to: ")
  (while (not (= (char-after) chr))
    (forward-char -1)))

(defun rn/seek-forward-to-char (chr)
  "Seek forward to CHR"
  (interactive "cSeek forward to: ")
  (while (not (= (char-after) chr))
    (forward-char 1)))

(defun rn/delete-between-pair (chr)
  "Delete in between given pair.  Handles opening and closing
brackets."
  (interactive "cDelete between: ")
  (let ((pairs (mapcar (lambda (x)
                         (cons (string-to-char (car x))
                               (string-to-char (cdr x))))
                       '(("<" . ">")
                         ("{" . "}")
                         ("(" . ")")
                         ("[" . "]"))))
        endch)
    (setq endch (or (cdr (assoc chr pairs))
                    chr))
    (rn/seek-backward-to-char chr)
    (forward-char 1)
    (zap-up-to-char 1 endch)))

(defun rn/kill-word ()
  "Kill word at point.  If point is at a word boundary, kill the word
to the right."
  (interactive)
  (if (looking-at "\\b")
      (kill-word 1)
    (backward-word)
    (kill-word 1)))

;;;*** Keybindings
(define-key rn/movement-map (kbd "i") 'rn/delete-between-pair)
(define-key rn/movement-map (kbd "w") 'rn/kill-word)
(define-key rn/movement-map (kbd "F") 'rn/seek-backward-to-char)
(define-key rn/movement-map (kbd "f") 'rn/seek-forward-to-char)

(rn/movement-define-key (kbd "n") 'forward-binary)
(rn/movement-define-key (kbd "p") 'backward-binary)
(rn/movement-define-key (kbd "}") 'forward-paragraph)
(rn/movement-define-key (kbd "{") 'backward-paragraph)
(rn/movement-define-key (kbd "k") 'kill-whole-line)

;;;** Paredit
(require 'paredit)
(autoload 'enable-paredit-mode "paredit"
  "Pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook #'enable-paredit-mode)

;;;* Mail
;; See "~/.gnus.el" for gnus configuration

;;;** Sending
(require 'starttls)
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-auth-credentials (expand-file-name "~/.authinfo.gpg")
      smtpmail-stream-type 'starttls
      smtpmail-smtp-server "outlook.office365.com"
      smtpmail-smtp-service 587)

;;;** Gnus as system's default mail client (macOS)
;; [08/25/2019] See https://www.emacswiki.org/emacs/MacOSTweaks#toc6
(defun switch-to-or-start-gnus ()
  "If there is no *Group*, call (gnus-start)"
  (interactive)
  (let ((group-buffer (get-buffer "*Group*")))
    (if group-buffer
        (switch-to-buffer group-buffer)
      (gnus))))

(setq url-mail-command
      (lambda () (progn (switch-to-or-start-gnus)
                        (gnus-group-mail))))

;;;* Comint
(setq comint-buffer-maximum-size 2048
      comint-prompt-read-only t)
(define-key global-map (kbd "C-c M-o") 'comint-clear-buffer)
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)

;;;* Eshell
(require 'eshell)
(defun rn/setup-eshell ()
  (interactive)
  ;; [08/16/2019] M-RET is bound to `eshell-queue-input' by default.
  ;; Removing this binding lets us use Hyperboles M-RET instead --
  ;; which is far more useful.
  (local-set-key (kbd "M-<RET>") nil))
(add-hook 'eshell-mode-hook 'rn/setup-eshell)

;;;* Dired
(require 'dired)
(setq dired-dwim-target t
      wdired-allow-to-change-permissions t)

(when (eq system-type 'darwin)
  (defun dired-mac-open ()
    (interactive)
    (let ((fname (dired-get-file-for-visit)))
      (if (file-exists-p fname)
          (call-process "/usr/bin/open" nil 0 nil fname))))
  (define-key dired-mode-map "o" 'dired-mac-open))

;;;* Window Management
(winner-mode 1)
(define-key global-map (kbd "<f7>") 'winner-undo)
(define-key global-map (kbd "<f8>") 'winner-redo)

(require 'ace-window)
(ace-window-display-mode 1)
(define-key global-map (kbd "C--") 'ace-window)
(setq aw-keys '(?a ?o ?e ?i ?d ?h ?t ?s)
      aw-scope 'frame)

;;;* Finding Files and File Management
(require 'ffap)
(setq ffap-require-prefix t)
(ffap-bindings)

(defvar rn/use-ido t)
(when rn/use-ido
  (require 'ido)
  (setq ido-enable-flex-matching t
        ido-use-virtual-buffers t
        ido-everywhere t
        ido-use-filename-at-point 'guess
        ido-use-url-at-point t
        ido-case-fold t
        ido-enable-regexp t
        ido-max-window-height 1)
  (define-key ido-file-dir-completion-map (kbd "C-l")
    'ido-delete-backward-word-updir)
  (ido-mode t))

;;;* Smex
(defvar rn/use-smex t)
(when rn/use-smex
  (require 'smex)
  (setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
  (smex-initialize)
  (define-key global-map (kbd "M-x") 'smex)
  (define-key global-map (kbd "M-X") 'smex-major-mode-commands)
  (define-key global-map (kbd "C-c C-c M-x") 'execute-extended-command))


;;;* Handling Buffers
(define-key global-map (kbd "C-x C-b") 'bs-show)

(defun rn/protect-buffers ()
  (let ((protected '("*scratch*" "*Messages*")))
    (dolist (buf protected)
      (with-current-buffer buf
	(emacs-lock-mode 'kill)))))
(add-hook 'after-init-hook 'rn/protect-buffers)

;;;* Completion (company)
(require 'company)
(global-company-mode 1)

;;;* Hyperbole
;; Disable default hyperbole keybindings.  Set `hkey-init' to non-nil
;; to reenable.
(setq hkey-init nil)
(require 'hyperbole)

(setq hpath:display-where 'this-window)

;; [08/21/2019] Is `hmouse-install' required?
(hmouse-unshifted-setup)
(hmouse-install)

;;;** Keybindings
;; Take a look at "(hyperbole)Global Key Bindings".
(define-key global-map (kbd "M-RET") 'hkey-either)
(define-key global-map (kbd "S-<mouse-1>") 'hkey-either)
(define-key global-map (kbd "S-<mouse-3>") 'assist-mouse-key-emacs)
(define-key global-map (kbd "C-c /") 'hui-search-web)
(define-key global-map (kbd "M-o") 'hkey-operate)
(define-key global-map (kbd "C-h A") 'hkey-help)
(define-key global-map (kbd "C-c <RET>") 'hui-select-thing)
(define-key global-map (kbd "C-h h") 'hyperbole)

;;;* Projectile
(require 'projectile)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(setq projectile-project-search-path '("~/"))
(setq projectile-indexing-method 'alien
      projectile-switch-project-action 'projectile-dired)

(projectile-mode 1)

;;;* Outline mode
(require 'outline)
(define-key outline-minor-mode-map (kbd "TAB")
  '(menu-item "" nil :filter
              (lambda (&optional _)
                (when (outline-on-heading-p)
                  'outline-toggle-children))))

(define-key outline-minor-mode-map (kbd "<backtab>")
  (lambda ()
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (outline-show-all)
      ;; (outline-hide-leaves)
      )))

;;;* Version control
;;;** VC
(require 'vc)
(add-hook 'vc-git-log-edit-mode-hook
          (lambda ()
            (set-fill-column 70)
            (turn-on-auto-fill)))

;;;** Magit
(require 'magit)
(define-key global-map (kbd "C-x g") 'magit-status)
(define-key global-map (kbd "C-c M-g") 'magit-dispatch-popup)

;;;* Compilation
(define-key global-map (kbd "C-c k") 'compile)

;;;* Languages
;;;** C
(defun rn/setup-c-mode ()
  (setq c-default-style "bsd"
        c-basic-offset 4
        tab-width 4
        show-trailing-whitespace t)
  (electric-pair-mode t)
  (local-set-key (kbd "M-*") 'pop-tag-mark)
  (local-set-key (kbd "C-c f") 'ff-find-other-file))
(add-hook 'c-mode-common-hook 'rn/setup-c-mode)

;;;** OCaml
(require 'tuareg)
(defun rn/setup-tuareg-mode ()
  (tuareg-opam-update-env "4.07.1")
  (setq show-trailing-whitespace t)
  (when (fboundp 'prettify-symbols-mode)
    (prettify-symbols-mode)))
(add-hook 'tuareg-mode-hook 'rn/setup-tuareg-mode)

(let ((opam-share
       (ignore-errors
	 (car (process-lines "opam" "config" "var" "share")))))
  (when (and opam-share (file-directory-p opam-share))
    (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
    (autoload 'merlin-mode "merlin" nil t nil)
    (add-hook 'tuareg-mode-hook 'merlin-mode t)
    (add-hook 'caml-mode-hook 'merlin-mode t)
    (setq merlin-command 'opam)
    (require 'ocp-indent)))
(with-eval-after-load 'company
  (add-to-list 'company-backends 'merlin-company-backend))

;;;** Coq
(require 'proof-general)
(setq proof-splash-enable nil)
(setq coq-compile-before-require t
      coq-diffs 'on                     ; pg only supports this for Coq >= 8.10
      proof-auto-raise-buffers nil
      proof-three-window-enable t
      proof-follow-mode 'followdown
      PA-one-command-per-line nil)
(setq company-coq-disabled-features '(prettify-symbols smart-subscripts))
(add-hook 'coq-mode-hook 'company-coq-mode)

;;;** Prolog
(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(setq prolog-system 'swi)
(add-to-list 'auto-mode-alist '("\\.pl$" . prolog-mode))

;;;** Standard ML
(require 'sml-mode)

(defun rn/setup-sml-mode ()
  (setq sml-indent-level 2)
  (setq fill-column 78)
  (turn-on-auto-fill))
(add-hook 'sml-mode-hook 'rn/setup-sml-mode)

;;;*** Indentation
;; This indents structures and signatures to look like
;; structure Foo =          signature FOO =
;; ..struct                 ..sig
;; ....{...}                ....{...}
;; ..end;                   ..end;
(defun rn/sml-rules (orig kind token)
  (pcase (cons kind token)
    (`(:before . "d=")
     (if (smie-rule-parent-p "structure" "signature" "functor")
         2
       (funcall orig kind token)))
    (`(:after . "struct") 2)
    (_ (funcall orig kind token))))
(add-hook 'sml-mode-hook
          (lambda ()
            (advice-add smie-rules-function :around #'rn/sml-rules)))

;;;** Twelf
;; TODO: Defer loading until actually required.  Consider adding rules
;; to `auto-mode-alist'.
(setq twelf-root "/Users/rnagasam/.smackage/lib/twelf/v1.7.1/")
(let* ((twelf-root "/Users/rnagasam/.smackage/lib/twelf/v1.7.1/")
       (twelf-init-file (concat twelf-root "emacs/twelf-init.el")))
  (if (file-exists-p twelf-init-file)
      (load twelf-init-file)))

;;;* Document Preperation
;;;** HTML
(require 'html-helper-mode)
(setq auto-mode-alist (cons '("\\.html$" . html-helper-mode) auto-mode-alist))
(setq html-helper-do-write-file-hooks t
      html-helper-build-new-buffer t)

;;;** LaTeX
;; [08/12/2019] auctex installed manually at
;; ~/.emacs.d/site-lisp/auctex-12.1
(load "auctex.el" nil t t)
(load "preview.el" nil t t)

(setq TeX-auto-save t
      TeX-parse-self t)
(setq-default TeX-master nil)

(defun rn/setup-latex ()
  (interactive)
  (visual-line-mode 1)
  (flyspell-mode 1)
  (setq-local fill-column 72)
  (turn-on-auto-fill))

(defun rn/auctex-preview-after-save ()
  "Run `preview-buffer' after saving, if any part of the buffer
was previewed before."
  (interactive)
  (add-hook 'after-save-hook
            (lambda ()
              (interactive)
              (when preview-last-location
                (preview-buffer))) nil t))

(add-hook 'LaTeX-mode-hook 'rn/setup-latex)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'rn/auctex-preview-after-save)

;;;** RefTeX
(require 'reftex)

(setq reftex-plug-into-AUCTeX t)
(setq reftex-default-bibliography '("~/database.bib"))
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

;;;* Org mode
(require 'org)
(require 'org-agenda)
(require 'ox-beamer)
(require 'ox-latex)

(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w@/!)" "VERIFY(v)" "|"
                  "DONE(d)" "CANCELLED(c)")
        (sequence "FUTURE" "NEXT" "REVIEW" "|" "READ")))

(setq org-log-done 'time
      org-enforce-todo-dependencies t
      org-enforce-todo-checkbox-dependencies t)

(setq org-agenda-files '("~/org")
      org-agenda-window-setup 'current-window
      org-agenda-ndays 7)

(setq org-use-fast-todo-selection t
      org-treat-S-cursor-todo-selection-as-state-change t
      org-use-speed-commands t
      org-use-sub-superscripts t
      org-return-follows-link t)

(setq org-default-notes-file "~/org/tasks.org")
(setq org-refile-targets '((nil :maxlevel . 3)
                           (org-agenda-files :maxlevel . 3))
      org-refile-allow-creating-parent-nodes 'confirm)

(setq org-capture-templates
      '(("t" "todo" entry (file "~/org/refile.org")
         "* TODO %?\n%U\n")))

(defun rn/show-agenda-and-todo (&optional arg)
  (interactive "P")
  (org-agenda arg "n"))

(add-to-list 'reftex-cite-format-builtin
             `(org "Org mode citations"
                   ((?\C-m . "cite:%l")
                    (?a . ",%l")
                    (?r . ,(concat
                            "** %t\n"
                            ":PROPERTIES:\n"
                            ":Author(s): %a\n"
                            ":Year: %y\n"
                            ":END:\n"
                            "[[~/Papers/%l.pdf][%l-paper]]")))))

(add-hook 'org-mode-hook
          (lambda () (define-key org-mode-map (kbd "C-c [")
                       (lambda ()
                         (interactive)
                         (let ((reftex-cite-format 'org))
                           (reftex-citation))))))

(define-key global-map (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "C-c z") 'rn/show-agenda-and-todo)
(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "<f12>") 'org-agenda)
(define-key global-map (kbd "C-c c") 'org-capture)

;;;** Org Babel
(require 'ob)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (emacs-lisp . t)
   (scheme . t)
   (shell . t)
   (ditaa . t)
   (dot . t)
   (java . t)
   (calc . t)
   (makefile . t)))

(setq org-confirm-babel-evaluate nil
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-src-preserve-indentation t
      org-src-window-setup 'current-window)

;;;* Miscellaneous
;;;** Digital paper
(setq dpt-addr "10.0.0.170")
(require 'dpt-mode)

;;;*
;;; Local Variables:
;;; outline-regexp: ";;;\\*+\\|\\`"
;;; eval: (outline-minor-mode 1)
;;; eval: (outline-hide-leaves)
;;; End:
