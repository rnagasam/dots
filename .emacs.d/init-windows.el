;; Settings for Windows

					; General
(setq delete-by-moving-to-trash t)
(setq w32-pipe-read-delay 0)

					; Git
(add-to-list 'exec-path "C:/Program Files/Git/bin/")
(setq magit-git-executable "C:/Program Files/Git/cmd/git.exe")

					; Paredit
					; C-S-) changes input languages on Windows
(define-key paredit-mode-map (kbd "C-*") 'paredit-forward-slurp-sexp)


					; Shells
(add-hook 'comint-mode-hook
	  (lambda ()
	    (setq comint-process-echoes t)))

(defun run-bash ()
  "Open a bash shell.  Requires WSL"
  (interactive)
  (let ((shell-file-name "C:/Windows/System32/bash.exe"))
    (shell "*wsl-bash*")))

(defun run-git-bash ()
  "Open a bash shell.  Uses bash installed with Git for windows"
  (interactive)
  (let ((shell-file-name "c:/Program Files/Git/bin/bash.exe"))
    (shell "*git-bash*")))

(defun run-cmdexe ()
  "Run cmd.exe"
  (interactive)
  (let ((shell-file-name "cmd.exe"))
    (shell "*cmd.exe*")))

(defun run-powershell ()
  "Run powershell"
  (interactive)
  (let
      ((pshell-cmd
	"c:/Windows/System32/WindowsPowerShell/v1.0/powershell.exe -Command -"))
    (async-shell-command pshell-cmd nil nil)))
