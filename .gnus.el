(require 'nnir)

					; Primary account
(setq gnus-select-method
      '(nnmaildir "Stevens"
		  (directory "~/Mail/Stevens/")
		  (directory-files nnheader-directory-files-safe)
		  (get-new-mail nil)))

					; Newsgroups
(setq gnus-secondary-select-methods
      '((nntp "news.gmane.org")))
(setq gnus-ignored-newsgroups ""
      gnus-fetch-old-headers 'some
      gnus-inhibit-images nil)

;; (setq mail-user-agent 'gnus-user-agent
;;       read-mail-command 'gnus)

;; (setq send-mail-function 'message-send-mail-with-sendmail
;;       sendmail-program "msmtp")

;; (setq message-send-mail-function 'message-send-mail-with-sendmail
;;       message-sendmail-extra-arguments '("-a" "default"))

(setq gnus-message-archive-group
      '(("Stevens" "nnimap+stevens:Stevens/Sent")))

(setq gnus-use-cache t)

					; HTML emails
(setq mm-text-html-renderer 'gnus-w3m
      gnus-html-frame-width 78)

(unless (boundp 'message-fill-column)
  (add-hook 'messages-mode-hook
	    #'(lambda ()
		(setq fill-column 72)
		(turn-on-auto-fill))))

(defun exit-gnus-on-exit ()
  (if (and (fboundp 'gnus-group-exit)
	   (gnus-alive-p))
      (with-current-buffer (get-buffer "*Group*")
	(let (gnus-interactive-exit)
	  (gnus-group-exit)))))
(add-hook 'kill-emacs-hook 'exit-gnus-on-exit)
