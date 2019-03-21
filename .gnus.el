(require 'nnir)

(setq gnus-select-method
      '(nnmaildir "Stevens"
		  (directory "~/Mail/Stevens/")
		  (directory-files nnheader-directory-files-safe)
		  (get-new-mail nil)))

(setq gnus-secondary-select-methods
      '((nntp "news.gmane.org")))

(setq gnus-ignored-newsgroups ""
      gnus-fetch-old-headers 'some
      gnus-inhibit-images nil)

(setq mail-user-agent 'gnus-user-agent
      read-mail-command 'gnus)

(setq send-mail-function 'message-send-mail-with-sendmail
      sendmail-program "msmtp")

(setq gnus-message-archive-group
      '(("Stevens" "nnimap+stevens:Stevens/Sent")
	(".*" ,(format-time-string "sent/%Y-%m"))))

(setq gnus-use-cache t)

(unless (boundp 'message-fill-column)
  (add-hook 'messages-mode-hook
	    #'(lambda ()
		(setq fill-column 72)
		(turn-on-auto-fill))))
