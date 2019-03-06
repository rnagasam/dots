(require 'nnir)

(setq gnus-select-method
      '(nnimap "outlook"
	       (nnimap-address "outlook.office365.com")
	       (nnimap-server-port "imaps")
	       (nnimap-stream ssl)))

(setq gnus-secondary-select-methods
      '((nntp "news.gmane.org")))

(setq gnus-ignored-newsgroups ""
      gnus-fetch-old-headers 'some
      gnus-inhibit-images nil)

(setq send-mail-function 'smtpmail-sent-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "outlook.office365.com"
      smtpmail-smtp-service 587)

(setq gnus-use-cache t)

(unless (boundp 'message-fill-column)
  (add-hook 'messages-mode-hook
	    #'(lambda ()
		(setq fill-column 72)
		(turn-on-auto-fill))))
