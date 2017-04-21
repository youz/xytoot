
(in-package "xytoot")

(setq *default-user* "username@mstdn.example.com")
(setq *plugins* nil
      *auto-reload* 300
      *username-style* '(:keyword 1)
      *hashtag-style* '(:keyword 0 :bold)
      *separater-style* '(:foreground 4)
      )

(when (modulep "browserex")
  (load-plugin "browser"))

(defun copy-focused-url ()
  (interactive)
  (whenlet url (focused-url)
    (copy-to-clipboard url)
    (message "copied: ~A" url)))

(defun open-client-url ()
  (interactive)
  (w/entry (application.website)
    (when application.website
      (shell-execute application.website t))))

(let ((m *xytoot-timeline-keymap*))
  (define-key m '(#\c #\c) 'copy-focused-url)
  (define-key m '(#\c #\l) 'open-client-url)
  (define-key m '(#\c #\o) (lambda () (interactive) (find-file "~/.xytoot/config.l")))
  )

