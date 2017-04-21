
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "browserex"))

(in-package "xytoot")

(defvar-local browser.current-media-index nil)

(defvar browser.*size* 300)
(defvar browser.*position* browserex::XPIS_BOTTOM)

(defun browser.show-media (&optional (n 0))
  (interactive "p")
  (w/entry #0=(media_attachments reblog.media_attachments)
    (whenlet media (or . #0#)
      (setq current-media-index 0)
      (w/json (url) (car media)
	(bx:navigate url
		     :size browser.*size*
		     :position browser.*position*)))))

(defun browser.show-next-media ()
  (interactive)
  (w/entry #0=(media_attachments reblog.media_attachments)
    (whenlet media (or . #0#)
      (when (< current-media-index (1- (length media)))
	(w/json (url) (nth (incf current-media-index) media)
	(bx:navigate url
		     :size browser.*size*
		     :position browser.*position*))))))

(defun browser.show-previous-media ()
  (interactive)
  (w/entry #0=(media_attachments reblog.media_attachments)
    (whenlet media (or . #0#)
      (when (> current-media-index 0)
	(w/json (url) (nth (decf current-media-index) media)
	(bx:navigate url
		     :size browser.*size*
		     :position browser.*position*))))))


(defun browser.show-link ()
  (interactive)
  (whenlet url (focused-url)
    (bx:navigate url
		 :size browser.*size*
		 :position browser.*position*)))

(defun browser.show-avatar ()
  (interactive)
  (w/entry #0=(account.avatar reblog.account.avatar)
	   (whenlet url (or . #0#)
      (bx:navigate url
		   :size browser.*size*
		   :position browser.*position*))))

(let ((m *xytoot-timeline-keymap*))
  (define-key m '(#\b #\m) 'browser.show-media)
  (define-key m '(#\b #\.) 'browser.show-next-media)
  (define-key m '(#\b #\,) 'browser.show-previous-media)
  (define-key m '(#\b #\l) 'browser.show-link)
  (define-key m '(#\b #\x) 'bx:close-browser)
  (define-key m '(#\b #\i) 'browser.show-avatar))
