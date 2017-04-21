;;; -*- mode:Lisp; package:xytoot -*-

;;; xytoot - Mastodon client for xyzzy (wip)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'xml-http-request)
  (require 'json))

(defpackage :xytoot
  (:use :lisp :editor))

(in-package "xytoot")
(export '(*default-user*
	  *auto-reload*
	  *timezone-offset*
	  *frame-name*
	  *open-url-by*
	  *username-style*
	  *hashtag-style*
	  *favourite-star-style*
	  *separater-style*
	  *xytoot-timeline-keymap*
	  *post-buffer-keymap*
	  *timeline-filter*))

;;; Consts
(defconstant +client-name+ "xytoot")
(defconstant +client-version+ nil)
(defconstant +client-website+ "https://github.com/youz/xytoot")


;;; Exported vars

;; settings
(defvar *default-user* nil
  "username@hostname")

(defvar *auto-reload* nil
  "auto-reload interval (sec)")

(defvar *timezone-offset* 9)

(defvar *frame-name* "xytoot"
  "frame name for xytoot (set nil if you don't need a frame)")

(defvar *open-url-by* (lambda (url) (shell-execute url t))
  "function for opening url")

;; styles
(defvar *username-style* '(:keyword 0))
(defvar *hashtag-style* '(:keyword 1))
(defvar *favourite-star-style*  '(:foreground 3))
(defvar *separater-style* '(:foreground 14))

;; keymaps
(defvar *xytoot-timeline-keymap* (make-sparse-keymap))
(defvar *post-buffer-keymap* (make-sparse-keymap))

;; etc
(defvar *timeline-filter* nil
  "list of filtering functions for timeline")


;;; Internal vars

;;; access tokens
(defvar *tokens* nil)

;; hooks
(defvar *timeline-reload-hook* nil)
(defvar *post-buffer-mode-hook* nil)

;; etc
(defvar *entity-renderer*
  '(:status       draw-status
    :notification draw-notification
    :account      draw-account)
  "eitity types & renderer functions")


;; default keymaps
(let ((m *xytoot-timeline-keymap*))
  ;; open timeline
  (define-key m #\H 'user::xytoot)
  (define-key m #\F 'user::xytoot-favourites)
  (define-key m #\S 'user::xytoot-search-tag)
  (define-key m #\U 'user::xytoot-search-user)

  ;; cursor
  (define-key m #\j 'forward-entry)
  (define-key m #\k 'backward-entry)
  (define-key m #\l 'next-link)
  (define-key m #\h 'previous-link)
  (define-key m #\RET 'open-link)
  (define-key m #\TAB 'next-link)
  (define-key m #\M-Down 'next-link)
  (define-key m #\M-Up 'previous-link)

  ;; post
  (define-key m #\t 'popup-toot-buffer)
  (define-key m #\@ 'popup-reply-to-buffer)
  (define-key m #\m 'popup-mention-buffer)
  (define-key m #\D 'delete-status)
  (define-key m '(#\f #\a) 'toggle-favourite)
  (define-key m '(#\r #\b) 'reblog)
  #+:not-implemented (define-key m '(#\u #\r) 'unreblog)

  ;; etc
  (define-key m '(#\f #\o) 'follow-focused-user)
  (define-key m '(#\u #\f) 'unfollow-focused-user)
  (define-key m #\R 'timeline-reload)
  (define-key m #\M-r 'toggle-auto-reload)
  (define-key m #\J 'timeline-append-page)
  (define-key m #\Q 'close-timeline-buffer)
  (define-key m #\C 'copy-status-url)
  #+:not-implemented (define-key m '(#\c #\o) 'show-context)
  (define-key m #\e 'expand-focused-url)
  )

(define-key *post-buffer-keymap* '(#\C-c #\C-c) 'post-from-buffer)
(define-key *post-buffer-keymap* '(#\C-x #\k) 'close-post-buffer)


;; link elements
(defvar *url-regexp* "https?://[-0-9A-Za-z_.!~*';/?:@&=+$,%#|]+")
(defvar *hashtag-regexp* "\\(^\\|[ 　,.､｡，．、。]\\)\\([#＃]\\([_a-zA-Z0-9ｦ-ﾟ０-ヶヽ-々ー亜-黑]+\\)\\)")
(defvar *user-regexp* "@\\([_a-zA-Z0-9]+\\)")
(defvar *remote-user-regexp* "\\b\\([_a-zA-Z0-9]+\\)@\\([-a-z.]+\\)")

(defvar *link-regexps*
  (list *url-regexp*
	*hashtag-regexp*
	*remote-user-regexp*
	*user-regexp*))

(defun make-regexp-keyword-list ()
  (compile-regexp-keyword-list
   `((,*hashtag-regexp* t ,*hashtag-style* nil 2)
     (,*remote-user-regexp* t ,*username-style*)
     (,*user-regexp* t ,*username-style*))))


;;; Buffer local data
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct timeline
    "timeline settings and data"
    host        ; instance host name (ex. "mastodon.cloud")
    user        ; username@hostname  (login username)
    token       ; Access Token (load from ~/.xytoot/tokens)
    mode        ; timeline mode name (:home-timeline, :user-timeline, etc)
    entity-type ; type of entities (:status, :notification, :account, etc)
    renderer    ; entity renderer (draw-status, draw-notification, etc)
    apifunc     ; #'api-home-timeline, #'api-user-timeline, etc
    params      ; parameters passed to apifunc
    auto-reload ; auto-reload interval (number of seconds, or nil)
    (unread 0)  ; unread counter
    request     ; xhr-cancel-ticket
    ;; timeline data
    alldata     ; list of all toots
    last-id     ; status id of the newest toot
    first-id    ; status id of the oldest toot
    (page 0)    ; reserved
    ))

(defvar-local buffer-timeline nil)
(defvar-local post-buffer-args nil)


;;; Utility functions & macros
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun symb (&rest args)
    (values (intern (format nil "~{~A~}" args))))

  (defun kwd (&rest args)
    (values (intern (format nil "~{~A~}" args) "keyword")))

  (defmacro w/uniq (names &body body)
    `(let ,(mapcar #'(lambda (s) `(,s (make-symbol ,(symbol-name s))))
		   (if (consp names) names (list names)))
       ,@body))

  (defmacro whenlet (var expr &body body)
    `(let ((,var ,expr)) (when ,var ,@body)))

  (defmacro whilet (var test &body body)
    `(do ((,var ,test ,test))
	 ((not ,var))
       ,@body))
  )

(defmacro json-value (obj key)
  (reduce #'(lambda (o k) `(cdr (assoc ,k ,o :test #'string=)))
	  (split-string (symbol-name key) #\.)
	  :initial-value obj))

(defmacro w/json (keys obj &body body)
  (w/uniq gobj
    (flet ((mkbind (k)
	     (if (consp k)
		 `(,(cadr k) (json-value ,gobj ,(car k)))
	       `(,k (json-value ,gobj ,k)))))
      `(let* ((,gobj ,obj) ,@(mapcar #'mkbind keys))
	 ,@body))))

(defun filter (filter-list xs)
  (flet ((filter1 (x)
	   (reduce #'(lambda (x f) (if x (funcall f x) nil))
		   filter-list
		   :initial-value x)))
    (delete-if-not #'filter1 xs)))

(defun url-encode (str)
  (si:www-url-encode (convert-encoding-from-internal *encoding-utf8n* str)))

(defun entity2char (str)
  (reduce #'(lambda (s pair) (apply #'substitute-string s pair))
	  '(("&gt;" ">") ("&lt;" "<") ("&quot;" "\"") ("&apos;" "'") ("&amp;" "&"))
	  :initial-value str))

(defun html2text (str)
  (entity2char
   (substitute-string
    (substitute-string str "<br />" "\n")
    "<[^>]+>" "")))

(defun parse-date (str)
  (let ((ut (ed:parse-date-string
	     (substitute-string
	      (substitute-string str "[TZ]" " ")
	      "-" "/"))))
    (+ ut (* *timezone-offset* 3600))))

(defun expand-short-url (url &optional depth)
  (while (and (string-match "http://\\([^/]+\\)/\\(.+\\)$" url)
	      (or (null depth) (>= (decf depth) 0)))
    (let ((host (match-string 1))
	  (path (match-string 2)))
      (with-open-stream (cn (connect host 80))
	(format cn "GET /~A HTTP/1.1\nHost: ~A\n\n" path host)
	(let* ((res (read-line cn nil))
	       (status (and (string-match "^HTTP/[0-9.]+ \\([0-9]+\\) .+$" res)
			    (match-string 1))))
	  (unless (string-match "^30." status)
	    (return))
	  (whilet l (read-line cn t)
	    (when (or (string-matchp "^Location: \\(.+\\)$" l) (string= l ""))
	      (return)))
	  (whenlet m (match-string 1)
	    (setq url (if (char= (char m 0) #\/) (concat "http://" host m) m)))))))
    url)

(defmacro w/buffer-modifying ((&optional buf) &body body)
  `(save-excursion
     (set-buffer ,(or buf '(selected-buffer)))
     (setq ed:buffer-read-only nil)
     ,@body
     (set-buffer-modified-p nil)
     (setq ed:buffer-read-only t)))

(defmacro w/popupbuf ((name &optional read-only) &body body)
  (w/uniq (km wc closer)
    `(let ((,km (make-sparse-keymap))
	   (,wc (current-window-configuration)))
       (labels ((,closer ()
		  (interactive)
		  (delete-buffer ,name)
		  (set-window-configuration ,wc)))
	 (define-key ,km #\q #',closer))
       (with-output-to-temp-buffer (,name t)
	 ,@body)
       (setq buffer-read-only ,read-only)
       (use-keymap ,km))))


;;; Plugin system
(defvar *plugins* nil "loaded plugins")

(defun load-plugin (plugin &optional force)
  (when (or force (not (find plugin *plugins* :test #'string=)))
    (let* ((name (string plugin))
	   (path (let ((*load-path* '("~/.xytoot/")))
		   (find-load-path name))))
      (unless path
	(error "plugin not found: ~A" name))
      (load-file path)
      (push name *plugins*)
      t)))


;;; Authentication
(defun load-token-file ()
  (let ((tokenfile "~/.xytoot/tokens"))
    (when (file-exist-p tokenfile)
      (with-open-file (is tokenfile :direction :input)
	(setq *tokens* (read is nil))))))

(defun write-token-file ()
  (let ((tokenfile "~/.xytoot/tokens"))
    (with-open-file (os tokenfile :direction :output
			:if-exists :supersede
			:if-does-not-exist :create)
      (format os "(~%~{ ~S~%~})~%" *tokens*))
    t))

(defun find-token (user)
  (unless (or *tokens* (load-token-file))
    #0=(error "Access token not found for ~S.\nTo get token, run 'xytoot-add-user' command" user))
  (let ((prf (find-if (lambda (prf) (string= user (getf prf :user)))
		      *tokens*)))
    (if prf (getf prf :token) #0#)))

(defun parse-user (user)
  (cond ((string-match (concat "^" *remote-user-regexp*) user)
	 (values (match-string 2) (match-string 1)))
	((string-match (concat "^" *user-regexp*) user)
	 (values nil (match-string 1)))
	(t
	 (error "parse-user unknown format ~A" user))))

(defun username-host (user)
  (nth-value 0 (parse-user user)))

(defun username-user (user)
  (nth-value 1 (parse-user user)))

(defun normalize-user (user default-host)
  (multiple-value-bind (host user) (parse-user user)
    (format nil "~A@~A" user (or host default-host))))


(defun load-client-tokens ()
  (let ((tokenfile "~/.xytoot/clients"))
    (when (file-exist-p tokenfile)
      (with-open-file (is tokenfile :direction :input)
	(read is nil)))))

(defun write-client-tokens (tokens)
  (let ((tokenfile "~/.xytoot/clients"))
    (with-open-file (os tokenfile :direction :output
			:if-exists :supersede
			:if-does-not-exist :create)
      (format os "(~%~{ ~S~%~})~%" tokens)))
  t)

(defun get-registered-client (host)
  (find-if (lambda (cli) (string= host (getf cli :host)))
	   (load-client-tokens)))

(defun register-application (host)
  (unless (api-instance :host host)
    (error "host ~A may not be a Mastodon instance"))
  (let ((res (api-register-application
	      :host host
	      :client_name +client-name+
	      :redirect_uris "urn:ietf:wg:oauth:2.0:oob"
	      :scopes "read write follow"
	      :website +client-website+)))
    (w/json (client_id client_secret) res
      (when (or (null client_id)
		(null client_secret))
	(error "app registration failed: ~A" host))
      (let ((clients (load-client-tokens))
	    (newcli (list :host host :client-id client_id :client-secret client_secret)))
	(write-client-tokens (append clients (list newcli)))
	newcli))))

(defun get-username (host token)
  (whenlet res (api-verify-credentials :host host :token token)
    (w/json (acct) res
      (concat acct "@" host))))

(defun get-access-token-by-code (host)
  (let* ((client (or (get-registered-client host)
		     (register-application host)))
	 (client-id (getf client :client-id))
	 (client-secret (getf client :client-secret))
	 (redirect-uri "urn:ietf:wg:oauth:2.0:oob")
	 (url (format nil "https://~A/oauth/authorize?client_id=~A&redirect_uri=~A&response_type=code"
		      host client-id redirect-uri)))
    (shell-execute url t)
    (let* ((code (read-string "authorization code: "))
	   (res (api-oauth-token :host host
				 :client_id  client-id
				 :client_secret client-secret
				 :grant_type "authorization_code"
				 :redirect_uri redirect-uri
				 :code code))
	   (token (json-value res access_token)))
      (unless token
	(error "authorization failed:~%~S" res))
      (whenlet user (get-username host token)
	(let ((prf (list :user user :token token)))
	  (setq *tokens* (delete-if (lambda (p) (string= user (getf p :user))) *tokens*))
	  (push prf *tokens*)
	  (write-token-file)
	  prf)))))

(defun get-access-token-by-password (host mailaddress password)
  (let* ((client (or (get-registered-client host)
		     (register-application host)))
	 (client-id (getf client :client-id))
	 (client-secret (getf client :client-secret)))
    (whenlet res (api-oauth-token
		  :host host
		  :client_id client-id
		  :client_secret client-secret
		  :grant_type "password"
		  :username mailaddress
		  :password password
		  :scope "read write follow")
      (let ((token (json-value res access_token)))
	(unless token
	  (error "authorization failed:~%~S" res))
	(whenlet user (get-username host token)
	  (let ((prf (list :user username :token token)))
	    (push prf *tokens*)
	    (write-token-file)
	    prf))))))


(defun select-host ()
  (completing-read "Host: "
		   (sort
		    (remove-duplicates
		     (mapcar #'(lambda (p) (getf p :host))
			     (or *tokens* (load-token-file)))
		     :test #'string=)
		    #'string<)
		   :case-fold nil
		   :default (username-host *default-user*)
		   :must-match t))

(defun select-login-user (&optional (prompt "Select login user: "))
  (completing-read prompt
		   (mapcar #'(lambda (p) (getf p :user))
			   (or *tokens* (load-token-file)))
		   :case-fold nil
		   :default *default-user*
		   :must-match t))

(defun select-default-user ()
  (cond (*default-user*
	 *default-user*)
	((= (length *tokens*) 1)
	 (getf (car *tokens*) :user))
	(t (select-login-user))))


;;; API request
(define-condition parameter-error (network-error)
  (host path message)
  (:report
   (lambda (c s)
     (format s "xytoot request parameter error~%~
                path: ~A~A~%~
                error: ~A"
	     (parameter-error-host c)
	     (parameter-error-path c)
	     (parameter-error-message c)))))

(define-condition request-error (network-error)
  (host path method status response)
  (:report
   (lambda (c s)
     (format s "xytoot request failed~%~
                url: ~A~A~%~
                method: ~A~%~
                status: ~A~%~
                response: ~A"
	     (request-error-host c)
	     (request-error-path c)
	     (request-error-method c)
	     (request-error-status c)
	     (request-error-response c)
	     ))))

(defun api-request (host token method path params)
  (let* ((url (concat "https://" host path))
	 (auth (if token (format nil "Bearer ~A" token))))
    (multiple-value-bind (res status headers)
	(if (eq method 'get)
	    (xhr:xhr-get url :query params .
			 #1=(:headers (if auth `(:Authorization ,auth))
			     :encoding *encoding-utf8n*
			     :key #'xhr:xhr-response-values))
	  (xhr:xhr-request method url params . #1#))
      (if (= status 200)
	  (values (json:json-decode res) status headers)
	(error 'request-error
	       :host host
	       :path path :method method
	       :status status :response res)))))

(defun api-request-async (host token method path params
			  &key raw onsuccess onfailure oncomplete handler)
  (let* ((url (concat "https://" host path))
	 (auth (if token (format nil "Bearer ~A" token)))
	 (succ (lambda #0=(res status header)
		 (if onsuccess
		     (funcall onsuccess (json:json-decode res))
		   (message "request ok: ~A ~A" status path))))
	 (fail (or onfailure
		   (lambda #0#
		     (error 'request-error
			    :host host :path path :method method
			    :status status :response res)))))
    (if (eq method 'get)
	(xhr:xhr-get-async url :query params .
			   #1=(:headers (if auth `(:Authorization ,auth))
			       :encoding *encoding-utf8n*
			       :key #'xhr:xhr-response-values
			       :onsuccess succ
			       :onfailure fail
			       :oncomplete oncomplete
			       :handler handler))
      (xhr:xhr-request-async method url params . #1#))))

(defmacro define-api (name
		      &key desc params path-params path
		      (auth t) (method 'get))
  (unless path (error "define-api: missing 'path'"))
  (let ((sync (symb "api-" name))
	(async (symb "api-" name "-async")))
    `(progn
       (defun ,sync (&key host ,@(if auth (list 'token) nil)
			  ,@params ,@path-params)
	 ,@(when desc (list desc))
	 (unless host
	   (error 'parameter-error :host host :path ',path :message "missing host parameter"))
	 ,@(when auth
	     `((when (null token)
		 (error 'parameter-error :host host :path ',path :message "missing access token"))))
	 (api-request host ,(if auth 'token nil)
		      ',method ,path
		      #1=(append
			  ,@(mapcar #'(lambda (p)
					`(if ,p ,(list 'list (kwd p) p)))
				    params))))

       (defun ,async (&key host ,@(if auth (list 'token) nil)
			   ,@params ,@path-params
			   onsuccess onfailure oncomplete handler)
	 ,@(when desc (list desc))
	 (unless host
	   (error 'parameter-error :host host :path ',path :message "missing host parameter"))
	 ,@(when auth
	     `((when (null token)
		 (error 'parameter-error :host host :path ',path :message "missing access token"))))
	 (api-request-async host ,(if auth 'token nil)
			    ',method ,path #1#
			    :onsuccess onsuccess
			    :onfailure onfailure
			    :oncomplete oncomplete
			    :handler handler))
       (export '(,sync ,async)))
    ))


;;; API definitions

;; OAuth
(define-api oauth-token
  :method post
  :auth nil
  :params (client_id client_secret grant_type
	   username password code redirect_uri)
  :path "/oauth/token")

;; Apps
(define-api register-application
  :desc "Registering an application"
  :method post
  :auth nil
  :params (client_name redirect_uris scopes website)
  :path "/api/v1/apps")
#|
- `client_name`: Name of your application
- `redirect_uris`: Where the user should be redirected after authorization (for no redirect, use `urn:ietf:wg:oauth:2.0:oob`)
- `scopes`: This can be a space-separated list of the following items: "read", "write" and "follow" (see [this page](OAuth-details.md) for details on what the scopes do)
- `website`: (optional) URL to the homepage of your app
|#

;; Account
(define-api account
  :desc "Fetching an account"
  :path-params (id)
  :path (format nil "/api/v1/accounts/~D" id))

(define-api verify-credentials
  :desc "Getting the current user"
  :path "/api/v1/accounts/verify_credentials")

(define-api update-credentials
  :desc "Updating the current user"
  :method patch
  :params (display_name note avatar header)
  :path "/api/v1/accounts/update_credentials")

#|
- `display_name`: The name to display in the user's profile
- `note`: A new biography for the user
- `avatar`: A base64 encoded image to display as the user's avatar (e.g. `data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAUoAAADrCAYAAAA...`)
- `header`: A base64 encoded image to display as the user's header image (e.g. `data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAUoAAADrCAYAAAA...`)
|#

(define-api followers
  :desc "Getting an account's followers"
  :params (max_id since_id)
  :path-params (id)
  :path (format nil "/api/v1/accounts/~D/followers" id))

(define-api following
  :desc "Getting who account is following"
  :params (max_id since_id)
  :path-params (id)
  :path (format nil "/api/v1/accounts/~D/following" id))

(define-api statuses
  :desc "Getting an account's statuses"
  :params (only_media exclude_replies max_id since_id)
  :path-params (id)
  :path (format nil "/api/v1/accounts/~D/statuses" id))

(define-api follow
  :desc "Following an account"
  :method post
  :path-params (id)
  :path (format nil "/api/v1/accounts/~D/follow" id))

(define-api unfollow
  :desc "Unfollowing an account"
  :method post
  :path-params (id)
  :path (format nil "/api/v1/accounts/~D/unfollow" id))

(define-api block
  :desc "blocking an account"
  :method post
  :path-params (id)
  :path (format nil "/api/v1/accounts/~D/block" id))

(define-api unblock
  :desc "Unblocking an account"
  :method post
  :path-params (id)
  :path (format nil "/api/v1/accounts/~D/unblock" id))

(define-api mute
  :desc "Muting an account"
  :method post
  :path-params (id)
  :path (format nil "/api/v1/accounts/~D/mute" id))

(define-api unmute
  :desc "Unmuting an account"
  :method post
  :path-params (id)
  :path (format nil "/api/v1/accounts/~D/unmute" id))

(define-api relationships
  :desc "Getting an account's relationships"
  :path-params (id)
  :path "/api/v1/accounts/relationships")

(define-api search-accounts
  :desc "Searching for accounts"
  :params (q limit)
  :path "/api/v1/accounts/search")

;; Blocks
(define-api blocks
  :desc "Fetching a user's blocks"
;  :params (max_id since_id)
  :path "/api/v1/blocks")


;; Favourites
(define-api favourites
  :desc "Fetching a user's favourites"
  :params (max_id since_id)
  :path "/api/v1/favourites")


;; Follow Requests
(define-api follow-requests
  :desc "Fetching a list of follow requests"
;  :params (max_id since_id)
  :path "/api/v1/follow_requests")

(define-api authorize-follow-request
  :desc "Authorizing follow requests"
  :params (id)
  :path "/api/v1/follow_requests/authorize")

(define-api reject-follow-request
  :desc "Rejecting follow requests"
  :params (id)
  :path "/api/v1/follow_requests/reject")

#|
- `id`: The id of the account to authorize or reject
|#

;; Follwing a remote user
(define-api follow-remote
  :desc "Following a remote user"
  :method post
  :params (uri) ;; username@domain
  :path "/api/v1/follows")


;; Instances
(define-api instance
  :desc "Getting instance information"
  :auth nil
  :path "/api/v1/instance")

#|
;; Media
(define-api upload-media
  :desc "Uploading a media attachment"
  :method post
  :params (file)
  :path "/api/v1/media")
|#

;; Mutes
(define-api mutes
  :desc "Fetching a user's mutes"
  :params (max_id since_id)
  :path "/api/v1/mutes")

;; Notifications
(define-api notifications
  :desc "Fetching a user's notifications"
  :params (max_id since_id)
  :path "/api/v1/notifications")

(define-api notification
  :desc "Getting a single notification"
  :path-params (id)
  :path (format nil "/api/v1/notifications/~D" id))

(define-api clear-notifications
  :desc "Clearing notifications"
  :method post
  :path "/api/v1/notifications/clear")

;; Reports
(define-api reports
  :desc "Fetching a user's reports"
  :params (max_id since_id)
  :path "/api/v1/reports")

(define-api make-report
  :desc "Reporting a user"
  :method post
  :params (account_id status_ids comment)
  :path "/api/v1/reports")


;; Search
(define-api search
  :desc "Searching for Content"
  :params (q resolve max_id since_id)
  :path "/api/v1/search")


;; Statuses
(define-api status
  :desc "Fetching a status"
  :path-params (status-id)
  :path (format nil "/api/v1/statuses/~D" status-id))

(define-api context
  :desc "Getting status context"
  :path-params (status-id)
  :path (format nil "/api/v1/statuses/~D/context" status-id))

(define-api card
  :desc "Getting a card associated with a status"
  :path-params (status-id)
  :path (format nil "/api/v1/statuses/~D/card" status-id))

(define-api reblogged-by
  :desc "Getting who reblogged a status"
  :path-params (status-id)
  :path (format nil "/api/v1/statuses/~D/reblogged_by" status-id))

(define-api favourited-by
  :desc "Getting who favourited a status"
  :path-params (status-id)
  :path (format nil "/api/v1/statuses/~D/favourited_by" status-id))

(define-api new-status
  :desc "Posting a new status"
  :method post
  :params (status in_reply_to_id media_ids sensitive spoiler_text visibility)
  :path "/api/v1/statuses")

#|
- `status`: The text of the status
- `in_reply_to_id` (optional): local ID of the status you want to reply to
- `media_ids` (optional): array of media IDs to attach to the status (maximum 4)
- `sensitive` (optional): set this to mark the media of the status as NSFW
- `spoiler_text` (optional): text to be shown as a warning before the actual content
- `visibility` (optional): either "direct", "private", "unlisted" or "public"
|#


(define-api delete-status
  :desc "Deleting a status"
  :method delete
  :path-params (status-id)
  :path (format nil "/api/v1/statuses/~D" status-id))

(define-api reblog
  :desc "Reblogging a status"
  :method post
  :path-params (status-id)
  :path (format nil "/api/v1/statuses/~D/reblog" status-id))

(define-api unreblog
  :desc "Unreblogging a status"
  :method post
  :path-params (status-id)
  :path (format nil "/api/v1/statuses/~D/unreblog" status-id))

(define-api favourite
  :desc "Favouriting a status"
  :method post
  :path-params (status-id)
  :path (format nil "/api/v1/statuses/~D/favourite" status-id))

(define-api unfavourite
  :desc "Unfavouriting a status"
  :method post
  :path-params (status-id)
  :path (format nil "/api/v1/statuses/~D/unfavourite" status-id))


;; Timelines
(define-api home-timeline
  :desc "Retrieving home timeline"
  :params (max_id since_id)
  :path "/api/v1/timelines/home")

(define-api public-timeline
  :desc "Retrieving public timeline"
  :params (local max_id since_id)
  :path "/api/v1/timelines/public")

(define-api tag-timeline
  :desc "Retrieving hashtag timeline"
  :params (local max_id since_id)
  :path-params (tag)
  :path (format nil "/api/v1/timelines/tag/~A" tag))



;;; draw statuses, notifications, accounts, etc
(defun draw-entities (buf entities renderer &key (point 0))
  (with-set-buffer
    (save-excursion
      (set-buffer buf)
      (w/buffer-modifying (buf)
	(with-output-to-buffer (buf point)
	  (dolist (e entities)
	    (let ((wc (min 80 (max 20 (1- (window-columns)))))
		  (start (buffer-stream-point *standard-output*)))
	      (format t " ~V@{-~}~%" (1- wc) t)
	      (funcall renderer e)
	      (apply #'set-text-attribute (1+ start) (+ start wc)
		     (cons :entry e) *separater-style*)))
	      ))
      (recenter))))

(defun draw-status (st)
  (w/json (account.acct account.display_name created_at
		favourited application.name
		spoiler_text reblog.spoiler_text
		content reblog.content
		reblog reblog.favourited
		favourites_count reblog.favourites_count) st
    (let ((date (format-date-string "%y/%m/%d (%a) %H:%M:%S"
				    (parse-date created_at)))
	  (spoiler (or reblog.spoiler_text spoiler_text))
	  (content (or reblog.content content))
	  (fav (max (or favourites_count 0) (or reblog.favourites_count 0))))
      (format t "~A ~@[/ ~A~] [~A] ~:[ ~;★~]"
	      #0=(if (find #\@ account.acct)
		     account.acct
		   (concat "@" account.acct))
	      account.display_name date
	      (or favourited reblog.favourited))
      (let ((p (buffer-stream-point *standard-output*)))
	(apply #'set-text-attribute (1- p) p (cons :fav favourited) *favourite-star-style*))
      (format t "~@[ from ~A~]~%" application.name)
      (when reblog
	(w/json (account.acct reblogs_count) reblog
	  (format t "RT~@[(~A)~] ~A: " reblogs_count #0#)))
      (format t "~A~A~%~@[[☆~A]~%~]"
	      (if (string/= spoiler "") (concat spoiler "\n---\n") "")
	      (html2text content)
	      (and (> fav 0) fav)))))

(defun draw-notification (notify)
  (w/json (type account.acct account.display_name created_at
		status.content) notify
    (let ((date (format-date-string "%y/%m/%d (%a) %H:%M:%S"
				    (parse-date created_at)))
	  (text (and status.content (html2text status.content))))
      (format t "~A ~@[/ ~A~] [~A]~%"
	      (if (find #\@ account.acct)
		     account.acct
		   (concat "@" account.acct))
	      account.display_name date)
      (cond ((string= type "mention")
	     (format t "mentioned to you:~%~A" text))
	    ((string= type "reblog")
	     (format t "reblogged your toot:~%~A" text))
	    ((string= type "favourite")
	     (format t "favourited your toot:~%~A" text))
	    ((string= type "follow")
	     (format t "followed you"))
	    (t (format t "unknown type: ~A" type)))
      (terpri))))

(defun draw-account (account)
  (w/json (id acct username display_name created_at
	      following_count followers_count statuses_count
	      note url) account
    (format t "~A / ~A~%"
	    (if (find #\@ acct) acct (concat "@" acct))
	    display_name)
    (format t "post:~A~%following: ~A~%followers: ~A~%"
	    statuses_count following_count followers_count)
    (format t "url: ~A~%notes:~%~A~%" url (html2text note))))


(defun entry-point (&optional (p (point)))
  (multiple-value-bind (start end tag)
      (find-text-attribute :entry :key #'safe-car :end (1+ p) :from-end t)
    (when start
      (values start (cdr tag)))))

(defun forward-entry ()
  (interactive)
  (whenlet start (find-text-attribute :entry :key #'safe-car :start (1+ (point)))
    (goto-char start)
    (forward-line 2)
    (recenter)))

(defun backward-entry ()
  (interactive)
  (whenlet c (entry-point)
    (whenlet p (entry-point (1- c))
      (goto-char p)
      (forward-line 2)
      (recenter))))

(defun get-link (point pattern &optional (match-index 0))
  (save-excursion
    (goto-char point)
    (when (scan-buffer pattern :reverse t :regexp t)
      (let ((beg (match-beginning 0))
	    (end (match-end 0)))
	(when (<= beg point end)
	  (values (match-string match-index) beg end))))))
  
(defun focused-url ()
  (get-link (point) *url-regexp*))

(defun focused-hashtag ()
  (get-link (point) *hashtag-regexp* 3))

(defun focused-user (&key do-not-search)
  (or (get-link (point) *remote-user-regexp* 0)
      (get-link (point) *user-regexp* 0)))

(defun search-user (target-user login-user)
  (let ((host (username-host login-user)))
    (multiple-value-bind (target-host user) (parse-user target-user)
      (when (or (null target-host)
		(string= host target-host))
	(setq target-user user)))
    (find target-user
	  (api-search-accounts :host host :token (find-token login-user)
			       :q target-user)
	  :key #'(lambda (u) (json-value u acct))
	  :test #'string=)))

(defun get-userid-from-current-entry (username)
  (unless (timeline-buffer-p (selected-buffer))
    (return-from get-userid-from-current-entry))
  (if (eq (timeline-entity-type buffer-timeline) :account)
      (w/entry (id) id)
    (w/entry (account mentions
		      reblog.account reblog.mentions)
      (whenlet user
	  (find username (append (list account reblog.account)
				 mentions reblog.mentions)
		:key #'(lambda (u) (json-value u acct))
		:test #'string=)
	(json-value user id)))))

(defun next-link ()
  (interactive)
  (when (or (focused-hashtag) (focused-user))
    (goto-char (match-end 0)))
  (when (and (scan-buffer (format nil "~{~A~^\\|~}" *link-regexps*) :regexp t :no-dup t)
	     (equal "#" (buffer-substring (1+ (point)) (+ (point) 2))))
    (forward-char)))

(defun previous-link ()
  (interactive)
  (when (or (focused-hashtag) (focused-user))
    (goto-char (match-beginning 0)))
  (when (and (scan-buffer (format nil "~{~A~^\\|~}" *link-regexps*) :regexp t :no-dup t :reverse t)
	     (equal "#" (buffer-substring (1+ (point)) (+ (point) 2))))
    (forward-char)))

(defun expand-focused-url (&optional d)
  (interactive "p")
  (multiple-value-bind (url beg end) (focused-url)
    (whenlet url (and url (expand-short-url url d))
      (w/buffer-modifying ()
	(delete-region beg end)
	(with-output-to-buffer ((selected-buffer) beg)
	  (format t "~A" url)))
      url)))

(defun open-link ()
  (interactive)
  (let ((login-user (timeline-user buffer-timeline))
	(user (focused-user))
	(tag (focused-hashtag)))
    (cond (user
	   (let* ((host (username-host login-user))
		  (user (normalize-user user host))
		  (id (or (get-userid-from-current-entry user)
			  (json-value (search-user user login-user) id))))
	     (unless id
	       (error "user-id for ~A not found" user))
	     (when (y-or-n-p "open *~A* :" user)
	       (tl-user id user login-user))))
	  (tag (when (y-or-n-p "open *#~A* :" tag)
		 (tl-hashtag tag login-user)))
	  (t
	   (whenlet url (expand-focused-url)
	     (when (y-or-n-p "open ~A :" url)
	       (if *open-url-by*
		   (funcall *open-url-by* url)
		 (shell-execute url t))))))))


;;; reload
(defun timeline-reload (&optional (buf (selected-buffer)))
  (interactive)
  (when (eq (buffer-local-value buf 'ed:buffer-mode) 'xytoot-timeline-mode)
    (let* ((name  (substitute-string (buffer-name buf) " ([0-9]+)$" ""))
	   (tl (buffer-local-value buf 'buffer-timeline))
	   (renderer (timeline-renderer tl)))
      (whenlet req #0=(timeline-request tl)
	(xhr:xhr-abort req))
      (setf #0#
	    (apply (timeline-apifunc tl)
		   :since_id #1=(timeline-last-id tl)
		   :onsuccess
		   (lambda (res)
		     (when res
		       (setf #1# (json-value (car res) id))
		       (unless #2=(timeline-first-id tl)
			 (setf #2# (json-value (car (last res)) id)))
		       (whenlet statuses (filter *timeline-filter* res)
			 (incf #3=(timeline-unread tl) (length statuses))
			 (setf #4=(timeline-alldata tl) (append statuses #4#))
			 (draw-entities buf statuses renderer)))
		     (setf #0# nil)
		     (rename-buffer (format nil "~A~[~:; (~D)~]" name #3# #3#) buf)
		     (save-excursion
		       (set-buffer buf)
		       (run-hook-with-args '*timeline-reload-hook* res))
		     (refresh-screen))
		   :onfailure
		   (lambda (res status header)
		     (setf #0# nil)
		     (message "HTTP ~D: reload failed" status))
		   (timeline-params tl)))
      t)))

(defun set-auto-reload (buf interval)
  (whenlet tl (buffer-local-value buf 'buffer-timeline)
    (whenlet ar #1=(timeline-auto-reload tl)
      (stop-timer ar))
    (setf #1# `(lambda ()
		 (unless (minibuffer-window-p (selected-window))
		   (timeline-reload ,buf))))
    (start-timer (max 30 interval) #1#)
    t))

(defun disable-auto-reload (buf)
  (whenlet tl (buffer-local-value buf 'buffer-timeline)
    (whenlet ar #1=(timeline-auto-reload tl)
      (stop-timer ar)
      (setf #1# nil))))

(defun toggle-auto-reload ()
  (interactive)
  (message "auto reload: ~:[off~;on (~A sec)~]"
	   (if (timeline-auto-reload buffer-timeline)
	       (disable-auto-reload (selected-buffer))
	     (when *auto-reload*
	       (set-auto-reload (selected-buffer) *auto-reload*)))
	   *auto-reload*))

(defun timeline-append-page ()
  (interactive)
  (when (eq buffer-mode 'xytoot-timeline-mode)
    (let* ((buf (selected-buffer))
	   (tl buffer-timeline)
	   (params (timeline-params tl))
	   (renderer (timeline-renderer tl)))
      (whenlet req #0=(timeline-request tl)
	(xhr:xhr-abort req))
      (if #2=(getf params :page)
	(incf #2#)
	(whenlet id #1=(timeline-first-id tl)
	  (setq params `(:max_id ,(1- id) ,@params))))
      (setf #0#
	    (apply (timeline-apifunc tl)
		   :onsuccess
		   (lambda (res)
		     (whenlet statuses (filter *timeline-filter* res)
		       (save-excursion
			 (set-buffer buf)
			 (draw-entities buf statuses renderer
					:point (point-max))
			 (refresh-screen))
		       (setf #1# (json-value (car (last statuses)) id)
			     #0# nil)
		       (nconc (timeline-alldata tl) statuses)))
		   :onfailure
		   (lambda (res status header)
		     (message "HTTP ~D: reload failed" status))
		   params)))
    t))


;;; major mode
(defun xytoot-init ()
  (unless (or *tokens* (load-token-file))
    (xytoot-add-user))
  (when (file-exist-p (merge-pathnames ".xytoot/config.l" (si:system-root)))
    (load-plugin "config"))
  t)

(defun timeline-on-activate ()
  (when (eq buffer-mode 'xytoot-timeline-mode)
    (setf (timeline-unread buffer-timeline) 0)))

(defun xytoot-timeline-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq buffer-mode 'xytoot-timeline-mode
	mode-name "xytoot"
	kept-undo-information nil
	need-not-save t
	buffer-read-only t
	auto-save nil)
  (unless (local-variable-p 'regexp-keyword-list)
    (make-local-variable 'regexp-keyword-list))
  (setq regexp-keyword-list
	(append regexp-keyword-list (make-regexp-keyword-list)))
  (set-buffer-fold-type-window)
  (use-keymap *xytoot-timeline-keymap*)
  (setq buffer-timeline (make-timeline))
  (add-hook 'ed:*post-command-hook* 'timeline-on-activate)
  ; delete timer
  (make-local-variable #0='ed:*before-delete-buffer-hook*)
  (add-hook #0#
	    (lambda (buf)
	      (when (eq buffer-mode 'xytt-timeline-mode)
		(disable-auto-reload buf))
	      t)))


(defun timeline-buffers ()
  (delete-if-not #'(lambda (buf) (eq (buffer-local-value buf 'ed:buffer-mode)
				     'xytoot-timeline-mode))
		 (buffer-list)))

(defun get-timeline-buffer-create (name)
  (or (find-if #'(lambda (buf) (string-match (concat "^" (regexp-quote name)) (buffer-name buf)))
	       (timeline-buffers))
      (create-new-buffer name)))


;;; define timeline function
(defmacro define-tl (name params
			  &key buffer-name login-user api-func api-params
			  entity-type hook auto-reload)
  (unless (or (null api-func) (symbolp api-func))
    (error "api-func parameter must be a symbol of function"))
  (unless entity-type
    (error "missing :entity-type parameter"))
  (w/uniq (ghost gbuf gparams grenderer)
    `(defun ,name ,params
       (unless (xytoot-init) (quit))
       (let* ((,ghost (username-host ,login-user))
	      (,gbuf (get-timeline-buffer-create ,buffer-name))
	      (,gparams (list :host (username-host ,login-user)
			      :token (find-token ,login-user)
			      ,@api-params))
	      (,grenderer (getf *entity-renderer* ,entity-type)))
	 (handler-case
	     (progn
	       (unless (or (functionp ,grenderer)
			   (and (symbolp ,grenderer)
				(fboundp ,grenderer)))
		 (error "renderer function for entity type :~A not found" ,entity-type))
	       (when (symbolp ,grenderer)
		 (setq ,grenderer (symbol-function ,grenderer)))
	       (when *frame-name*
		 (select-pseudo-frame (or (find-pseudo-frame *frame-name*)
					  (new-pseudo-frame *frame-name*))))
	       (set-buffer ,gbuf)
	       (unless (eq buffer-mode 'xytoot-timeline-mode)
		 (xytoot-timeline-mode)
		 (setf (timeline-mode buffer-timeline) ,(kwd name)
		       (timeline-user buffer-timeline) ,ghost
		       (timeline-user buffer-timeline) ,login-user
		       (timeline-apifunc buffer-timeline) #',api-func
		       (timeline-params buffer-timeline) ,gparams
		       (timeline-entity-type buffer-timeline) ,entity-type
		       (timeline-renderer buffer-timeline) ,grenderer)
		 (whenlet interval (or ,auto-reload *auto-reload*)
		   (unless (<= interval 0)
		     (set-auto-reload ,gbuf interval))))
	       (timeline-reload)
	       ,(when hook `(run-hooks ,hook))
	       t)
	   (error (c)
	     (msgbox "~A" c)
	     (delete-buffer buf)))))))


(define-tl tl-home (login-user)
  :login-user login-user
  :buffer-name (format nil "*~A: home*" login-user)
  :api-func api-home-timeline-async
  :entity-type :status)

(define-tl tl-notifications (login-user)
  :login-user login-user
  :buffer-name (format nil "*~A: notifications*" login-user)
  :api-func api-notifications-async
  :entity-type :notification)

(define-tl tl-user (userid username login-user)
  :login-user login-user
  :buffer-name (format nil "*~A*" username)
  :api-func api-statuses-async
  :api-params (:id userid)
  :entity-type :status)

(define-tl tl-hashtag (hashtag login-user)
  :login-user login-user
  :buffer-name (format nil "*~A/#~A*" (username-host login-user) hashtag)
  :api-func api-tag-timeline-async
  :api-params (:tag (url-encode hashtag))
  :entity-type :status)

(define-tl tl-favourites (login-user)
  :login-user login-user
  :buffer-name (format nil "*~A: favs*" login-user)
  :api-func api-favourites-async
  :entity-type :status
  :auto-reload -1)

(define-tl tl-search-accounts (q login-user)
  :login-user login-user
  :buffer-name (format nil "*user search: ~A*" q)
  :api-func api-search-accounts-async
  :api-params (:q q)
  :auto-reload -1
  :entity-type :account)


;;; user commands
(defun user::xytoot-add-user (host)
  (interactive "sHost: ")
  (whenlet prf (get-access-token-by-code host)
    (msgbox "Added ~A" (getf prf :user))))

(defun user::xytoot (&optional login-user)
  (interactive "P")
  (unless (xytoot-init) (quit))
  (cond
   (*prefix-args*
    (setq login-user (select-login-user)))
   ((null login-user)
    (setq login-user (select-default-user))))
  (when *frame-name*
    (select-pseudo-frame (or (find-pseudo-frame *frame-name*)
			     (new-pseudo-frame *frame-name*))))
  (when (= (count-windows) 1)
    (split-window-vertically))
  (other-window 0)
  (tl-home login-user)
  (other-window 1)
  (tl-notifications login-user)
  (move-previous-window))

(defun user::xytoot-search-user (user &optional login-user)
  (interactive "sUsername: \nP")
  (unless (xytoot-init) (quit))
  (cond
   (*prefix-args*
    (setq login-user (select-login-user)))
   ((null login-user)
    (setq login-user (select-default-user))))
  (tl-search-accounts user login-user))

(defun user::xytoot-search-tag (hashtag &optional login-user)
  (interactive "sHashtag: #\nP")
  (unless (xytoot-init) (quit))
  (cond
   (*prefix-args*
    (setq login-user (select-login-user)))
   ((null login-user)
    (setq login-user (select-default-user))))
  (tl-hashtag hashtag login-user))

(defun user::xytoot-favourites (&optional login-user)
  (interactive "P")
  (unless (xytoot-init) (quit))
  (cond
   (*prefix-args*
    (setq login-user (select-login-user)))
   ((null login-user)
    (setq login-user (select-default-user))))
  (tl-favourites login-user))



;;; actions
(defmacro w/entry (keys &body body)
  `(multiple-value-bind (#:s #1=#:tag) (entry-point)
     (when #1# (w/json ,keys #1# ,@body))))

(defun show-input-length ()
  (let ((chars (- (point-max) (point-min))))
    (message "~:[!! ~;~]~D 文字 (残り ~D)" (<= chars 500) chars (- 500 chars))))

(defun read-status (prompt &key default)
  (add-hook '*post-command-hook* #'show-input-length)
  (labels ((remover (buffer history)
	     (delete-hook '*post-command-hook* #'show-input-length)
	     (delete-hook '*exit-minibuffer-hook* #'remover)))
    (add-hook '*exit-minibuffer-hook* #'remover))
  (read-string prompt :default default))


(defun toot ()
  (interactive)
  (unless (xytoot-init) (quit))
  (unless *default-user*
    (error "Set xytoot:*default-user* first"))
  (let ((status (read-status "Toot: ")))
    (when (string/= status "")
      (api-new-status :host (username-host *default-user*)
		      :token (find-token *default-user*)
		      :status status)
      (timeline-reload))))

(defun reblog ()
  (interactive)
  (unless (and (timeline-buffer-p (selected-buffer))
	       (eq (timeline-entity-type buffer-timeline) :status))
    (return-from reblog))
  (w/entry (id account.username content)
    (let ((user (timeline-user buffer-timeline)))
      (when (y-or-n-p "Reblog? ~A:~A ~:[...~;~]"
		      account.username
		      (subseq content 0 (min #1=(length content) 50))
		      (< #1# 50))
	(api-reblog :host (username-host user) :token (find-token user)
		    :status-id id)))))

;; fixme
(defun unreblog ()
  (interactive)
  (unless (and (timeline-buffer-p (selected-buffer))
	       (eq (timeline-entity-type buffer-timeline) :status))
    (return-from unreblog))
  (w/entry (id account.username content)
    (let ((user (timeline-user buffer-timeline)))
      (when (y-or-n-p "Unreblog? ~A:~A ~:[...~;~]"
		      account.username
		      (subseq content 0 (min #1=(length content) 50))
		      (< #1# 50))
	(api-reblog :host (username-host user) :token (find-token user)
		    :status-id id)))))

(defun delete-status ()
  (interactive)
  (unless (and (timeline-buffer-p (selected-buffer))
	       (eq (timeline-entity-type buffer-timeline) :status))
    (return-from delete-status))
  (w/entry (id account.username content)
    (let ((user (timeline-user buffer-timeline))
	  (host (username-host user))
	  (text (html2text content)))
      (when (y-or-n-p "Delete? ~A:~A ~:[...~;~]"
		      account.username
		      (subseq text 0 (min #1=(length text) 50))
		      (< #1# 50))
	(api-delete-status-async :host (username-host user) :token (find-token user)
				 :status-id id)))))


(defun toggle-favourite ()
  (interactive)
  (multiple-value-bind (from status) (entry-point)
    (multiple-value-bind (sfrom sto stag)
	(find-text-attribute :fav :start from :key #'safe-car)
      (when sfrom
	(let ((buf (selected-buffer))
	      (state (cdr stag))
	      (mk (make-marker))
	      (user (timeline-user buffer-timeline)))
	  (set-marker mk sfrom)
	  (labels ((toggle (s)
		     (save-excursion
		       (set-buffer buf)
		       (w/buffer-modifying ()
			 (goto-char (marker-point mk))
			 (delete-char)
			 (insert (if s "★" " "))
			 (apply #'set-text-attribute (1- (point)) (point)
				(cons :fav s) (if s *favourite-star-style*))
			 (delete-marker mk)))
		     (refresh-screen)))
	    (funcall (if state #'api-unfavourite-async #'api-favourite-async)
		     :status-id (json-value status id)
		     :host (username-host user)
		     :token (find-token user)
		     :onsuccess (lambda (res) (toggle (not state)))
		     :onfailure
		     (lambda (res status headers)
		       (when (= status 403)
			 (toggle (not state)))))))))))

(defun follow-focused-user ()
  (interactive)
  (unless (timeline-buffer-p (selected-buffer))
    (return-from follow-focused-user))
  (let ((login-user (timeline-user buffer-timeline)))
    (whenlet user (or (focused-user)
		      (w/entry (account.acct acct)
			(or account.acct acct)))
      (when (y-or-n-p "follow ~A ?" user)
	(let ((id (or (get-userid-from-current-entry user)
		      (json-value (search-user user login-user) id))))
	  (unless id
	    (error "user-id for ~A not found" user))
	  (api-follow-async
	   :host (username-host login-user)
	   :token (find-token login-user)
	   :id id
	   :onsuccess
	   (lambda (&rest args)
	     (message "followed: ~A" user))))))))

(defun unfollow-focused-user ()
  (interactive)
  (unless (timeline-buffer-p (selected-buffer))
    (return-from unfollow-focused-user))
  (let ((login-user (timeline-user buffer-timeline)))
    (whenlet user (or (focused-user)
		      (w/entry (account.acct acct)
			(or account.acct acct)))
      (when (y-or-n-p "unfollow ~A  (from ~A)?" user login-user)
	(let ((id (or (get-userid-from-current-entry user)
		      (json-value (search-user user login-user) id))))
	  (unless id
	    (error "user-id for ~A not found" user))
	  (api-unfollow-async
	   :host (username-host login-user)
	   :token (find-token login-user)
	   :id id
	   :onsuccess
	   (lambda (&rest args)
	     (message "unfollowed: ~A" user))))))))


;;; post buffer
(defun post-buffer-count-chars ()
  (let ((c (length (buffer-substring (point-min) (point-max)))))
    (setq mode-line-format
	  (format nil "~A (~A) 残り ~D 文字"
		  (getf post-buffer-args :name)
		  mode-name (- 500 c)))))

(defun post-buffer-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq buffer-mode 'post-buffer-mode
	mode-name "xytoot-post-buffer"
	need-not-save t
	kept-undo-information 1000)
  (make-local-variable 'mode-line-format)
  (use-keymap *post-buffer-keymap*)
  (make-local-variable '#0=regexp-keyword-list)
  (setq #0# (append #0# (make-regexp-keyword-list)
		    (compile-regexp-keyword-list
		     '(("^-\\{20,\\}" nil (:color 14))))))
  (make-local-variable 'ed:*post-command-hook*)
  (set-buffer-fold-type-window)
  (add-hook 'ed:*post-command-hook* 'post-buffer-count-chars)
  (run-hooks '*post-buffer-mode-hook*))


(defun popup-post-buffer (&key name user params default
			       reload ref cursor-top
			       (api #'api-new-status-async)
			       (status-field :status))
  (let ((buf (get-buffer-create name))
	(wc (current-window-configuration))
	(w (window-columns)))
    (split-window -8 nil)
    (set-buffer buf)
    (erase-buffer buf)
    (post-buffer-mode)
    (setq post-buffer-args
	  (list :name name :user user :params params :reload reload :ref ref
		:api api :status-field status-field))
    (with-output-to-buffer (buf)
      (format t "[~A]   Post:C-c C-c   Close:C-x k~%~@[~A~%~]" user ref)
      (format t "~V@{-~}~%" (1- w) t))
    (narrow-to-region #0=(point-max) #0#)
    (when default
      (if cursor-top (save-excursion #1=(insert default)) #1#))
    (make-local-variable #2='ed:*before-delete-buffer-hook*)
    (post-buffer-count-chars)
    (flet ((restore-wc (buf)
	     (ignore-errors (set-window-configuration wc)) t))
      (add-hook #2# #'restore-wc))))

(defun close-post-buffer ()
  (interactive)
  (when (eq buffer-mode 'post-buffer-mode)
    (delete-buffer (selected-buffer))))

(defun post-from-buffer ()
  (interactive)
  (let* ((text (buffer-substring (point-min) (point-max)))
	 (args post-buffer-args)
	 (user (getf post-buffer-args :user)))
    (apply (getf post-buffer-args :api)
	   :host (username-host user)
	   :token (find-token user)
	   (getf post-buffer-args :status-field) text
	   :onsuccess
	   (lambda (res)
	     (whenlet tlbuf (getf args :reload)
	       (start-timer 1 (lambda () (timeline-reload tlbuf)) t))
	     (message "Done."))
	   :onfailure
	   (lambda (res status headers)
	     (message "Failed: ~A ~A" status res)
	     (apply #'popup-post-buffer :default text :cursor-top t args)
	     (refresh-screen))
	   (getf args :params))
    (close-post-buffer)))

(defun popup-toot-buffer ()
  (interactive)
  (popup-post-buffer
   :name "*toot*"
   :user (timeline-user buffer-timeline)
   :reload (selected-buffer)))

(defun popup-mention-buffer ()
  (interactive)
  (w/entry (account.acct)
    (let ((user (focused-user)))
      (popup-post-buffer
       :name "*toot*"
       :user (timeline-user buffer-timeline)
       :default (concat "@" (or user account.acct) " ")
       :reload (selected-buffer)))))

(defun popup-reply-to-buffer ()
  (interactive)
  (unless (eq (timeline-entity-type buffer-timeline) :status)
    (return-from popup-reply-to-buffer))
  (w/entry (id account.acct content)
    (popup-post-buffer
     :name "*reply-to*"
     :user (timeline-user buffer-timeline)
     :params (list :in_reply_to_id id)
     :ref (format nil "reply-to @~A: ~A" account.acct (html2text content))
     :default (concat "@" account.acct " ")
     :reload (selected-buffer))))

#+:nil
(defun popup-send-message-buffer ()
  (interactive)
  (w/entry (user.id user.screen_name sender.id sender.screen_name)
    (let* ((name (or sender.screen_name user.screen_name))
	   (id (or sender.id user.id)))
      (popup-post-buffer
       :name "*DM*"
       :params (list :user_id id)
       :api #'api-direct-messages-new-async
       :status-field :text
       :ref (format nil "DirectMessage to @~A" name)
       :reload (selected-buffer)))))



;;; utilities for timeline buffer
(defun status-url ()
  (w/entry (url reblog.url)
    (or reblog.url url)))

(defun copy-status-url ()
  (interactive)
  (let ((url (status-url)))
    (copy-to-clipboard url)
    (message "Copied: ~A" url)))

(defun show-irt-status ()
  (interactive)
  (w/entry (id . #0=(in_reply_to_status_id retweeted_status.in_reply_to_status_id))
    (whenlet irtid (or . #0#)
      (whenlet irtst (api-show-status :id irtid)
	(w/json (user.screen_name text) irtst
	  (popup-string (format nil "@~A: ~A" user.screen_name text)
			(point)))))))

(defun timeline-buffer-p (buf)
  (eq (buffer-local-value buf 'ed:buffer-mode) 'xytoot-timeline-mode))

(defun close-timeline-buffer (&optional (buf (selected-buffer)))
  (interactive)
  (when (timeline-buffer-p buf)
    (disable-auto-reload buf)
    (kill-buffer buf)
    (when (and *frame-name* (notany #'timeline-buffer-p (buffer-list)))
      (delete-pseudo-frame (find-pseudo-frame *frame-name*)))))


(defun show-jsondata ()
  (interactive)
  (multiple-value-bind (s json) (entry-point)
    (w/popupbuf ("*tw:jsondata*")
      (write json)
      (lisp-mode)
      (while (scan-buffer ")+" :regexp t :tail t)
	(unless (eq (parse-point-syntax) :string)
	  (insert "\n")))
      (beginning-of-buffer)
      (indent-region (point-min) (point-max)))))

(define-key *xytoot-timeline-keymap* #\T 'show-jsondata)


;;; for edit
(setf (get 'w/uniq #1='ed:lisp-indent-hook) 1
      (get 'whenlet #1#) 2
      (get 'whilet #1#) 2
      (get 'w/json #1#) 2
      (get 'w/entry #1#) 1
      (get 'w/buffer-modifying #1#) 1
      (get 'w/popupbuf #1#) 1
      (get 'define-api #1#) 1
      (get 'define-tl #1#) 2)


(provide "xytoot")
