(in-package :extract-rss)

(defun extract-rss (webpage &optional folder)
  "return rss feed xml text, optionally write out to a folder"
  (let* ((articles (get-articles webpage))
	 (feed-text
	  (make-feed webpage
		     (date (first articles))
		     (format nil "~{~a~%~}"
			     (mapcar 'as-rss-entry articles)))))
    (if folder (write-rss-file folder webpage feed-text))
    feed-text))

;;; --- Helpers ---

(defun write-rss-file (folder webpage text)
  (let* ((path (if (equalp folder "")
		   "./"
		 (concatenate 'string folder "/")))
	 (filename (merge-pathnames
		    path
		    (make-pathname :name (xml-file webpage) :type "xml"))))
    (ensure-directories-exist filename)
    (with-open-file
     (f filename
	:direction :output
	:if-exists :supersede
	:if-does-not-exist :create)
     (format f text))))

(defun get-articles (webpage)
  (let ((articles
	 (loop
	  for node in
	  (get-article-nodes webpage)
	  collect
	  (handler-case (funcall (make-article webpage) node)
	    (error (e)
		   (format t "Couldn't parse article, error ~%~a~%node:~%~a~%~%" e node)
		   nil)))))
    (reverse (remove nil articles))))

(defun get-article-nodes (webpage)
  (funcall (extract-article-nodes webpage)
	   (get-page-root (url webpage))))

(defun get-page-root (url)
  (plump:parse (get-page url)))

(defun get-page (url)
  (handler-case (dex:get (fix-url url) :force-string t)
    (error (e) (progn (format t "Couldn't get webpage ~a, error:~%~a~%" url e) ""))))

(defun fix-url (url)
  (if (equalp (char url 0) #\h)
      url
    (format nil "http://~a" url)))
