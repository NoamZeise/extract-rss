(in-package :extract-rss)

(defun extract-rss (webpage &optional folder)
  (let* ((articles (get-articles webpage))
	 (folder-path (if (or (not folder) (equalp folder ""))
			  "./"
			(concatenate 'string folder "/")))
	 (filename (merge-pathnames
		    folder-path
		    (make-pathname :name (xml-file webpage) :type "xml"))))
    (ensure-directories-exist filename)
    (with-open-file
     (f filename
	:direction :output
	:if-exists :supersede
	:if-does-not-exist :create)
     (format f (make-feed webpage
			  (date (first articles))
			  (format nil "~{~a~}"
				  (mapcar 'as-rss-entry articles)))))))


;;; --- Helpers ---

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
