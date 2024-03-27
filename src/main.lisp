(in-package :extract-rss)

(defun extract-rss (webpage folder)
  (let ((articles (get-articles webpage))
	(filename (merge-pathnames
		   (concatenate 'string folder "/")
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
	  (funcall (extract-article-nodes webpage) (get-page-root (url webpage)))
	  collect
	  (handler-case (funcall (make-article webpage) node)
	    (error (e)
		   (format t "Couldn't parse article, error ~%~a~%node:~%~a~%~%" e node)
		   nil)))))
    (reverse (remove nil articles))))

(defun get-page-root (url)
  (plump:parse (get-page url)))

(defun get-page (url)
  (handler-case (dex:get (fix-url url) :force-string t)
    (error (e) (progn (format t "Couldn't get webpage ~a, error:~%~a~%" url e) ""))))

(defun fix-url (url)
  (if (equalp (char url 0) #\h)
      url
    (format nil "http://~a" url)))
