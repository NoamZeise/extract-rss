(in-package :extract-rss)

(defun extract-rss (webpage folder)
  (let ((articles
	 (reverse
	  (remove
	   nil
	   (loop for node in (funcall (extract-article-nodes webpage)
				      (plump:parse (get-page (url webpage))))
		 collect
		 (handler-case (funcall (make-article webpage) node)
		   (error (e)
			  (format t "Couldn't parse article, error ~%~a~%node:~%~a~%~%" e node)
			  nil)))))))
    (with-open-file
     (f (concatenate 'string folder (xml-file webpage) ".xml")
	:direction :output :if-exists :supersede :if-does-not-exist :create)
     (format f (wrap-feed
		webpage
		(date (first articles))
		(format nil "~{~a~}"
			(mapcar
			 'as-rss-entry articles)))))))

(defun get-page (url)
  (handler-case (dex:get (fix-url url) :force-string t)
    (error (e) (progn (format t "Couldn't get webpage ~a, error:~%~a~%" url e) ""))))

(defun fix-url (url)
  (if (equalp (char url 0) #\h)
      url
    (concatenate 'string "http://" url)))
