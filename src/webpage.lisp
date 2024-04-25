(in-package :extract-rss)

(defclass webpage ()
  ((title :initarg :title :accessor title)
   (xml-file :initarg :xml-file :accessor xml-file)
   (url :initarg :url :accessor url)
   (extract-article-nodes :initarg :extract-article-nodes :accessor extract-article-nodes)
   (make-article :initarg :make-article :accessor make-article)))

(defun make-feed (webpage latest entries-str)
  (concatenate
   'string
   "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
   (wrap "feed"
	 (concatenate
	  'string
	  (format nil "~%")
	  "<link href=\"" (url webpage) "\" rel=\"self\" type=\"application/atom+xml\"/>"
	  (format nil "~%")
	  (wrap "generator" "extract-rss" "uri=\"https://github.com/NoamZeise/extract-rss\"")
	  (wrap "updated" latest)
	  (wrap "id" (url webpage))
	  (wrap "title" (title webpage))
	  (format nil "~%")
	  entries-str)
	 "xmlns=\"http://www.w3.org/2005/Atom\"")))
