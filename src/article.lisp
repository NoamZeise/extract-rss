(in-package :extract-rss)

(defclass article ()
  ((title    :accessor title    :initarg :title    :initform nil)
   (link     :accessor link     :initarg :link     :initform nil)
   (author   :accessor author   :initarg :author   :initform nil)
   (image    :accessor image    :initarg :image    :initform nil)
   (summary  :accessor summary  :initarg :summary  :initform nil)
   (content  :accessor content  :initarg :content  :initform nil)
   (date     :accessor date     :initarg :date     :initform nil)
   (category :accessor category :initarg :category :initform nil)))

(defmethod print-object ((obj article) out)
	   (print-unreadable-object
	    (obj out :type t)
	    (format out
		    "
title: ~a~%link: ~a~%image: ~a~%author: ~a~%date: ~a~%category: ~a~%summary: ~a~%content: ~a~%"
		    (title obj) (link obj) (image obj) (author obj) (date obj)
		    (category obj) (summary obj) (content obj))))

(defgeneric as-rss-entry (article)
	    (:documentation "turn article into rss entry"))

(defmethod as-rss-entry ((article article))
	   (wrap "entry"
		 (concatenate
		  'string
		  (format nil "~%")
		  (wrap "title" (validify-string (title article)) "type=\"html\"")
		  "<link href=\"" (link article) "\"/>"
		  (format nil "~%")
		  (wrap "id" (link article))
		  (wrap "updated" (date article))
		  (wrap "summary" (validify-string (summary article)))
		  (build-tag (validify-string (content article))
			     "<content type=\"html\">" "</content>")
		  (build-tag (validify-string (category article)) "<category term=\"" "\"/>")
		  "<author><name>" (validify-string (author article)) "</name></author>"
		  (format nil "~%")
		  (build-tag (image article) "<media:thumbnail xmlns:media=\"" "\"/>"))))

(defun build-tag (slot start end)
  (if slot (concatenate 'string start slot end (format nil "~%")) nil))

(defun wrap (tagname text &optional (attribs nil))
  (concatenate 'string "<" tagname (if attribs (concatenate 'string " " attribs) nil)
	       ">" text "</" tagname ">" (format nil "~%")))

(defun validify-string (str)
  (setf str (cl-ppcre:regex-replace-all "&" str "&amp;"))
  (setf str (cl-ppcre:regex-replace-all "<" str "&lt;"))
  (setf str (cl-ppcre:regex-replace-all ">" str "&gt;"))
  (setf str (cl-ppcre:regex-replace-all "\"" str "&quot;"))
  (setf str (cl-ppcre:regex-replace-all "'" str "&apos;")))
