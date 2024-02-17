(in-package :extract-rss)

(defun extract-rss (webpage)
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
     (f "feed.xml" :direction :output :if-exists :supersede :if-does-not-exist :create)
     (format f (wrap-feed
		webpage
		(date (first articles))
		(format nil "~{~%~a~%~}"
			(mapcar
			 'as-rss-entry articles)))))))

(defun wrap-feed (webpage latest entries-str)
  (concatenate
   'string
   "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
   (wrap "feed"
	 (concatenate
	  'string
	  "<link href=\"" (url webpage) "\" rel=\"self\" type=\"application/atom+xml\"/>"
	  (wrap "generator" "extract-rss" "uri=\"https://www.noamzeise.com\"")
	  (wrap "updated" latest)
	  (wrap "id" (url webpage))
	  (wrap "title" (title webpage))
	  entries-str)
	 "xmlns=\"http://www.w3.org/2005/Atom\"")))

(defun get-page (url)
  (handler-case (dex:get (fix-url url) :force-string t)
    (error (e) (progn (format t "Couldn't get webpage ~a, error:~%~a~%" url e) ""))))

(defun fix-url (url)
  (if (equalp (char url 0) #\h)
      url
    (concatenate 'string "http://" url)))

(defun has-child (node tag)
  (loop for c across (plump:child-elements node)
	when (equalp (plump:tag-name c) tag) return t
	return nil))

(defclass article ()
  ((title :accessor title :initarg :title :initform nil)
   (link :accessor link :initarg :link :initform nil)
   (author :accessor author :initarg :author :initform nil)
   (image :accessor image :initarg :image :initform nil)
   (summary :accessor summary :initarg :summary :initform nil)
   (content :accessor content :initarg :content :initform nil)
   (date :accessor date :initarg :date :initform nil)
   (category :accessor category :initarg :category :initform nil)))

(defmethod print-object ((obj article) out)
	   (print-unreadable-object
	    (obj out :type t)
	    (format out
		    "
title: ~a~%link: ~a~%image: ~a~%author: ~a~%date: ~a~%category: ~a~%summary: ~a~%content: ~a~%"
		    (link obj) (image obj) (title obj) (author obj) (date obj)
		    (category obj) (summary obj) (content obj))))

(defgeneric as-rss-entry (article)
	    (:documentation "turn article into rss entry"))

(defmethod as-rss-entry ((article article))
	   (wrap "entry"
		 (concatenate
		  'string
		  (wrap "title" (validify-string (title article)) "type=\"html\"")
		  "<link href=\"" (link article) "\"/>"
		  (wrap "id" (link article))
		  (wrap "updated" (date article))
		  (wrap "summary" (validify-string (summary article)))
		  (build-tag (validify-string (content article))
			     "<content type=\"html\">" "</content>")
		  (build-tag (validify-string (category article)) "<category term=\"" "\"/>")
		  "<author><name>" (validify-string (author article)) "</name></author>"
		  (build-tag (image article) "<media:thumbnail xmlns:media=\"" "\"/>"))))

(defun build-tag (slot start end)
  (if slot (concatenate 'string start slot end) nil))

(defun wrap (tagname text &optional (attribs nil))
  (concatenate 'string "<" tagname (if attribs (concatenate 'string " " attribs) nil)
	       ">" text "</" tagname ">"))

(defun validify-string (str)
  (setf str (cl-ppcre:regex-replace-all "&" str "&amp;")))

(defun get-text (nodes)
  (let ((n (first nodes)))
    (if n (plump:text n) nil)))

(defun get-attrib (name nodes)
  (let ((n (first nodes)))
    (if n (plump:get-attribute n name) nil)))

(defun select-elem (node tag &optional (attribs ()))
  "attribs can be a dotted pair of atrrib names and regex for values"
  (loop for e in (plump:get-elements-by-tag-name node tag) when
	(check-attribs e attribs) collect e))

(defun check-attribs (node attribs)
  (if attribs
      (loop for k being the hash-keys in (plump:attributes node) using (hash-value v)
	    when (loop for (a . r) in attribs
		       when (if (equalp k a) (cl-ppcre:scan r v) nil)
		       return t)
	    return t
	    finally (return nil))
    t))

(defclass webpage ()
  ((title :initarg :title :accessor title)
   (url :initarg :url :accessor url)
   (extract-article-nodes :initarg :extract-nodes :accessor extract-article-nodes)
   (make-article :initarg :make-article :accessor make-article)))

(defparameter
 *lol-dev*
 (make-instance
  'webpage
  :title "League of Legends Dev Blog"
  :url "https://www.leagueoflegends.com/en-us/news/dev/"
  :extract-nodes
  (lambda (root)
    (let ((articles ()))
      (loop for a in (plump:get-elements-by-tag-name root "a") do
	    (if (has-child a "article")
		(push a articles))) 
      articles))
  :make-article
  (lambda (node)
    (let* ((link (plump:get-attribute node "href"))
	   (title (get-text (select-elem node "h2" '(("data-testid" . "title")))))
	   (image (get-attrib "src" (select-elem node "img")))
	   (author (get-text (select-elem node "span" '(("data-testid" . "author")))))
	   (category (get-text (select-elem node "div" '(("data-testid" . "category")))))
	   (date (get-attrib "datetime" (select-elem node "time"))))
      (make-instance 'article
		     :title title
		     :link (concatenate 'string "https://www.leagueoflegends.com/" link)
		     :author author :image image :date date :category category)))))
