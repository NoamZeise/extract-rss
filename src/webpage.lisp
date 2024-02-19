(in-package :extract-rss)

(defclass webpage ()
  ((title :initarg :title :accessor title)
   (xml-file :initarg :xml-file :accessor xml-file)
   (url :initarg :url :accessor url)
   (extract-article-nodes :initarg :extract-article-nodes :accessor extract-article-nodes)
   (make-article :initarg :make-article :accessor make-article)))

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

(defun has-child (node tag)
  (loop for c across (plump:child-elements node)
	when (equalp (plump:tag-name c) tag) return t
	return nil))

(defun check-attribs (node attribs)
  (if attribs
      (loop for k being the hash-keys in (plump:attributes node) using (hash-value v)
	    when (loop for (a . r) in attribs
		       when (if (equalp k a) (cl-ppcre:scan r v) nil)
		       return t)
	    return t
	    finally (return nil))
    t))
