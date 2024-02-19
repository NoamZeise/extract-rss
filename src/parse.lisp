;;; Functions to help with selecting elements from the dom
;;; for building feeds for a specific website
(in-package :extract-rss)

(defun get-text (nodes)
  "extract text from first element of nodes"
  (let ((n (first nodes)))
    (if n (plump:text n) nil)))

(defun get-attrib (name nodes)
  "get attribute value with name from first element of nodes"
  (let ((n (first nodes)))
    (if n (plump:get-attribute n name) nil)))

(defun has-child (node tag)
  "true if node has child with tag-name of tag"
  (loop for c across (plump:child-elements node)
	when (equalp (plump:tag-name c) tag) return t
	return nil))

(defun select-elem (node tag &optional (attribs ()))
  """select nodes with tagname and attributes with values matching regexs
attribs must be a dotted pair of atrribute names and regex for
matching attribute values"""
  (loop for e in (plump:get-elements-by-tag-name node tag) when
	(check-attribs e attribs) collect e))

;;; Helpers 

(defun check-attribs (node attribs)
  (if attribs
      (loop for k being the hash-keys in (plump:attributes node) using (hash-value v)
	    when (loop for (a . r) in attribs
		       when (if (equalp k a) (cl-ppcre:scan r v) nil)
		       return t)
	    return t
	    finally (return nil))
    t))
