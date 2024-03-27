(in-package :cl-user)

(defpackage extract-rss
	    (:use :cl)
	    (:export #:extract-rss
		     ;; holds parsing info for a given site
		     #:webpage
		     ;; helpers for selecting from dom
		     #:get-text
		     #:get-attrib
		     #:has-child
		     #:select-elem
		     ;; helpers for debugging parsing
		     #:get-page-root
		     #:make-article))
