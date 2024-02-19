(in-package :cl-user)

(defpackage extract-rss
	    (:use :cl)
	    (:export #:extract-rss
		     ;; helpers for selecting from dom
		     #:get-text
		     #:get-attrib
		     #:has-child
		     #:select-elem))
