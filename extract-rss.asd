(in-package :asdf-user)
(defsystem "extract-rss"
	   :description "extract an rss feed from a webpage"
	   :depends-on (:dexador :plump :cl-ppcre)
	   :components
	   ((:module "src"
	     :components
	     ((:file "package")
	      (:file "parse")))))