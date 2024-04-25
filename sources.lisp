;;; Can store the webpage defs here
;;; Add to *sources* to make the feed file when
;;; refresh-files is called

(load (merge-pathnames "extract-rss.asd" *load-truename*))
(ql:quickload :extract-rss :silent t)
(in-package :extract-rss)

(defparameter
 *lol-dev*
 (make-instance
  'webpage
  :title "League of Legends Dev Blog"
  :url "https://www.leagueoflegends.com/en-us/news/dev/"
  :xml-file "lol-devblog"
  :extract-article-nodes
  (lambda (root)
    (loop for a in (plump:get-elements-by-tag-name root "a")
	  when (has-child a "article") collect a))
  :make-article
  (lambda (node)
    (make-instance
     'article
     :title (get-text (select-elem node "h2" '(("data-testid" . "title"))))
     :link (format nil "https://www.leagueoflegends.com/~a"
		   (plump:get-attribute node "href"))
     :author (get-text (select-elem node "span" '(("data-testid" . "author"))))
     :image (get-attrib "src" (select-elem node "img"))
     :date (get-attrib "datetime" (select-elem node "time"))
     :category (get-text (select-elem node "div" '(("data-testid" . "category"))))))))
 
(in-package :cl-user)

(defparameter
 *picotron-carts*
 (make-instance
  'extract-rss:webpage
  :title "Picotron Cartridges"
  :url "https://www.lexaloffle.com/bbs/?cat=8#sub=2"
  :xml-file "picotron-carts"
  :extract-article-nodes
  (lambda (node)
    (let* ((script-node (plump:get-element-by-id node "cart_data_script"))
	   (raw-text (if script-node (plump:text script-node) ""))
	   (start-array (cl-ppcre:scan "pdat=\\[" raw-text))
	   (end-array (nth-value 1 (cl-ppcre:scan "\\];" raw-text)))
	   (array-text (subseq raw-text start-array end-array)))
      (loop for s in 
	    (uiop:split-string
	     array-text
	     :separator uiop:+lf+)
	    when (cl-ppcre:scan ",2,'" s)
	    collect s)))
  :make-article
  (lambda (text)
    (let ((article (make-instance 'extract-rss:article))
	  (data (get-picotron-article-data text)))
      (loop for e in data and i from 0 do
	    (let ((dat (string-trim " '\"`" e)))
	      (cond
	       ((= i 1)
		(setf
		 (extract-rss::link article)
		 (format nil "https://www.lexaloffle.com/bbs/?tid=~a" dat)))
	       ((= i 2) (setf (extract-rss::title article) dat))
	       ((= i 3)
		(setf
		 (extract-rss::image article)
		 (format nil "https://www.lexaloffle.com~a" dat)))
	       ((= i 8) (setf (extract-rss::author article) dat))
	       ((= i 9) (setf (extract-rss::date article) dat))
	       ((= i 18) (setf (extract-rss::category article) dat)))))
      article))))

(defun get-picotron-article-data (article)
  (let ((inside-arr nil)
	(inside-bracket nil)
	(inside-quote nil)
	(start nil)
	(collected (list)))
    (loop for ch across article and idx from 0 do
	  (if inside-arr
	      (progn
		(if (equalp ch inside-quote)
		    (setq inside-quote nil)
		  (if (and (not inside-quote)
			   (cl-ppcre:scan "\"|`|'" (list ch)))
		      (setq inside-quote ch)))
		(if (not inside-quote)
		    (if inside-bracket
			(if (equalp ch #\])
			    (setq inside-bracket nil))
		      (if (equalp ch #\[)
			  (setq inside-bracket t))))
		(if (not start)
		    (setq start idx)
		  (if (not (or inside-quote inside-bracket))
		      (if (equalp ch #\,)
			  (progn
			    (push (subseq article start idx) collected)
			    (setq start nil))))))
	    (if (equalp ch #\[) (setq inside-arr t))))
    (reverse collected)))

(defparameter *sources*
	      (list extract-rss::*lol-dev*
		    *picotron-carts*))

(defun refresh-files (folder)
  (ensure-directories-exist folder)
  (loop for s in *sources* do
	(extract-rss:extract-rss s folder)))
