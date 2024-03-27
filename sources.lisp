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

(defparameter *sources* (list extract-rss::*lol-dev*))

(defun refresh-files (folder)
  (ensure-directories-exist folder)
  (loop for s in *sources* do
	(extract-rss:extract-rss s folder)))
