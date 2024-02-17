(load "extract-rss.asd")
(ql:quickload :extract-rss :silent t)
(in-package :extract-rss)

(defparameter
 *lol-dev*
 (make-instance
  'webpage
  :title "League of Legends Dev Blog"
  :url "https://www.leagueoflegends.com/en-us/news/dev/"
  :xml-file "lol-devblog"
  :extract-nodes
  (lambda (root)
    (loop for a in (plump:get-elements-by-tag-name root "a")
	  when (has-child a "article") collect a))
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

(defparameter *sources* (list extract-rss::*lol-dev*))

(defun refresh-files ()
  (loop for s in *sources* do
	(extract-rss:extract-rss s "sources/")))
