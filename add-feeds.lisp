;;; create feeds and push to external repo
;;; takes folder to repo as arg

(load (merge-pathnames "sources.lisp" *load-truename*))

(destructuring-bind (path) (uiop:command-line-arguments)
		    (refresh-files path)
		    (uiop:chdir path)
		    (handler-case
			(progn
			  (uiop:run-program "git add -A" :output t)
			  (uiop:run-program
			   (format nil "git commit -m \"updated feeds, time: ~a\""
				   (get-universal-time))
			   :output t)
			  (uiop:run-program "git push" :output t))
		      (error (e) (format t "git: ~a~%" e))))

