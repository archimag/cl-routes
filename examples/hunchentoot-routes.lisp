
(asdf:operate 'asdf:load-op 'cl-who)
(asdf:operate 'asdf:load-op 'hunchentoot-routes)


(defparameter *map* (make-instance 'routes:mapper))

(setq hunchentoot:*dispatch-table*
      (list (routes.hunchentoot:make-dispatcher *map*)
            #'hunchentoot:default-dispatcher))

(defparameter *server* (hunchentoot:start-server :port 8080))

;;; vhost "archimag"

(defun index.html (bindings)
  (declare (ignore bindings))
  (who:with-html-output-to-string (s)
    (:html 
     (:body 
      (:h1 "Index")
      (:ul (loop for x from 1 to 10
              do (who:htm (:li (:a :href (format nil "chapter-~S.html" x)
                                   (who:str (format nil "Chapter ~S" x)))))))))))
  
(defun chapter-?.html (bindings)
  (let ((id (cdr (assoc :id bindings))))
    (who:with-html-output-to-string (s)
      (:html
       (:body
        (:h1 (who:fmt "Chapter ~A" id))
        (:p (who:fmt "This is a chapter number ~A" id))
        (:a :href "index.html" "Back to index"))))))


(routes.hunchentoot:connect-handler *map*
                                    "routes/chapter-:(id).html"
                                    'chapter-?.html
                                    :host "archimag:8080"
                                    :method :get)

(routes.hunchentoot:connect-handler *map*
                                    "routes/index.html"
                                    'index.html
                                    :host "archimag:8080"
                                    :method :get)

;;; vhost "tabris"

(defun welcome-to-tabris.get.html (bindings)
  (declare (ignore bindings))
  (who:with-html-output-to-string (s)
    (:html
     (:body
      (:h1 "Welcome to Tabris!")
      (:form :method "post" :action "index.html"
       (:div "Input test message:")
       (:input :name "message")
       (:input :value "Send" :type "submit"))))))

(defun welcome-to-tabris.post.html (bindings)
  (declare (ignore bindings))
  (who:with-html-output-to-string (s)
    (:html
     (:body
      (:h1 "Welcome to Tabris!")
      (:div (:b "test message: ")
            (who:str (hunchentoot:post-parameter "message")))
      (:a :href "" "again")))))

(routes.hunchentoot:connect-handler *map*
                                    "index.html"
                                    'welcome-to-tabris.get.html
                                    :host "tabris:8080"
                                    :method :get)

(routes.hunchentoot:connect-handler *map*
                                    "index.html"
                                    'welcome-to-tabris.post.html
                                    :host "tabris:8080"
                                    :method :post)
                                    