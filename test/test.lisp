;; test.lisp

(defpackage :routes.test
  (:use :cl :lift :routes)
  (:export
   :run-routes-tests))

(in-package :routes.test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; routest-test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftestsuite routes-test ()
  (map)
  (:setup (progn
            (setq map (make-instance 'mapper))
            (connect map (make-route "archive/:year/:month/:day"))
            (connect map (make-route "forum/:chapter/:topic/:message"))
            (connect map (make-route "archive/:(year)-:(month)-:(day).html"))
            (connect map (make-route "feeds/forum/atom.xml"
                                     :extra-bindings '(:action feeds :chapter "forum")))
            (connect map (make-route "forum/:chapter/:topic/:message/:comment"
                                     :extra-bindings '(:view develop-comment)
                                     :conditions (list :chapter #'(lambda (var)
                                                                    (string= var "develop")))))
            (connect map (make-route "forum/:chapter/:topic/:message/:comment"
                                     :extra-bindings '(:view not-develop-comment)
                                     :conditions (list :chapter #'(lambda (var)
                                                                    (not (string= var "develop")))))))))

(addtest (routes-test)
  dynamic-part-1
  (ensure-same '((:chapter . "develop") (:topic . "34") (:message . "25"))
               (cdr (match map #u"forum/develop/34/25"))))

(addtest (routes-test)
  grouping-1
  (ensure-same '((:year . "2008") (:month . "12") (:day . "12"))
               (cdr (match map "archive/2008-12-12.html"))))

(addtest (routes-test)
  extra-bindings-1
  (ensure-same '((:action . feeds) (:chapter . "forum"))
               (cdr (match map #u"feeds/forum/atom.xml"))))

(addtest (routes-test)
  conditions-1
  (ensure-same 'develop-comment
               (cdr (assoc :view (cdr (match map "forum/develop/453/23/12"))))))

(addtest (routes-test)
  conditions-2
  (ensure-same 'not-develop-comment
               (cdr (assoc :view (cdr (match map "forum/live/453/23/12"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-routes-tests ()
  (run-tests :suite 'routes-test :report-pathname nil))
