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
            (connect map (routes:make-route "archive/:year/:month/:day"))
            (connect map (routes:make-route "forum/:chapter/:topic/:message"))
            (connect map (routes:make-route "archive/:(year)-:(month)-:(day).html"))
            )))

(addtest (routes-test)
  test-1
  (ensure-same '((:chapter . "develop") (:topic . "34") (:message . "25"))
               (reverse (match map #u"forum/develop/34/25"))))

(addtest (routes-test)
  test-2
  (ensure-same '((:year . "2008") (:month . "12") (:day . "12"))
               (reverse (match map #u"archive/2008-12-12.html"))))
         

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-routes-tests ()
  (run-tests :suite 'routes-test :report-pathname nil))
