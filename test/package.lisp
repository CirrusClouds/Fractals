;;;; package.lisp

(defpackage :cl-fractals/test
  (:use #:cl #:fiveam #:cl-fractals)
  (:local-nicknames (#:f #:fset)
                    (#:s #:serapeum))
  (:export #:guesser-suite))
