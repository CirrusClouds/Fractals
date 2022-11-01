;;;; package.lisp

(defpackage :cl-fractals
  (:use #:cl)
  (:local-nicknames (#:f #:fset)
                    (#:s #:serapeum))
  (:export
   #:switch-state
   #:comb-map
   #:db
   #:combination-generator
   #:filter-set
   #:init-db
   #:combination-lookup
   #:make-guess
   #:play-game
   #:init-game!))
