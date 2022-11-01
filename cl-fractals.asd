;;;; cl-fractals.asd

(asdf:defsystem #:cl-fractals
  :description "Describe cl-fractals here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:fiveam #:alexandria #:serapeum #:trivia #:fset)
  :components ((:module "src"
                :serial t
                :components
                ((:file "package")
                 (:file "guesser")))
               (:module "test"
                :serial t
                :components
                ((:file "package")
                 (:file "guesser-tests")))))
