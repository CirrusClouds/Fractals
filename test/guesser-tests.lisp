;;;; guesser-tests.lisp

(in-package :cl-fractals/test)

(def-suite guesser-suite
  :description "guesser suite")

(in-suite guesser-suite)

(test db-type-tests
  (is (typep
       (f:set (f:map (:combination (vector 'L 'L 'R 'R 'L))
                     (:L-count 0)
                     (:R-count 0))) `db)))

(test combination-generator-tests
  (is (= (length (combination-generator (f:set 'L 'R) 5 nil))
         32)))

(test filter-set-tests
  (is (filter-set (lambda (x) t) (f:set 'a))))

(test init-db-tests
  (is (typep (f:arb (init-db)) `comb-map)))

(test combination-lookup-tests
  (is (equalp
       (f:map (:combination
               (vector 'L 'L 'R 'R 'L))
              (:L-count 0)
              (:R-count 0))
       (combination-lookup (vector 'L 'L 'R 'R 'L)
                           (f:set (f:map (:combination
                                          (vector 'L 'L 'R 'R 'L))
                                         (:L-count 0)
                                         (:R-count 0)))))))

(test make-guess-tests
  (is (eq 'cl-fractals::L
          (first (make-guess
                  nil
                  (f:set (f:map (:combination
                                 (vector 'L 'L 'R 'R 'L))
                                (:L-count 0)
                                (:R-count 1)))))))
  (is (eq 'cl-fractals::L
          (first (make-guess
                  '(cl-fractals::L cl-fractals::L cl-fractals::R)
                  (f:set (f:map (:combination
                                 (vector 'cl-fractals::L 'cl-fractals::L 'cl-fractals::R 'cl-fractals::R 'cl-fractals::L))
                                (:L-count 0)
                                (:R-count 1)))))))
  (is (eq 'cl-fractals::R
          (first (make-guess
                  '(cl-fractals::L cl-fractals::L
                    cl-fractals::R cl-fractals::R cl-fractals::L)
                  (f:set (f:map (:combination
                                 (vector 'cl-fractals::L 'cl-fractals::L 'cl-fractals::R 'cl-fractals::R 'cl-fractals::L))
                                (:L-count 0)
                                (:R-count 1))))))))
