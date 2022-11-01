;;;; Guesser.lisp

(in-package :cl-fractals)

;; Configuration

(defconstant +states+ (f:set 'L 'R))

(defconstant +n-ary+ 5)

(deftype maybe (a) `(or null ,a))

(deftype bool-fn (a) `(s:-> (,a) boolean))

(deftype switch-state () '(member L R))

(s:-> n-combination-p (vector) boolean)
(defun n-combination-p (xs)
  (= (length xs) +n-ary+))

(deftype n-combination () `(and (vector switch-state)
                                (satisfies n-combination-p)))

(s:-> comb-map-p (f:wb-map) boolean)
(defun comb-map-p (x)
  (and (f:@ x :combination)
       (typep (f:@ x :combination) `n-combination)
       (f:@ x :L-count)
       (typep (f:@ x :L-count) `number)
       (f:@ x :R-count)
       (typep (f:@ x :R-count) `number)))

(deftype comb-map () `(and f:wb-map (satisfies comb-map-p)))

(deftype queue () `(maybe (cons switch-state)))

(s:-> db-p (f:wb-set) boolean)
(defun db-p (db)
  (and (typep db 'f:wb-set)
       (every (lambda (x) (typep x `comb-map))
              (fset:convert 'list db))))

(deftype db () `(satisfies db-p))

;; Gamestuff

(s:-> combination-generator (f:wb-set number list) list)
(defun combination-generator (elements n acc)
  (if (= n 0)
      (list acc)
      (mapcan (lambda (elem)
                (combination-generator elements
                                   (- n 1)
                                   (cons elem acc)))
              (f:convert 'list elements))))

(s:-> filter-set ((bool-fn t) f:wb-set) t)
(defun filter-set (f s)
  (let ((s-prime (f:convert 'list (f:filter f s))))
    (if (not (= (length s-prime) 1))
        (error (format nil "Filter failed on ~s" s))
        (first s-prime))))

(s:-> init-db () db)
(defun init-db ()
  (f:convert `fset:wb-set
             (mapcar
              (lambda (combo)
                (f:map (:combination (coerce combo 'vector))
                       (:L-count 0)
                       (:R-count 0)))
              (combination-generator +states+ +n-ary+ nil))))


(s:-> combination-lookup (n-combination db) comb-map)
(defun combination-lookup (lookup db)
  (filter-set (lambda (combo-map)
                (equalp (f:@ combo-map :combination)
                        lookup))
              db))

(s:-> random-guess () switch-state)
(defun random-guess ()
  (if (= 0 (random 2))
      'L
      'R))

(s:-> make-guess (queue db) list)
(defun make-guess (queue db) 
  (if (>= (length queue) +n-ary+)
      (let* ((lookup (coerce (subseq queue 0 +n-ary+) 'vector))
             (relevant-map (combination-lookup lookup db)))
        (cond ((> (f:@ relevant-map :L-count)
                  (f:@ relevant-map :R-count))
               (list 'L relevant-map))
              ((> (f:@ relevant-map :R-count)
                  (f:@ relevant-map :L-count))
               (list 'R relevant-map))
              (t
               (list (random-guess) relevant-map))))
      (list (random-guess) nil)))

(s:-> update-db (db switch-state comb-map) db)
(defun update-db (db answer to-update)
  (f:union (f:less db to-update)
           (f:set
            (f:map
             (:combination (f:@ to-update :combination))
             (:L-count (if (eq answer 'L)
                           (+ 1 (f:@ to-update :L-count))
                           (f:@ to-update :L-count)))
             (:R-count (if (eq answer 'R)
                           (+ 1 (f:@ to-update :R-count))
                           (f:@ to-update :R-count)))))))

(s:-> play-game (queue db) t)
(defun play-game (queue db win-count move-count)
  (format t "Move: ~s" move-count)
  (terpri)
  (if (> (length queue) 15)
      (format t "queue is ~s..." (subseq queue 0 15))
      (format t "queue is ~s" queue))
  (terpri)
  (format t "My win percentage: ~s%"
          (float (* 100 (/ win-count move-count))))
  (terpri)
  (format t "Please make your guess")
  (terpri)
  (let* ((guess
          (make-guess queue db))
        (their-guess
          (case (read-from-string (read-line))
            (L 'L)
            (LEFT 'L)
            (R 'R)
            (RIGHT 'R)
            (otherwise 'FAIL)))
        (win-state (if (eq (first guess) their-guess)
                       1
                       0)))
    (if (eq their-guess 'FAIL)
        (progn
          (format t "Bad input")
          (terpri)
          (terpri)
          (play-game queue db win-count move-count))
        (progn
          (format t "You guessed ~s" their-guess)
          (terpri)
          (format t "Well I guessed ~s" (first guess))
          (terpri)
          (format t "Using combination ~s" (second guess))
          (terpri)
          (terpri)
          (if (= 1 win-state)
              (format t "I WIN!")
              (format t "I lose :("))
          (terpri)
          (terpri)
          (play-game (cons their-guess queue)
                     (if (second guess)
                         (update-db db their-guess (second guess))
                         db)
                     (+ win-state win-count)
                     (+ 1 move-count))))))

(defun init-game! ()
  (play-game nil (init-db) 0 1))
