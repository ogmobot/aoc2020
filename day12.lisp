(defun load-lines (filename)
    (with-open-file (stream filename)
        (loop for line = (read-line stream nil)
            while line
            collect (cons (aref line 0) (parse-integer (subseq line 1))))))

(defun turn (letter current-direction)
    (let ((directions '(:north :east :south :west)))
        (nth
            (rem (+
                (position current-direction directions)
                (if (char= letter #\L)
                    3 ; i.e. 4-1
                    1))
                4)
            directions)))

(defun turn-vector (letter x y)
    (if (char= letter #\L)
        (cons (- y) x)
        (cons y (- x)))) ; trust me dude

(defun new-coords-1 (x y direction instruction)
    (let ((i-char (car instruction))
          (i-val  (cdr instruction)))
        (case i-char
            ((#\N #\S #\E #\W #\F)
                (let ((deltas
                        (case i-char
                            (#\N '(0 . 1))
                            (#\S '(0 . -1))
                            (#\E '(1 . 0))
                            (#\W '(-1 . 0))
                            (#\F (case direction
                                (:north '(0 . 1))
                                (:south '(0 . -1))
                                (:east '(1 . 0))
                                (:west '(-1 . 0)))))))
                    (setf x (+ x (* i-val (car deltas))))
                    (setf y (+ y (* i-val (cdr deltas))))))
            ((#\L #\R)
                (let ((num-turns (/ i-val 90)))
                    (loop for i from 0 upto (- num-turns 1)
                        do (setf direction (turn i-char direction))))))
        (list x y direction)))

(defun new-coords-2 (x y wpx wpy instruction)
    (let ((i-char (car instruction))
          (i-val  (cdr instruction)))
        (case i-char
            (#\F
                (setf x (+ x (* i-val wpx)))
                (setf y (+ y (* i-val wpy))))
            ((#\N #\S #\E #\W)
                (let ((deltas
                        (case i-char
                            (#\N '(0 . 1))
                            (#\S '(0 . -1))
                            (#\E '(1 . 0))
                            (#\W '(-1 . 0)))))
                    (setf wpx (+ wpx (* i-val (car deltas))))
                    (setf wpy (+ wpy (* i-val (cdr deltas))))))
            ((#\L #\R)
                (let ((num-turns (/ i-val 90)))
                    (loop
                        for i from 0 upto (- num-turns 1)
                        for new-wp = (turn-vector i-char wpx wpy)
                        do (progn
                            (setf wpx (car new-wp))
                            (setf wpy (cdr new-wp)))))))
        (list x y wpx wpy)))

(let ((instructions (load-lines #p"input12.txt")))
    ; east and north are +ve
    ; west and south are -ve
    (let ((x 0)
          (y 0)
          (direction :east))
        (loop
            for instruction in instructions
            for result = (new-coords-1 x y direction instruction)
            do (progn
                (setf x (car result))
                (setf y (cadr result))
                (setf direction (caddr result))
                ;(format t "(~a ~a) facing ~a~%" x y direction)
                ))
        (format t "~a~%" (+ (abs x) (abs y))))
    (let ((x    0)
          (y    0)
          (wpx 10)
          (wpy  1))
        (loop
            for instruction in instructions
            for result = (new-coords-2 x y wpx wpy instruction)
            do (progn
                (setf x (car result))
                (setf y (cadr result))
                (setf wpx (caddr result))
                (setf wpy (cadddr result))
                ;(format t "(~a ~a)~%" x y)
                ))
        (format t "~a~%" (+ (abs x) (abs y)))))

;; Cool solution using complex numbers from /g/ #79171501
;(ql:quickload :alexandria)
;(ql:quickload :cl-ppcre)
;
;(defpackage :day12
    ;(:use :cl :alexandria :cl-ppcre))
;
;(in-package :day12)
;
;(defun part1 (file)
    ;(with-open-file (s file)
        ;(loop with pos = #C(0 0)
              ;with facing = #C(1 0)
              ;for inst = (read-line s nil nil) while inst
              ;for dir = (char inst 0)
              ;for num = (parse-integer (subseq inst 1)) do
                ;(case dir
                    ;(#\N (incf pos (complex 0 num)))
                    ;(#\S (incf pos (complex 0 (- num))))
                    ;(#\E (incf pos (complex num 0)))
                    ;(#\W (incf pos (complex (- num) 0)))
                    ;(#\L (setf facing (* facing (expt (complex 0 1) (/ num 90)))))
                    ;(#\R (setf facing (* facing (expt (complex 0 (- 1)) (/ num 90)))))
                    ;(#\F (incf pos (* facing num)))
              ;finally (return (+ (abs (realpart pos)) (abs (imagpart pos))))))))
;
;(defun part2 (file)
    ;(with-open-file (s file)
        ;(loop with pos = #C(0 0)
              ;with waypoint = #C(10 1)
              ;for inst = (read-line s nil nil) while inst
              ;for dir = (char inst 0)
              ;for num = (parse-integer (subseq inst 1)) do
                ;(case dir
                    ;(#\N (incf waypoint (complex 0 num)))
                    ;(#\S (incf waypoint (complex 0 (- num))))
                    ;(#\E (incf waypoint (complex num 0)))
                    ;(#\W (incf waypoint (complex (- num) 0)))
                    ;(#\L (setf waypoint (* waypoint (expt (complex 0 1) (/ num 90)))))
                    ;(#\R (setf waypoint (* waypoint (expt (complex 0 (- 1)) (/ num 90)))))
                    ;(#\F (incf pos (* num waypoint)))
              ;finally (return (+ (abs (realpart pos)) (abs (imagpart pos))))))))
;
;(defun answers (file)
    ;(values (part1 file) (part2 file)))
