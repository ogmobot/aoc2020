(defun load-strings (filename)
    (with-open-file (stream filename)
        (loop for line = (read-line stream nil)
            while line
            collect line)))

(defun decode-binary (s ones)
    (parse-integer
        (format nil "~{~a~^~}"
            (loop for c across s
                collect (if (member c ones) 1 0)))
        :radix 2))
    
(defun seat-id (s)
    (decode-binary s '(#\B #\R)))

(defun find-missing (seats)
    (loop for n = (apply #'min seats) then (+ n 1)
        when (not (member n seats))
            return n))
        
(let ((seat-ids (mapcar #'seat-id (load-strings #p"input05.txt"))))
    (format t "~a~%" (apply #'max seat-ids))
    (format t "~a~%" (find-missing seat-ids)))

;; Above uses O(n) space and O(n^2) time.
;; Below solution (from /g/ #79051766) uses O(1) space and O(n) time
;; by using some cool features of the (loop) macro.

;(defun str-replace (text old new)
;    (concatenate 'string
;        (loop for letter across text
;            collect(cond ((member letter old :test #'char=) new) (t letter)))))

;(defun get-binary (text)
;    (str-replace(str-replace text '(#\F #\L) #\0) '(#\B #\R) #\1))

;(defun get-result (max-seat min-seat missing)
;    (cons
;        max-seat
;        (floor (/ (+ missing (-(expt (+ 1 max-seat) 2)(expt min-seat 2))) 2))))

;(defun solve ()
;    (with-open-file (stream "input05.txt")
;        (loop for line = (read-line stream nil)
;            while line
;            do (setq seat (parse-integer (get-binary line) :radix 2))
;            maximize seat into max-seat minimize seat into min-seat
;            sum (- (* seat seat) (expt (+ 1 seat) 2)) into missing
;            finally (return (get-result max-seat min-seat missing)))))

;(write (solve))
