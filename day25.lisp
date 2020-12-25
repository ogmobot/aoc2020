(defconstant +divisor+ 20201227)

(defun load-lines (filename)
    (with-open-file (stream filename)
        (loop for line = (read-line stream nil)
            while line
            collect (parse-integer line))))

(defun transform (subject n)
    (mod (* subject n) +divisor+))

(defun mod-expt (a n m)
    (loop
        with c = 1 while (plusp n)
        do (if (oddp n) (setf c (mod (* a c) m)))
           (setf n (ash n -1)
                 a (mod (* a a) m))
        finally (return c)))

(defun test-loop (subject loop-size)
    (mod-expt subject loop-size +divisor+))

(defun find-target (subject target)
    ; returns loop size
    (let ((value 1))
        (loop
            for i from 1
            do (setf value (transform subject value))
            when (= value target) do (return i))))

(let* ((lines (load-lines #p"input25.txt"))
       (card-pub (car lines))
       (door-pub (cadr lines))
       (door-loop (find-target 7 door-pub)))
    (format t "~a~%" (test-loop card-pub door-loop)))
