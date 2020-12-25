(defun load-file (filename)
    (with-open-file (stream filename)
        (loop for line = (read stream nil)
            while line
            collect line)))

(defun solve (numbers target nums-remaining)
    (cond
        ((null numbers) nil)
        ((= nums-remaining 0) nil)
        ((and (= nums-remaining 1) (= (car numbers) target)) target)
        (t (let ((result (solve (cdr numbers) (- target (car numbers)) (- nums-remaining 1))))
            (if result
                (* (car numbers) result)
                (solve (cdr numbers) target nums-remaining))))))

(format t "~a~%" (solve (load-file "input01.txt") 2020 2))
(format t "~a~%" (solve (load-file "input01.txt") 2020 3))
