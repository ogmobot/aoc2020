(defun load-strings (filename)
    (with-open-file (stream filename)
        (loop for line = (read-line stream nil)
            while line
            collect line)))

(defun count-trees (grid dx dy)
    (count-if (lambda (x) (eql x #\#))
        (loop for n = 0 then (+ n 1)
            while (< (* n dy) (length grid))
            collect (char (nth (* n dy) grid) (mod (* n dx) (length (nth 0 grid)))))))

(let ((the-grid (load-strings #p"input03.txt")))
    (format t "~a~%" (count-trees the-grid 3 1))
    (format t "~a~%" (*
        (count-trees the-grid 1 1)
        (count-trees the-grid 3 1)
        (count-trees the-grid 5 1)
        (count-trees the-grid 7 1)
        (count-trees the-grid 1 2))))


;;; Alternative solution
;(defun day3-solver-iterate (input x y)
;    (let ((height (length input))
;          (width (length (aref input 0))))
;    (iter
;        (for row :from 0 :below height :by y)
;        (for col :initially 0 :then (mod (+ col x) width))
;        (counting (char= (char (aref input row) col) #\#)))))
