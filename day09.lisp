(defun load-nums (filename)
    (with-open-file (stream filename)
        (loop for line = (read-line stream nil)
            while line
            collect (parse-integer line))))

(defun allowable-numbers (preamble)
    (apply #'append
        (loop for n in preamble
            collect (mapcar (lambda (x) (+ x n)) (cdr preamble)))))

(defun allowable-numbers-hash (preamble)
    (let ((table (make-hash-table)))
        (mapcar (lambda (x) (setf (gethash x table) t))
            (allowable-numbers preamble))
        table))

(defun find-impostor (the-list preamble-length &optional use-hash)
    (let ((member-function (if use-hash #'gethash #'member))
          (set-maker (if use-hash #'allowable-numbers-hash #'allowable-numbers)))
        (if (<= (length the-list) preamble-length)
            nil
            (if (funcall member-function
                    (nth preamble-length the-list)
                    (funcall set-maker (subseq the-list 0 preamble-length)))
                (find-impostor (cdr the-list) preamble-length)
                (nth preamble-length the-list)))))

(defun find-contiguous (the-list target)
    (let ((the-array (apply #'vector the-list))
          (i 0)
          (j 2))
        (loop
            for the-sum = (reduce '+ (subseq the-array i j)) then the-sum
            while (/= the-sum target)
            do (if (or (< the-sum target) (<= (- j i) 2))
                (progn
                    (setq the-sum (+ the-sum (aref the-array j)))
                    (setq j (+ 1 j)))
                (progn
                    (setq the-sum (- the-sum (aref the-array i)))
                    (setq i (+ 1 i)))))
        (subseq the-array i j)))

; part 1
(let* ((the-list (load-nums #p"input09.txt"))
       (impostor (find-impostor the-list 25 t)))
    (format t "~a~%" impostor)
    ; part 2
    (let ((contig (find-contiguous the-list impostor)))
        (format t "~a~%" (+ (reduce #'min contig) (reduce #'max contig)))))
