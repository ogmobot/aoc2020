(defconstant +puzzle-input+ '(8 5 3 1 9 2 6 4 7))

(defun circular-length (clist)
    (if (null clist)
        0
        (loop
            for ptr = (cdr clist) then (cdr ptr)
            for i from 1
            until (or (null ptr) (eq ptr clist))
            finally (return i))))

(defun make-refs (long-list)
    (let ((result (make-array
                    (+ 1 (circular-length long-list))
                    :element-type 'list :initial-element nil)))
        (loop
            for i from 1 upto (circular-length long-list)
            for ptr = long-list then (cdr ptr)
            do (setf (aref result (car ptr)) ptr))
        result))

(defun circular (items)
    (let ((tmp (copy-list items)))
        (setf (cdr (last tmp)) tmp)))

(defun move-circular-refs (cups refs)
    ; (car cups) is current cup.
    ; refs is a vector of length 1000000 where ref[n] points to (n ...)
    (let* ((picked-head (cdr cups))
           (dest (loop
                for destination = (- (car cups) 1) then (- destination 1)
                when (< destination 1) do (incf destination (- (length refs) 1))
                while (or
                    (eql destination (car cups))
                    (eql destination (car picked-head))
                    (eql destination (cadr picked-head))
                    (eql destination (caddr picked-head)))
                finally (return destination))))
        (let ((dest-target (aref refs dest)))
            ; Skip from 1st element to 5th element (removing elements 2, 3, 4)
            ; (picked-head keeps a pointer to the 2nd element)
            (setf (cdr cups) (nthcdr 4 cups)
                  (cdddr picked-head) (cdr dest-target)
                  (cdr dest-target) picked-head))
        (cdr cups)))

; part 1
(let* ((cups (circular +puzzle-input+))
       (refs (make-refs cups)))
    (loop
        for i from 1 upto 100
        ;do (format t "move ~a ~s~%" i cups)
        do (setf cups (move-circular-refs cups refs))) ; overkill? what's that?
    (let ((the-one (aref refs 1)))
        (format t "~{~^~a~}~%" (subseq the-one 1 9))))
; part 2
(let* ((cups
        (circular (append
            +puzzle-input+
            (loop for i from 10 upto 1000000 collect i))))
       (refs (make-refs cups)))
    (loop
        for i from 1 upto 10000000
        do (setf cups (move-circular-refs cups refs))
    )
    (let ((the-one (aref refs 1)))
    (format t "~a~%" (* (cadr the-one) (caddr the-one)))))
