; possible optimization: use something other than a list to record coordinates.
; this might make it faster to look up coordinates in the hash tables.
(defun load-lines (filename)
    (with-open-file (stream filename)
        (loop for line = (read-line stream nil)
            while line
            collect line)))

(defun neighbour-coordinates (r)
    ; returns a list containing the coordinates of each neighbour, as well as the cell itself
    (defun recurse-coords (vec)
        (if vec
            (append
                (mapcar (lambda (x) (cons (+ (car vec) 1) x)) (recurse-coords (cdr vec)))
                (mapcar (lambda (x) (cons    (car vec)    x)) (recurse-coords (cdr vec)))
                (mapcar (lambda (x) (cons (- (car vec) 1) x)) (recurse-coords (cdr vec))))
            (list nil)))
    (recurse-coords r))

(defun make-universe (dimension input-lines)
    (let ((universe (make-hash-table :test #'equal)))
        (loop
            for line in input-lines
            for y from 0
            do (loop
                for c across line
                for x from 0
                do (setf
                        (gethash
                            (append (list x y) (loop for i from 3 upto dimension collect 0))
                            universe)
                        (char= c #\#))))
        universe))

(defun update-universe (universe)
    ; returns a hash table with the number of neighbours each cell can see
    (let ((neighbour-map (make-hash-table :test #'equal))
          (new-universe (make-hash-table :test #'equal)))
        (loop
            for r being the hash-key
            using (hash-value val) of universe
            when val
            do (loop
                for n-r in (neighbour-coordinates r)
                do (let ((current-value (or (gethash n-r neighbour-map) 0)))
                    (setf (gethash n-r neighbour-map) (+ 1 current-value)))
                finally (incf (gethash r neighbour-map) -1))) ; don't include self among neighbours
        (loop
            for r being the hash-key
            using (hash-value num-neighbours) of neighbour-map
            when (or (= num-neighbours 3) (and (= num-neighbours 2) (gethash r universe)))
            do (setf (gethash r new-universe) t))
        new-universe))

; Peter Norvig's Memoization code
(defun memo (fn &key (key #'first) (test #'eql) name)
    (let ((table (make-hash-table :test test)))
        (setf (get name :memo) table)
        #'(lambda (&rest args)
            (let ((k (funcall key args)))
                (multiple-value-bind (val found-p)
                    (gethash k table)
                (if found-p val
                    (setf (gethash k table) (apply fn args))))))))

(defun memoize (fn-name &key (key #'first) (test #'eql))
    (setf (symbol-function fn-name)
        (memo (symbol-function fn-name)
            :name fn-name :key key :test test)))

;(memoize 'neighbour-coordinates :test #'equal)

(let ((universe-input (load-lines #p"input17.txt")))
    ; parts 1 and 2
    (mapcar (lambda (num-dimensions)
        (let ((universe (make-universe num-dimensions universe-input)))
            (loop
                for time-index from 1 upto 6
                do (setf universe (update-universe universe)))
            (format t "~a~%"
                (loop
                    for k being the hash-key
                    using (hash-value v) of universe
                    counting v into result
                    finally (return result)))))
    '(3 4)))
