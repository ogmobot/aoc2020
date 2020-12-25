(defun load-lines (filename)
    (with-open-file (stream filename)
        (loop for line = (read-line stream nil)
            while line
            collect line)))

(defun lines-to-grid (lines neighbour-function)
    (let ((result (make-hash-table :test #'equal))
          (all-neighbours (make-hash-table :test #'equal)))
        (loop
            for line in lines
            for y = 0 then (+ y 1)
            do (loop
                for chr across line
                for x = 0 then (+ x 1)
                do (setf (gethash (cons x y) result) chr)))
        (loop
            for line in lines
            for y = 0 then (+ y 1)
            do (loop
                for chr across line
                for x = 0 then (+ x 1)
                do (setf
                    (gethash (cons x y) all-neighbours)
                    (funcall neighbour-function result x y))))
        (cons result all-neighbours)))

(defun short-neighbour-coords (grid x y)
    (let ((candidates (mapcar
        (lambda (deltas) (cons (+ x (car deltas)) (+ y (cdr deltas))))
              '((-1 . -1) (0 . -1) (1 . -1)
                (-1 .  0)          (1 .  0)
                (-1 .  1) (0 .  1) (1 .  1)))))
        (loop for val in candidates when (gethash val grid) collect val)))
    
(defun long-neighbour-coords (grid x y)
    (let ((directions
          '((-1 . -1) (0 . -1) (1 . -1)
            (-1 .  0)          (1 .  0)
            (-1 .  1) (0 .  1) (1 .  1))))
        (loop
            for cell in (mapcar
                (lambda (deltas)
                    (loop
                        for distance = 1 then (+ 1 distance)
                        for cell-val = (gethash
                            (cons (+ x (* distance (car deltas)))
                                  (+ y (* distance (cdr deltas))))
                            grid)
                        while (and cell-val (char= cell-val #\.))
                        finally (return
                            (cons (+ x (* distance (car deltas)))
                                  (+ y (* distance (cdr deltas)))))))
                directions)
            when (gethash cell grid) collect cell)))

(defun update-cell (grid x y neighbour-table rules)
    (let* ((current-cell (gethash (cons x y) grid))
           (neighbours (gethash (cons x y) neighbour-table))
           (neighbour-count
                (reduce '+ (mapcar
                    (lambda (coords)
                        (if (char= (gethash coords grid) #\#) 1 0))
                    neighbours))))
        (funcall (cdr (assoc current-cell rules)) neighbour-count)))

(defun update-world (grid all-neighbours rules)
    (let ((result (make-hash-table :test #'equal)) (changed nil))
        (loop for key being the hash-keys of grid
            do (progn
                (let ((new-cell
                        (update-cell grid (car key) (cdr key) all-neighbours rules)))
                    (setf (gethash key result) (car new-cell))
                    (if (cdr new-cell) (setf changed t)))))
        (cons result changed)))

(defun print-corner (grid size)
    (loop for y from 0 upto size
        do (progn
            (loop for x from 0 upto size
                do (format t "~a" (gethash (cons x y) grid)))
            (format t "~%")))
    (format t "~%"))

(defun run-until-stable (grid all-neighbours rules)
    (loop
        for next-step = (update-world grid all-neighbours rules)
        do (setf grid (car next-step))
        until (null (cdr next-step))
        finally (return grid)))

(let ((lines (load-lines #p"input11.txt")))
    ; part 1
    (let* ((grid-neighbours (lines-to-grid lines #'short-neighbour-coords))
           (grid (car grid-neighbours))
           (all-neighbours (cdr grid-neighbours))
           (rules (list
                (cons #\. (lambda (val)
                    (declare (ignore val))
                    '(#\. . nil)))
                (cons #\# (lambda (val)
                    (if (>= val 4) '(#\L . t)
                                   '(#\# . nil))))
                (cons #\L (lambda (val)
                    (if (= val 0) '(#\# . t)
                                  '(#\L . nil)))))))
        (format t "~a~%" (count-if
            (lambda (x) (char= #\# x))
            (loop
                for val being the hash-values of
                    (run-until-stable grid all-neighbours rules) collect val))))
    ; part 2
    (let* ((grid-neighbours (lines-to-grid lines #'long-neighbour-coords))
           (grid (car grid-neighbours))
           (all-neighbours (cdr grid-neighbours))
           (rules (list
                (cons #\. (lambda (val)
                    (declare (ignore val))
                    '(#\. . nil)))
                (cons #\# (lambda (val)
                    (if (>= val 5) '(#\L . t)
                                   '(#\# . nil))))
                (cons #\L (lambda (val)
                    (if (= val 0) '(#\# . t)
                                  '(#\L . nil)))))))
        (format t "~a~%" (count-if
            (lambda (x) (char= #\# x))
            (loop
                for val being the hash-values of
                    (run-until-stable grid all-neighbours rules) collect val)))))
"
    ; bonus: Conway's Game of Life
     (let* ((grid-neighbours (lines-to-grid lines #'short-neighbour-coords))
           (grid (car grid-neighbours))
           (all-neighbours (cdr grid-neighbours))
           (rules (list
                (cons #\. (lambda (val)
                    (if (= val 3) '(#\# . t)
                                  '(#\. . nil))))
                (cons #\# (lambda (val)
                    (if (<= 2 val 3) '(#\# . nil)
                                     '(#\. . t))))
                (cons #\L (lambda (val)
                    (declare (ignore val))
                    (if (= 0 (random 2))
                        '(#\# . t)
                        '(#\. . t)))))))
            (loop
                for i from 0 upto 300
                for next-grid = (car (update-world grid all-neighbours rules))
                do (print-corner grid 90)
                do (format t ~a/300~% i)
                do (sleep 0.2)
                do (setf grid next-grid))))
"
