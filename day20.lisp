(defconstant +last-index+ 9)
(defconstant +tile-size+ 8)
(defconstant +buffer-size+ 96)
; tile structure:
; (:id 2311 :data ("###" ... "###") :orientation 0)
; (getf tile :data) is a list of ten lenth-10 strings

; orientations
; 0 (unchanged) ab  4 (flipped) ad
;               dc              bc

; 1 (90 cwise)  da  5           ba
;               cb              cd

; 2 (180 cwise) cd  6           cb
;               ba              da

; 3 (270 cwise) bc  7           dc
;               ad              ab

(defun load-all (filename)
    ; returns a list of tiles
    (with-open-file (stream filename)
            (loop for line = (read-line stream nil)
                while line
                collect (list
                    :id (parse-integer (subseq line 5) :junk-allowed t)
                    :data (loop for line = (read-line stream nil)
                            while (and line (not (string= line "")))
                            collect line)
                    :orientation 0))))

(defun raw-top (tile)
    ; returns top row LEFT-TO-RIGHT in orientation 0 (a->b)
    (car (getf tile :data)))

(defun raw-bottom (tile)
    ; returns bottom row RIGHT-TO-LEFT in orientation 0 (c->d)
    (reverse (nth +last-index+ (getf tile :data))))

(defun raw-right (tile)
    ; returns right row TOP-TO-BOTTOM in orientation 0 (b->c)
    (format nil "~{~^~a~}"
        (mapcar (lambda (row) (char row +last-index+)) (getf tile :data))))

(defun raw-left (tile)
    ; returns right row BOTTOM-TO-TOP in orientation 0 (d->a)
    (format nil "~{~^~a~}"
        (mapcar (lambda (row) (char row 0)) (reverse (getf tile :data)))))

(defun rel-top (tile)
    ; returns top row in given orientation
    (case (getf tile :orientation)
        (0 (raw-top tile))      (4 (reverse (raw-left tile)))
        (1 (raw-left tile))     (5 (reverse (raw-top tile)))
        (2 (raw-bottom tile))   (6 (reverse (raw-right tile)))
        (3 (raw-right tile))    (7 (reverse (raw-bottom tile)))))

(defun rel-bottom (tile)
    ; returns bottom row in given orientation
    (case (getf tile :orientation)
        (0 (raw-bottom tile))   (4 (reverse (raw-right tile)))
        (1 (raw-right tile))    (5 (reverse (raw-bottom tile)))
        (2 (raw-top tile))      (6 (reverse (raw-left tile)))
        (3 (raw-left tile))     (7 (reverse (raw-top tile)))))

(defun rel-right (tile)
    ; returns right row in given orientation
    (case (getf tile :orientation)
        (0 (raw-right tile))    (4 (reverse (raw-bottom tile)))
        (1 (raw-top tile))      (5 (reverse (raw-left tile)))
        (2 (raw-left tile))     (6 (reverse (raw-top tile)))
        (3 (raw-bottom tile))   (7 (reverse (raw-right tile)))))

(defun rel-left (tile)
    ; returns left row in given orientation
    (case (getf tile :orientation)
        (0 (raw-left tile))     (4 (reverse (raw-top tile)))
        (1 (raw-bottom tile))   (5 (reverse (raw-right tile)))
        (2 (raw-right tile))    (6 (reverse (raw-bottom tile)))
        (3 (raw-top tile))      (7 (reverse (raw-left tile)))))

(defun place-all-tiles (tiles tile-map)
    ; place first tile
    (setf (gethash (cons 0 0) tile-map) (car tiles))
    (setf tiles (cdr tiles))
    ; place remaining tiles
    (loop
        while (> (length tiles) 0)
        ;do (format t "tiles: ~S~%" tiles)
        do (loop named each-tile
            for k being the hash-key
            using (hash-value v) of tile-map
            do
                (let ((above-space (cons (car k) (- (cdr k) 1)))
                      (below-space (cons (car k) (+ (cdr k) 1)))
                      (left-space  (cons (- (car k) 1) (cdr k)))
                      (right-space (cons (+ (car k) 1) (cdr k))))
                    (loop
                        for xs in (list (list :space above-space :base-fn #'rel-top    :new-fn #'rel-bottom)
                                        (list :space below-space :base-fn #'rel-bottom :new-fn #'rel-top)
                                        (list :space left-space  :base-fn #'rel-left   :new-fn #'rel-right)
                                        (list :space right-space :base-fn #'rel-right  :new-fn #'rel-left))
                        when (null (gethash (getf xs :space) tile-map))
                        ; try to fit a piece in
                        do (loop
                            for candidate in tiles
                            do (loop
                                for ori from 0 upto 7
                                do (progn
                                    (setf (getf candidate :orientation) ori)
                                    (if (string=
                                            (funcall (getf xs :base-fn) v)
                                            (reverse (funcall (getf xs :new-fn) candidate)))
                                        (progn
                                            (setf (gethash (getf xs :space) tile-map) candidate)
                                            (setf tiles (remove candidate tiles))
                                            (return-from each-tile (getf xs :space)))))))))))
    tile-map)

(defun get-extremes (tile-map)
    (loop for k being the hash-key of tile-map
        minimizing (car k) into min-x
        maximizing (car k) into max-x
        minimizing (cdr k) into min-y
        maximizing (cdr k) into max-y
        finally (return (list min-x max-x min-y max-y))))

(defun tile-map-to-buffer (tile-map)
    (let* ((extremes (get-extremes tile-map))
           (min-x (car extremes))
           (min-y (caddr extremes))
           (buffer (make-array
                    `(,+buffer-size+ ,+buffer-size+)
                    :element-type 'character
                    :initial-element #\.)))
        (loop
            for k being the hash-key
            using (hash-value v) of tile-map
            for x = (car k)
            for y = (cdr k)
            do (loop
                for rindex from 0 upto (- +tile-size+ 1)
                do (loop
                    for cindex from 0 upto (- +tile-size+ 1)
                    do (let* ((m (getf v :data))
                        (ch
                        (case (getf v :orientation) ; DON'T TOUCH
                            (0 (char (nth (+ 1 rindex) m) (+ 1 cindex)))
                            (1 (char (nth (+ 1 cindex) (reverse m)) (+ 1 rindex)))
                            (2 (char (reverse (nth (+ 1 rindex) (reverse m))) (+ 1 cindex)))
                            (3 (char (reverse (nth (+ 1 cindex) m)) (+ 1 rindex)))
                            (4 (char (nth (+ 1 cindex) m) (+ 1 rindex)))
                            (5 (char (reverse (nth (+ 1 rindex) m)) (+ 1 cindex)))
                            (6 (char (reverse (nth (+ 1 cindex) (reverse m))) (+ 1 rindex)))
                            (7 (char (nth (+ 1 rindex) (reverse m)) (+ 1 cindex)))
                            (otherwise #\x))))
                        (setf (aref
                            buffer
                            (+ rindex (* +tile-size+ (- y min-y)))
                            (+ cindex (* +tile-size+ (- x min-x))))
                            ch)))))
    buffer))

(defun display-buffer (buffer)
    (loop for row from 0 upto (- +buffer-size+ 1)
        do (loop for col from 0 upto (- +buffer-size+ 1)
            do (format t "~c" (aref buffer row col)))
        do (format t "~%")))

(defun rotate-array (a)
    (let* ((num-rows (car (array-dimensions a)))
           (num-cols (cadr (array-dimensions a)))
           (result-array (make-array (list num-cols num-rows)
                            :element-type 'character :initial-element #\x)))
        (loop
            for row from 0 upto (- num-rows 1)
            do (loop
                for col from 0 upto (- num-cols 1)
                do (setf
                    (aref result-array col (- num-rows (+ row 1)))
                    (aref a row col))))
        result-array))

(defun flip-array (a)
    (let* ((num-rows (car (array-dimensions a)))
           (num-cols (cadr (array-dimensions a)))
           (result-array (make-array (list num-cols num-rows)
                            :element-type 'character :initial-element #\x)))
        (loop
            for row from 0 upto (- num-rows 1)
            do (loop
                for col from 0 upto (- num-cols 1)
                do (setf
                    (aref result-array col row)
                    (aref a row col))))
        result-array))

(defun make-dragon-mask (orientation)
    (let ((dragon (make-array '(3 20) :element-type 'character
            :initial-contents '("                  # "
                                "#    ##    ##    ###"
                                " #  #  #  #  #  #   "))))
        (case orientation
            (0 dragon)
            (1 (rotate-array dragon))
            (2 (rotate-array (rotate-array dragon)))
            (3 (rotate-array (rotate-array (rotate-array dragon))))
            (4 (flip-array dragon))
            (5 (rotate-array (flip-array dragon)))
            (6 (rotate-array (rotate-array (flip-array dragon))))
            (7 (rotate-array (rotate-array (rotate-array (flip-array dragon))))))))

(defun find-sea-dragon (buffer)
    ; assume no overlapping dragons.
    (loop
        for orientation from 0 upto 7
        do (let* ((dragon-mask (make-dragon-mask orientation))
                  (num-rows (car (array-dimensions dragon-mask)))
                  (num-cols (cadr (array-dimensions dragon-mask))))
            (loop
                for x-offset from 0 upto (- +buffer-size+ num-cols)
                do (loop
                    for y-offset from 0 upto (- +buffer-size+ num-rows)
                    ; check for dragon at this location
                    when (loop named dragon-check
                        for row from 0 upto (- num-rows 1)
                        do (loop
                            for col from 0 upto (- num-cols 1)
                            when (and (char= (aref dragon-mask row col) #\#)
                                      (char= (aref buffer (+ y-offset row) (+ x-offset col)) #\.))
                                ; no dragon
                                do (return-from dragon-check nil))
                        finally (return-from dragon-check t))
                    ; dragon exists
                    do (loop
                        for row from 0 upto (- num-rows 1)
                        do (loop
                            for col from 0 upto (- num-cols 1)
                            when (char= (aref dragon-mask row col) #\#)
                            do (setf (aref buffer (+ y-offset row) (+ x-offset col)) #\O)))))))
    buffer)

(let ((tiles (load-all #p"input20.txt"))
      (tile-map (make-hash-table :test #'equal)))
    (place-all-tiles tiles tile-map)
    ; part 1
    ;(format t "~S~%" tile-map)
    (let* ((extremes (get-extremes tile-map))
           (min-x (car extremes))
           (max-x (cadr extremes))
           (min-y (caddr extremes))
           (max-y (cadddr extremes))
           (corners (list
                        (cons min-x min-y)
                        (cons max-x min-y)
                        (cons min-x max-y)
                        (cons max-x max-y))))
        (format t "~a~%"
            (apply '* (mapcar
                (lambda (k) (getf (gethash k tile-map) :id))
                corners))))
    ; part 2
    (let ((buffer (tile-map-to-buffer tile-map)))
        (find-sea-dragon buffer)
        ;(display-buffer buffer)
        (format t "~a~%"
            (loop
                for row from 0 upto (- +buffer-size+ 1)
                sum (loop
                    for col from 0 upto (- +buffer-size+ 1)
                    count (char= (aref buffer row col) #\#))))))
