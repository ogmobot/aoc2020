(require :sb-sprof)

(defun load-lines (filename)
    (with-open-file (stream filename)
        (loop for line = (read-line stream nil)
            while line
            collect line)))

(defun get-coords (text)
    (let ((location (cons 0 0))
          (last-seen #\x)) ; gets set to #\n or #\s
        (loop
            for c across text
            when (or (char= c #\e) (char= c #\w))
            do (let ((delta (cond
                        ((char= last-seen #\n)
                            (if (char= c #\e)
                                (cons 1 -1)
                                (cons -1 -1)))
                        ((char= last-seen #\s)
                            (if (char= c #\e)
                                (cons 1 1)
                                (cons -1 1)))
                        (t
                            (if (char= c #\e)
                                (cons 2 0)
                                (cons -2 0))))))
                    (setf location
                        (cons (+ (car location) (car delta))
                              (+ (cdr location) (cdr delta)))))
            do (setf last-seen c))
        location))

(defun flip-tiles (lines)
    (let ((tiles (make-hash-table :test #'equalp)))
        (loop
            for line in lines
            for coords = (get-coords line)
            for current = (gethash coords tiles)
            do (setf (gethash coords tiles) (not current)))
        tiles))

(defun tile-neighbours (coord)
    (let ((deltas '(( 0 .  0)
                    (-2 .  0)
                    ( 2 .  0)
                    (-1 .  1)
                    ( 1 .  1)
                    (-1 . -1)
                    ( 1 . -1))))
        (mapcar
            (lambda (delta) (cons (+ (car coord) (car delta))
                                  (+ (cdr coord) (cdr delta))))
            deltas)))

(defun update-tiles (tiles)
    (let ((neighbour-map (make-hash-table :test #'equalp))
          (new-tiles (make-hash-table :test #'equalp)))
        (loop
            for coord being the hash-key
            using (hash-value v) of tiles
            when v
            do (loop
                for n-coord in (tile-neighbours coord)
                do (let ((current-value (or (gethash n-coord neighbour-map) 0)))
                    (setf (gethash n-coord neighbour-map) (+ 1 current-value)))))
        (loop
            for coord being the hash-key
            using (hash-value num-neighbours) of neighbour-map
            when (or (and (gethash coord tiles) (<= 1 (- num-neighbours 1) 2))
                     (and (not (gethash coord tiles)) (= num-neighbours 2)))
            do (setf (gethash coord new-tiles) t))
        new-tiles))

(let* ((lines (load-lines #p"input24.txt"))
       (tiles (flip-tiles lines)))
    ; part 1
    (format t "~a~%"
    (loop
        for k being the hash-value of tiles
        count k))
    ; part 2
    (loop
        for time-index from 1 upto 100
        do (setf tiles (update-tiles tiles)))
    (format t "~a~%"
    (loop
        for k being the hash-value of tiles
        count k)))
