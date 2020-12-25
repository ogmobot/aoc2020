(defun load-lines (filename)
    (with-open-file (stream filename)
        (loop for line = (read-line stream nil)
            while line
            collect line)))

(defun match-left-to-right (text lbracket)
    ; returns the index *after* the matching rbracket
    (let ((opens 1))
        (loop
            for c across (subseq text (+ lbracket 1))
            for index = (+ lbracket 1) then (+ 1 index)
            while (> opens 0)
            when (char= c #\() do (incf opens  1)
            when (char= c #\)) do (incf opens -1)
            finally (return index))))

(defun match-left-to-right-derp (text lbracket)
    ; returns the index *after* the matching rbracket
    (let ((opens 1))
        (loop
            for c across (subseq text (+ lbracket 1))
            for index = (+ lbracket 1) then (+ 1 index)
            while (> opens 0)
            when (char= c #\)) do (incf opens  1)
            when (char= c #\() do (incf opens -1)
            finally (return index))))


(defun match-right-to-left (text rbracket)
    ; returns the index *of* the matching rbracket
    (let ((opens 1))
        (loop
            for c across (reverse (subseq text 0 (- rbracket 1)))
            for index = (- rbracket 1) then (- index 1)
            while (> opens 0)
            when (char= c #\)) do (incf opens  1)
            when (char= c #\() do (incf opens -1)
            finally (return (- index 1)))))
            
(defun build-tree-from-right (text)
    ; tree nodes go (function (ltree) (rtree))
    (let ((op nil) (ltree nil) (rtree nil) (opindex nil))
        (cond
        ((= (length text) 1) (parse-integer text))
        ((> (length text) 0)
            (progn
            ;(format t "~S~%" text)
            ; left tree
            (if (char= (char text 0) #\()
                (setf
                    opindex (+ 1 (match-left-to-right text 0))
                    ltree (build-tree-from-right (subseq text 1 (match-left-to-right text 0))))
                ; all numbers are 1 character long
                (setf
                    opindex 2
                    ltree (parse-integer (subseq text 0 1))))
            ; operator
            (if (< opindex (length text))
                (progn
                    (if (char= (char text opindex) #\+)
                        (setf op '+)
                        (setf op '*))
                    ; right tree
                    (setf rtree (build-tree-from-right (subseq text (+ opindex 2))))
                    (list op ltree rtree))
                ; entire expression is bracketed
                ltree))))))

(defun build-tree-from-right-derp (text)
    ; tree nodes go (function (ltree) (rtree))
    (let ((op nil) (ltree nil) (rtree nil) (opindex nil))
        (cond
        ((= (length text) 1) (parse-integer text))
        ((> (length text) 0)
            (progn
            ;(format t "~S~%" text)
            ; left tree
            (if (char= (char text 0) #\))
                (setf
                    opindex (+ 1 (match-left-to-right-derp text 0))
                    ltree (build-tree-from-right-derp (subseq text 1 (match-left-to-right-derp text 0))))
                ; all numbers are 1 character long
                (setf
                    opindex 2
                    ltree (parse-integer (subseq text 0 1))))
            ; operator
            (if (< opindex (length text))
                (progn
                    (if (char= (char text opindex) #\+)
                        (setf op '+)
                        (setf op '*))
                    ; right tree
                    (setf rtree (build-tree-from-right-derp (subseq text (+ opindex 2))))
                    (list op ltree rtree))
                ; entire expression is bracketed
                ltree))))))

(defun build-rpn-list (text)
    (let ((output '()) (opstack '()))
    (loop
        for token across text
        ;do (format t "~S ~S~%" token opstack)
        when (not (char= token #\Space))
        do (cond
            ((char<= #\0 token #\9)
                (setf output (append output (list token))))
            ((char= token #\()
                (push token opstack))
            ((char= token #\))
                (loop
                    while (not (char= (car opstack) #\())
                    do (setf output (append output (list (pop opstack))))
                    finally (pop opstack))) ;discard #\( token
            ((char= token #\*)
                (loop
                    while (and
                            opstack
                            (not (char= (car opstack) #\()))
                    do (setf output (append output (list (pop opstack))))
                    finally (push token opstack)))
            ((char= token #\+)
                (push token opstack))))
    (loop
        while opstack
        do (setf output (append output (list (pop opstack)))))
    output))

(defun eval-rpn (rpn)
    (let ((stack '()))
        (loop
            for token in rpn
            do (cond
                ((char<= #\0 token #\9)
                    (push (digit-char-p token) stack))
                ((char= token #\+)
                    (push (+ (pop stack) (pop stack)) stack))
                ((char= token #\*)
                    (push (* (pop stack) (pop stack)) stack))))
        (car stack)))

(let ((lines (load-lines #p"input18.txt")))
    ; part 1
    (format t "~a~%"
        (reduce '+ (mapcar
            (lambda (x)
                (progn
                    ;(format t "~S~%    ~S~%" x (build-tree-from-right-derp (reverse x)))
                    (eval (build-tree-from-right-derp (reverse x)))))
            lines)))
    ; part 2
    (format t "~a~%"
        (reduce '+ (mapcar
            (lambda (x)
                (progn
                    ;(format t "~S~%    ~S~%" x (build-rpn-list x))
                    (eval-rpn (build-rpn-list x))))
            lines))))
