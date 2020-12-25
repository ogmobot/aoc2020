(require :uiop)

(defun remove-non-alnum (s)
    (format nil "~{~^~a~}"
        (loop for ch across s
            when (or (char<= #\a ch #\z) (char<= #\A ch #\Z) (char<= #\0 ch #\9))
            collect ch)))

(defun line-to-food (line)
    ; converts a line to (:ingredients () :allergens ())
    (let ((lbracket-pos (position #\( line)))
        (list
            :ingredients (uiop:split-string (subseq line 0 (- lbracket-pos 1)))
            :allergens (mapcar
                #'remove-non-alnum
                (uiop:split-string (subseq line (+ lbracket-pos 10)))))))

(defun all-allergens (foods)
    (reduce
        (lambda (x y) (union x y :test #'string=))
        (mapcar (lambda (f) (getf f :allergens)) foods)))

(defun allergen-options (allergen foods)
    ; returns a list of ingredients that could be the allergen
    (reduce (lambda (x y) (intersection x y :test #'string=))
        (mapcar
            (lambda (f) (getf f :ingredients))
            (remove-if-not
                (lambda (x) (position allergen (getf x :allergens) :test #'string=))
                foods))))

(defun assign-allergen (foods allergen-list)
    ; allergen-list is an a-list (("dairy" . "mxmxvkd") ("fish" . "sqjhc") ... )
    (loop
        for allergen in (all-allergens foods)
        when (not (assoc allergen allergen-list :test #'string=)) ; haven't assigned it yet
        do (let ((possibilities
                    (remove-if
                        (lambda (ingredient) (rassoc ingredient allergen-list :test #'string=))
                        (allergen-options allergen foods))))
            (if (= (length possibilities) 1)
                (progn
                    (setf
                        allergen-list
                        (acons allergen (car possibilities) allergen-list))
                    (return allergen-list))))))

(uiop:with-safe-io-syntax () :body
(let ((foods (mapcar #'line-to-food (uiop:read-file-lines #p"input21.txt")))
      (allergen-list '()))
    (loop
        while (< (length allergen-list) (length (all-allergens foods)))
        do (setf allergen-list (assign-allergen foods allergen-list)))
    ; part 1
    (format t "~a~%"
        (loop
            for food in foods
            summing (length (remove-if
                (lambda (ingredient) (rassoc ingredient allergen-list :test #'string=))
                (getf food :ingredients)))))
    ; part 2
    (format t "~{~a~^,~}~%"
        (mapcar #'cdr (sort allergen-list (lambda (x y) (string<= (car x) (car y))))))))
