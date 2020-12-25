(require :uiop)

(defun load-all (filename)
    ; returns a list (rules-text my-ticket nearby-tickets)
    (with-open-file (stream filename)
        (list
            (loop for line = (read-line stream nil)
                while (and line (not (string= line "")))
                collect line)
            (loop for line = (read-line stream nil)
                while (and line (not (string= line "")))
                collect line)
            (loop for line = (read-line stream nil)
                while line
                collect line))))

(defun line-to-range-fn (text)
    ; returns a function that returns t if a number is within the range
    ; e.g. "departure location: 50-688 or 707-966" => f(x); f(70) => t
    (let* ((pos-colon (position #\: text))
           (pos-dash1 (+ pos-colon (position #\- (subseq text pos-colon))))
           (pos-or    (+ pos-dash1 (position #\o (subseq text pos-dash1))))
           (pos-dash2 (+ pos-or    (position #\- (subseq text pos-or   ))))

           (lower-a   (parse-integer (subseq text (+ pos-colon 1) pos-dash1)))
           (upper-a   (parse-integer (subseq text (+ pos-dash1 1) pos-or   )))
           (lower-b   (parse-integer (subseq text (+ pos-or    2) pos-dash2)))
           (upper-b   (parse-integer (subseq text (+ pos-dash2 1)))))
        (lambda (x)
            (or (<= lower-a x upper-a) (<= lower-b x upper-b)))))

(defun find-invalid-values (ticket-vals field-functions)
    ; returns a list of invalid values found in the ticket.
    ; ticket is a list of ints.
    (loop
        for val in ticket-vals
        when (not (reduce
            (lambda (x y) (or x y))
            (mapcar (lambda (fn) (funcall fn val)) field-functions)))
        collect val))

(defun rule-works-for-index (fn tickets index)
    (reduce
        (lambda (x y) (and x y))
        (mapcar
            (lambda (tick) (funcall fn (nth index tick)))
            tickets)))

(defun find-field-order-new (rule-strings tickets)
    ; use assoc list
    (let ((rules (pairlis
                    (mapcar (lambda (s) (subseq s 0 (position #\: s))) rule-strings)
                    (mapcar #'line-to-range-fn rule-strings)))
          (fields (make-array (length rule-strings) :initial-element nil)))

        (defun possibles (rule fields)
            (loop
                for index from 0 upto (- (length rules) 1)
                when (and
                    (rule-works-for-index (cdr rule) tickets index)
                    (null (aref fields index)))
                collect index))
        (loop
            while (> (count-if #'null fields) 0)
            do (loop
                for rule in rules
                do (if (= 1 (length (possibles rule fields)))
                    (setf (aref fields (car (possibles rule fields))) (car rule)))))
        fields))

(defun starts-with (long short)
    (and
        (>= (length long) (length short))
        (string= (subseq long 0 (length short)) short)))

(let* ((data (load-all #p"input16.txt"))
       (rule-strings (car data))
       (my-ticket (cadadr data))
       (nearby-tickets (mapcar
                            (lambda (ticket)
                                (mapcar
                                    #'parse-integer
                                    (uiop:split-string ticket :separator ",")))
                            (cdaddr data)))
       (rule-fns (mapcar #'line-to-range-fn rule-strings)))
    ; part 1
    (format t "~a~%"
    (apply '+
        (apply #'append
            (mapcar (lambda (s) (find-invalid-values s rule-fns)) nearby-tickets))))
    ; part 2
    (let* ((valid-tickets
                (remove-if (lambda (s) (find-invalid-values s rule-fns)) nearby-tickets))
           (field-order
                (find-field-order-new rule-strings valid-tickets)))
        ;(loop
            ;for field across field-order
            ;for val in (uiop:split-string my-ticket :separator ",")
            ;do (format t "~a: ~a~%" field val))
        (format t "~a~%"
            (apply '*
                (loop
                    for num in (uiop:split-string my-ticket :separator ",")
                    for field across field-order
                    when (starts-with field "departure")
                collect (parse-integer num))))))
