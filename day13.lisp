(require :uiop)

(defun load-lines (filename)
    (with-open-file (stream filename)
        (let ((arrival-time (parse-integer (read-line stream nil)))
              (bus-times (read-line stream nil)))
            (cons
                arrival-time
                (loop for word in (uiop:split-string bus-times :separator ",")
                    collect (if (char= (char word 0) #\x)
                                'x
                                (parse-integer word)))))))

(defun mod-inverse (a b) ; extended euclidean algorithm, without t_i
    ; finds multiplicative inverse of a mod b (= a^)
    ; so that a * (a^) mod b = 1
    (let ((ss '(0 1))
          ;(ts '(1 0))
          (rs (list b a)))
        (loop
            while (/= 0 (car rs))
            for quotient = (floor (/ (cadr rs) (car rs)))
            do (setf ss (cons (- (cadr ss) (* quotient (car ss))) ss))
            ;do (setf ts (cons (- (cadr ts) (* quotient (car ts))) ts))
            do (setf rs (cons (- (cadr rs) (* quotient (car rs))) rs)))
        (cadr ss)))

(defun get-vals-mods (bus-ids)
    (loop
        for bus-id in bus-ids
        for index = 0 then (+ index 1)
        when (not (equal bus-id 'x)) collect (- index) into vals
        when (not (equal bus-id 'x)) collect bus-id into mods
        finally (return (list vals mods))))

(defun solve (vals mods)
    (mod
        (apply '+ (loop
            for a in vals
            for m in mods
            for index = 0 then (+ index 1)
            collect (*
                a
                (reduce '* (loop
                    for m* in mods
                    for index* = 0 then (+ index* 1)
                    when (/= index index*) collect m*
                    when (/= index index*) collect (mod-inverse m* m))))))
        (apply '* mods)))

(let* ((data (load-lines "input13.txt"))
       (arrival-time (car data))
       (bus-ids (cdr data)))
    ; part 1
    (defun bus-time (bus-id)
        (- bus-id (mod arrival-time bus-id)))
    (let ((best-bus
        (reduce
            (lambda (a b) (if (< (bus-time a) (bus-time b)) a b))
            (remove-if (lambda (a) (equal a 'x)) bus-ids))))
        (format t "~a~%" (* best-bus (bus-time best-bus))))
    ;part 2
    (format t "~a~%"
        (apply #'solve (get-vals-mods bus-ids))))
