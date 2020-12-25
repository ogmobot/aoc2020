(defun load-nums (filename)
    (with-open-file (stream filename)
        (loop for line = (read-line stream nil)
            while line
            collect (parse-integer line))))

(defun differences (nums)
    (if (cdr nums) ; length >= 2
        (cons
            (- (car nums) (cadr nums))
            (differences (cdr nums)))))

(defun find-paths (remaining-nodes alist)
    ; alist stores number of paths from some nodes
    (let* ((current-node (car remaining-nodes))
           (options (list
                (assoc (+ current-node 1) alist)
                (assoc (+ current-node 2) alist)
                (assoc (+ current-node 3) alist)))) ; some options may be nil
        (let* ((path-sum (reduce '+ (mapcar (lambda (x) (if (null x) 0 (cdr x))) options)))
               (paths (if (= 0 path-sum)
                1 ; this must be the first go-through
                path-sum)))
            (if (= current-node 0)
                paths
                (find-paths (cdr remaining-nodes) (acons current-node paths alist))))))

(let* ((input-nums (sort (load-nums "input10.txt") '>))
       (lowest-num 0) (highest-num (+ 3 (car input-nums)))
       (all-nums (append (list highest-num) input-nums (list lowest-num))))
    (let ((diffs (differences all-nums)))
        (format t "~a~%"
            (* (count-if (lambda (x) (= x 3)) diffs)
               (count-if (lambda (x) (= x 1)) diffs))))
    (format t "~a~%"
        (find-paths all-nums '())))

