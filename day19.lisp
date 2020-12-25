(require :cl-ppcre)
(require :uiop)

(defun load-all (filename)
    ; returns a list (rules-defs exprs)
    (with-open-file (stream filename)
        (list
            (loop for line = (read-line stream nil)
                while (and line (not (string= line "")))
                collect line)
            (loop for line = (read-line stream nil)
                while line
                collect line))))

(defun get-rule (rules num)
    ; if rules structure changes, update this!
    (cdr (assoc num rules)))

(defun text-to-regex-maker (text)
    ; text should be e.g.
    ;   "a"
    ;   4 1 5
    ; this returns a function f(rules) that outputs a regex.
    (if (char= (char text 0) #\")
        (lambda (rules)
            (declare (ignore rules))
            (subseq text 1 2))
        (lambda (rules)
            (apply #'concatenate
                (append (list 'string "(")
                (mapcar
                    (lambda (token)
                        (if (char= (char token 0) #\|)
                            "|"
                            (funcall (get-rule rules (parse-integer token)) rules)))
                    (uiop:split-string text))
                (list ")"))))))

(defun test-regex (regex line)
    (cl-ppcre:scan (concatenate 'string "^" regex "$") line))

(let* ((all-data (load-all #p"input19.txt"))
       (rules-text (car all-data))
       (expressions (cadr all-data))
       (rules '()))
    (loop
        for line in rules-text
        do (setf rules
            (acons
                (parse-integer line :junk-allowed t)
                (text-to-regex-maker (subseq line (+ 2 (position #\: line))))
                rules)))
    (let ((rule0 (funcall (get-rule rules 0) rules)))
        (format t "~a~%"
            (loop
                for expr in expressions
                count (test-regex rule0 expr))))
    ; replace rules 8 and 11 with custom regexes
    (let ((regex42 (funcall (get-rule rules 42) rules))
          (regex31 (funcall (get-rule rules 31) rules)))
        (setf
            rules
            (acons 8 (lambda (rs)
                        (declare (ignore rs))
                        (concatenate 'string regex42 "+"))
                    rules))
        (setf
            rules
            (acons 11 (lambda (rs)
                    (declare (ignore rs))
                    (concatenate 'string
                        "(" regex42 regex31
                        "|" regex42 "{2}" regex31 "{2}"
                        "|" regex42 "{3}" regex31 "{3}"
                        "|" regex42 "{4}" regex31 "{4}"
                        ")")) ; if it's stupid, but it works, it's not stupid.
                    rules)))
    (let ((rule0 (funcall (get-rule rules 0) rules)))
        ;(format t "~a~%(~a)~%" rule0 (length rule0))
        (format t "~a~%"
            (loop
                for expr in expressions
                count (test-regex rule0 expr)))))
