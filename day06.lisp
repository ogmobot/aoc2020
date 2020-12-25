(defun load-records (filename)
    (with-open-file (stream filename)
        (loop for line = (read-line stream nil)
            while line
            collect (mapcar (lambda (s) (coerce s 'list))
                (cons line
                    (loop for subline = (read-line stream nil)
                        while (and subline (not (string= subline "")))
                        collect subline))))))

(defun solve (reduce-function records)
    (apply '+
        (mapcar #'length
            (mapcar (lambda (r) (reduce reduce-function r)) records))))

(let ((records (load-records #p"input06.txt")))
    (format t "~a~%" (solve #'union records))
    (format t "~a~%" (solve #'intersection records)))
