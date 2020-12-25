(defun load-strings (filename)
    (with-open-file (stream filename)
        (loop for line = (read-line stream nil)
            while line
            collect line)))

(defun test-1 (lower upper char-req passwd)
            (and (>= (count char-req passwd) lower) (<= (count char-req passwd) upper)))

(defun test-2 (lower upper char-req passwd)
            (not (eql (eql char-req (char passwd (- lower 1))) (eql char-req (char passwd (- upper 1))))))

(defun valid-passwd (text test)
    (let
        ((sep-hyphen (search "-" text))
         (sep-space (search " " text))
         (sep-colon (search ": " text)))
        (funcall test
            (parse-integer (subseq text 0 sep-hyphen))
            (parse-integer (subseq text (+ 1 sep-hyphen) sep-space))
            (char text (+ 1 sep-space))
            (subseq text (+ 2 sep-colon)))))

(let ((strings (load-strings #p"input02.txt")))
    (format t "~a~%" (length (remove-if-not (lambda (x) (valid-passwd x #'test-1)) strings)))
    (format t "~a~%" (length (remove-if-not (lambda (x) (valid-passwd x #'test-2)) strings))))
