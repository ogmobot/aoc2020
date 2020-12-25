(require :uiop)

(defun load-lines (filename)
    (with-open-file (stream filename)
        (loop for line = (read-line stream nil)
            while line
            collect line)))

(defun starts-with (long short)
    (string= (subseq long 0 (length short)) short))

(defun apply-mask (mask value)
    (parse-integer
        (format nil "~{~^~a~}"
            (loop
                for m-char across mask
                for v-char across (format nil "~36,'0B" value)
                when      (char= m-char #\X)  collect v-char
                when (not (char= m-char #\X)) collect m-char))
        :radix 2))

(defun all-addresses (mask addr-int)
    ; returns a list of all memory addresses to write to
    (let ((addr (format nil "~36,'0B" addr-int)))
        (if (= (length addr) 0)
            (list "")
            (cond
                ((char= (char mask 0) #\0)
                    (mapcar
                        (lambda (s) (uiop:strcat (subseq addr 0 1) s))
                        (all-addresses (subseq mask 1) (subseq addr 1))))
                ((char= (char mask 0) #\1)
                    (mapcar
                        (lambda (s) (uiop:strcat "1" s))
                        (all-addresses (subseq mask 1) (subseq addr 1))))
                ((char= (char mask 0) #\X)
                    (append
                        (mapcar
                            (lambda (s) (uiop:strcat "1" s))
                            (all-addresses (subseq mask 1) (subseq addr 1)))
                        (mapcar
                            (lambda (s) (uiop:strcat "0" s))
                            (all-addresses (subseq mask 1) (subseq addr 1)))))
                (t (list ""))))))

(defun lines-to-memory-map (lines which-part)
    (let ((memory (make-hash-table))
          (mask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))
    (loop
        for line in lines
        when (starts-with line "mask")
            do (setf mask (subseq line 7))
        when (starts-with line "mem")
            do (let* ((pos-of-= (position #\= line))
                      (addr (parse-integer (subseq line 4 pos-of-=) :junk-allowed t))
                      (val (parse-integer (subseq line (+ pos-of-= 1)))))
                (if (= which-part 1)
                    (setf (gethash addr memory) (apply-mask mask val))
                    (loop
                        for bin-string in (all-addresses mask addr)
                        do (setf (gethash (parse-integer bin-string :radix 2) memory) val)))))
    memory))

(let ((lines (load-lines #p"input14.txt")))
    ; part 1
    (format t "~a~%"
        (apply '+
            (loop
                for value being the hash-values of (lines-to-memory-map lines 1)
                collect value)))
    ; part 2
    (format t "~a~%"
        (apply '+
            (loop
                for value being the hash-values of (lines-to-memory-map lines 2)
                collect value))))
