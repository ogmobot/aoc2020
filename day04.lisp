(defun load-records (filename)
    (with-open-file (stream filename)
        (loop for line = (read-line stream nil)
            while line
            collect (format nil "~{~a~^ ~}"
                (cons line 
                    (loop for subline = (read-line stream nil)
                        while (and subline (not (string= subline "")))
                        collect subline))))))

(defun split (delimiter text)
    (loop for i = 0 then (+ 1 j)
        as j = (position delimiter text :start i)
        collect (subseq text i j)
        while j))
;; alternatively:
; (require :uiop)
;; then use
; (uiop:split-string text '(#\:))
; (uiop:split-string text '(#\Space))

(defun record->list (record)
    ; "a:b c:d" -> (("a" "b") ("c" "d"))
    (mapcar (lambda (x) (split #\: x)) (split #\Space record)))

(defun get-field (field record)
    (if record
        (if (string= field (caar record))
            (cadar record)
            (get-field field (cdr record)))))

(defun valid-hexchar (c)
    (or (char<= #\0 c #\9) (char<= #\a c #\f)))

(defun valid-haircolour (text)
    (and
        (= (length text) 7)
        (reduce (lambda (x y) (and x y))
            (loop for n from 0 upto 6
                collect (or
                    (and (= n 0) (char= (char text n) #\#))
                    (and (> n 0) (valid-hexchar (char text n))))))))

(defun all-digits (text)
    (reduce (lambda (x y) (and x y))
        (loop for n from 0 to (- (length text) 1)
            collect (char<= #\0 (char text n) #\9))))

(defun valid-height (text)
    (let ((tlen (length text)))
    (if (> tlen 2)
        (and
            (all-digits (subseq text 0 (- tlen 2)))
            (or
                (and (string= (subseq text (- tlen 2)) "cm")
                     (<= 150 (parse-integer text :junk-allowed t) 193))
                (and (string= (subseq text (- tlen 2)) "in")
                     (<= 59 (parse-integer text :junk-allowed t) 76)))))))

(defun validate (record)
    (and
        (get-field "byr" record)
        (get-field "iyr" record)
        (get-field "eyr" record)
        (get-field "hgt" record)
        (get-field "hcl" record)
        (get-field "ecl" record)
        ;(get-field "cid" record)
        (get-field "pid" record)))

(defun validate-strict (record)
    (let ((byr (get-field "byr" record))
          (iyr (get-field "iyr" record))
          (eyr (get-field "eyr" record))
          (hgt (get-field "hgt" record))
          (hcl (get-field "hcl" record))
          (ecl (get-field "ecl" record))
          ;(cid (get-field "cid" record))
          (pid (get-field "pid" record)))
    (if (validate record)
        (and
            (all-digits byr) (<= 1920 (parse-integer byr) 2002)
            (all-digits iyr) (<= 2010 (parse-integer iyr) 2020)
            (all-digits eyr) (<= 2020 (parse-integer eyr) 2030)
            (valid-height hgt)
            (member ecl '("amb" "blu" "brn" "gry" "grn" "hzl" "oth") :test #'string=)
            (valid-haircolour hcl)
            ;(valid-country cid)
            (all-digits pid) (= (length pid) 9)))))

(let ((records (mapcar #'record->list (load-records #p"input04.txt"))))
    (format t "~a~%" (count-if #'validate records))
    (format t "~a~%" (count-if #'validate-strict records)))
