(require :uiop)

(defun load-strings (filename)
    (with-open-file (stream filename)
        (loop for line = (read-line stream nil)
            while line
            collect line)))

(defun load-memory (filename)
    (mapcar
        (lambda (line)
            (let ((words (uiop:split-string line)))
                (cons (car words) (parse-integer (cadr words)))))
        (load-strings filename)))

(defun cpu-step (cpu)
    ; cpu is (instruction-pointer accumulator memory-list)
    ; each element of memory-list is of the form ("cmd" . val)
    (let ((opcode (nth (car cpu) (caddr cpu))))
        ;(format t "~a~%" opcode)
        (setf (car cpu) (+ (car cpu)
            (cond
                ((string= (car opcode) "nop") ; do nothing
                    1) ; ip++
                ((string= (car opcode) "acc") ; update acc
                    (setf (cadr cpu) (+ (cadr cpu) (cdr opcode)))
                    1) ; ip++
                ((string= (car opcode) "jmp") ; change ip
                    (cdr opcode))
                (t 1)))))
    cpu)

(defun cpu-run-until-loop (cpu)
    (let ((seen-ips `(,(length (caddr cpu)))))
        (loop
            while (not (member (car cpu) seen-ips))
            do (progn
                ;(format t "~a ~S~%" (cadr cpu) seen-ips)
                (setq seen-ips (cons (car cpu) seen-ips))
                (cpu-step cpu))
            finally (return (cons
                                (cadr cpu)
                                (if (= (car cpu) (length (caddr cpu)))
                                    :okay
                                    :crash))))))

(defun toggle-instruction (cpu addr)
    (setf (car (nth addr (caddr cpu)))
        (cond
            ((string= (car (nth addr (caddr cpu))) "nop") "jmp")
            ((string= (car (nth addr (caddr cpu))) "jmp") "nop")
            (t (car (nth addr (caddr cpu))))))
    cpu)

(defun deep-copy (l)
    (cond
        ((null l) nil)
        ((atom l) l)
        (t (cons (deep-copy (car l)) (deep-copy (cdr l))))))
        
(defun new-cpu (rom)
    `(0 0 ,(deep-copy rom)))

(let ((rom (load-memory #p"input08.txt")))
    ; part 1
    (let ((cpu (new-cpu rom)))
        (format t "~a~%" (car (cpu-run-until-loop cpu))))
    ; part 2
    (format t "~a~%"
    (block outer
        (mapcar
            (lambda (addr)
                (let* ((cpu (toggle-instruction (new-cpu rom) addr))
                       (result (cpu-run-until-loop cpu)))
                    (if (eql (cdr result) :okay)
                        (return-from outer (car result)))))
            (loop for n from 0 upto (- (length rom) 1) collect n)))))
