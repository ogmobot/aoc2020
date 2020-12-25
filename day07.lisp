(require :uiop)

(defun load-strings (filename)
    (with-open-file (stream filename)
        (loop for line = (read-line stream nil)
            while line
            collect line)))

(defun lines->words (s)
    ; converts line to ("light" "red" "bags" ... )
    (loop
        for protoword in (uiop:split-string s)
        collect (let ((last-index (- (length protoword) 1)))
            (if (member (char protoword last-index) '(#\, #\.) :test #'char=)
                (subseq protoword 0 last-index)
                protoword))))

(defun join-words (a b)
    (format nil "~a ~a" a b))

(defun words->rules (words)
    ; converts ("light" "red" "bags" ... )
    ;       to ("light red" (1 . "bright white") (2 . "muted yellow"))
    ; probs need to refactor rules structure for big boy input
    ; so that each colour records its parents
    (cons
        (join-words (nth 0 words) (nth 1 words))
        (if (not (string= (nth 4 words) "no"))
            (loop
                for n = 4 then (+ n 4)
                while (< n (length words))
                collect (cons
                    (parse-integer (nth n words))
                    (join-words (nth (+ n 1) words) (nth (+ n 2) words)))))))

(defun get-rule (rules parent-colour)
    (loop for rule in rules
        when (string= (car rule) parent-colour)
        return rule))
    
(defun parents (rules child-colour)
    ; gets all parents of that child
    ; probs need to refactor based on new rules structure for big boy input
    (loop for rule in rules
        if (member child-colour (mapcar #'cdr (cdr rule)) :test #'string=)
        collect (car rule)))

(defun make-tree (rules root)
    ; treenode: (colour (subtree) (subtree))
    (cons root (mapcar (lambda (x) (make-tree rules x)) (parents rules root))))

(defun flatten (tree)
    (if (null tree)
        nil
        (if (atom (car tree))
            (cons (car tree) (flatten (cdr tree)))
            (append (flatten (car tree)) (flatten (cdr tree))))))

(defun get-colours (rules root-colour)
    (remove-duplicates (flatten (make-tree rules root-colour))))

(defun get-weight (rules colour)
    ; cache known colours for big boy
    (let ((rule (get-rule rules colour)))
        (if (cdr rule)
            (+ 1 (apply '+ (mapcar
                (lambda (pair) (* (car pair) (get-weight rules (cdr pair))))
                (cdr rule))))
            1)))

(let ((rules (mapcar #'words->rules (mapcar #'lines->words (load-strings #p"input07.txt")))))
    (format t "~a~%" (- (length (get-colours rules "shiny gold")) 1))
    (format t "~a~%" (- (get-weight rules "shiny gold") 1)))
    ;(format t "(time: ~6fs)~%" (/ (get-internal-run-time) internal-time-units-per-second)))
