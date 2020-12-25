(defun load-all (filename)
    ; returns a list ((deck1) (deck2))
    (with-open-file (stream filename)
        (list
            (mapcar #'parse-integer (cdr ; don't bother with "player 1" identifier
                (loop for line = (read-line stream nil)
                    while (and line (not (string= line "")))
                    collect line)))
            (mapcar #'parse-integer (cdr
                (loop for line = (read-line stream nil)
                    while line
                    collect line))))))

(defun play-round (decks)
    (let ((deck1 (car decks))
          (deck2 (cadr decks)))
        (if (> (car deck1) (car deck2))
            (setf deck1 (append deck1 (list (car deck1) (car deck2))))
            (setf deck2 (append deck2 (list (car deck2) (car deck1)))))
        (setf deck1 (cdr deck1)
              deck2 (cdr deck2))
        (list deck1 deck2)))

(declaim (ftype function play-game-recursive))

(defun round-winner-recursive (decks)
    ; returns 1 or 2
    (let ((deck1 (car decks))
          (deck2 (cadr decks)))
        (if (and (> (length deck1) (car deck1))
                 (> (length deck2) (car deck2)))
            ; subgame...
            (if (car (play-game-recursive (list
                    (subseq (copy-list (cdr deck1)) 0 (car deck1))
                    (subseq (copy-list (cdr deck2)) 0 (car deck2)))))
                1
                2)
            ; else
            (if (> (car deck1) (car deck2)) 1 2))))

(defun play-game-recursive (decks)
    ; returns (deck1 deck2)
    ;(format t "==Begin Game==~%")
    (let ((deck1 (car decks))
          (deck2 (cadr decks))
          (cache (make-hash-table :test #'equal)))
        (loop
            for round-index from 1
            ;do (format t "--Round ~a--~%P1: ~s~%P2: ~s~%" round-index deck1 deck2)
            while (and deck1 deck2)
            when (gethash (append (list (length deck1)) deck1 deck2) cache) do (return)
            do (setf (gethash (append (list (length deck1)) deck1 deck2) cache) t)
            do (let ((winner (round-winner-recursive (list deck1 deck2))))
                ;(format t "~a wins~%" winner)
                (if (= winner 1)
                    (setf deck1 (append deck1 (list (car deck1) (car deck2))))
                    (setf deck2 (append deck2 (list (car deck2) (car deck1)))))
                (setf deck1 (cdr deck1)
                      deck2 (cdr deck2))))
        ; If round ends due to cache hit, deck1 isn't nil.
        ; When caller sees deck1 is present, it will assume that Player 1 won.
        (list deck1 deck2)))

(defun score-deck (deck)
    (loop
        for val in (reverse deck)
        for i from 1
        sum (* i val)))

; part 1
(let ((decks (load-all #p"input22.txt")))
    (loop
        while (and (car decks) (cadr decks))
        do (setf decks (play-round decks)))
    (format t "~a~%" (cons (score-deck (car decks)) (score-deck (cadr decks)))))

; part 2
(let ((decks (load-all #p"input22.txt")))
    (setf decks (play-game-recursive decks))
    (format t "~a~%" (cons (score-deck (car decks)) (score-deck (cadr decks)))))
