
; function to create an N*M matrix
; (defun create-matrix (n m) 
;   (cond 
;     ((= n 0) nil)
;     (t
;      (let 
;        ((row (make-array m :initial-element 'paret)))
;        (append (list row) (create-matrix (1- n) m))
;      )
;     )
;   )
; )
(setq displacements '((1 0) (-1 0) (0 1) (0 -1)))
; create a 2D array inicialised to valor
(defun create-matrix (n m value) 
  (cond 
    ((= n 0) nil)
    (t
     (cons (replicate m value) 
           (create-matrix (- n 1) m value)
     )
    )
  )
)



; Create a list with n repetitions of the element e
(defun replicate (n e) 
  (cond 
    ((= 0 n) nil)
    (t (cons e (replicate (- n 1) e)))
  )
)

(defun chooseRandomPos (matrix) 
  (let 
    ((i (random (length matrix))) 
      (j (random (length (car matrix))))
    )

    (list i j)
  )
)
; Function to set a random matrix value to 'entada
(defun setEntrada (matrix value pos) 

  (setq matrix (setValue matrix (car pos) (cadr pos) value))
)
; sets matrix[i][j] = value
(defun setValue (matrix i j value) 
  (cond 
    ((/= 0 i)
     (cons (car matrix) (setValue (cdr matrix) (- i 1) j value))
    )
    (t (cons (changeValue j (car matrix) value) (cdr matrix)))
  )
)
; sets llista[index]=value.
;Returns llista
(defun changeValue (index llista value) 
  (cond 
    ((= 0 index) (cons value (cdr llista)))
    (t (cons (car llista) (changeValue (- index 1) (cdr llista) value)))
  )
)
(defun genera-laberint () 

  ; (setq laberint (create-matrix '3 '3 'paret))
  (let 
    ((laberint (create-matrix '3 '3 'paret)))
    (setq pos (chooseRandomPos laberint))
    (setq laberint (setEntrada laberint 'entrada pos))
    (setq currentI (car pos))
    (setq currentJ (cadr pos))
    (format t "currentI = ~A~%" currentI)
    (format t "currentJ= ~A~%" currentJ)
    (format t "laberint = ~A~%" laberint)
    (casellaAdjacentRandom laberint currentI currentJ)
  )
)



(defun casellaAdjacentRandom (matrix currentI currentJ) 
  (let 
    ((newPos 
       (mapcar 
         (lambda (llista) 
           (list (+ currentI (car llista)) (+ currentJ (cadr llista)))
         )
         displacements
       )
     ) )
    (let ((validPos (removeElementOFB newPos matrix))) 
    validPos)
      
  
  )
)


(defun removeElementOFB (llista matrix) 
  (let 
    ((rows (length matrix)) (columns (length (car matrix))))
    (cond 
      ((null llista) nil)
      ((and (< (caar llista) rows) 
            (< (cadar llista) columns)
            (>= (caar llista) 0)
            (>= (cadar llista) 0)
       )
       (cons 
         (car llista)
         (removeElementOFB (cdr llista) matrix)
       )
      )
      (t (removeElementOFB (cdr llista) matrix))
    )
  )
)


