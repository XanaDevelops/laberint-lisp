
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

; Function to set a random matrix value to 'entada
(defun setEntrada (matrix value) 
  (let 
    ((i (random (length matrix))) 
      (j (random (length (car matrix))))
    )
    ; (format t "i = ~A~%" i)
    ; (format t "j = ~A~%" i)
    (setq matrix (setValue matrix i j value))
  )
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



