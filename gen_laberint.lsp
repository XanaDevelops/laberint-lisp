
(load "libs/listLib.lsp")
(load "VARSGLOBALS.lsp")
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
; Function to set a random matrix value to 'value
; pos = (i,j)
(defun setMatrixValue (matrix value pos) 

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
    (setq laberint (setMatrixValue laberint entrada pos))
    (setq currentI (car pos))
    (setq currentJ (cadr pos))
    (format t "currentI = ~A~%" currentI)
    (format t "currentJ= ~A~%" currentJ)
    (format t "laberint = ~A~%" laberint)
    (casellaAdjacentRandom laberint currentI currentJ)
  )
)

(defun paretIUnicCami (currentI currentJ laberint) 

  (cond 
    ((not equal (getIJLaberint (list (currentI currentJ) laberint)) paret) (nil))
    ((equal nil (unicCami (currentI currentJ laberint))) nil)
    (t t)
  )
)

(defun prova () 

  (unicCami 
    '1
    '2
    '((paret paret paret paret paret)
      (paret paret cami cami paret)
      (paret paret cami cami paret)
      (paret cami cami cami paret)
      (paret paret paret paret paret)
     )
  )
)

(defun unicCami (i j laberint) 
  (let 
    ((adjacents 
       (casellaAdjacentRandom laberint i j)
     ) 
    )

    (cond 
      ( ; dado que la casella actual o es entrada/paret.
       ; Solo verificaremos que ninguna de las casella vecinas sea cami
       (= 0 (repetitionsX cami (getValors adjacents laberint)))

       t
      )
    )
  )
)

  ; Dada una llista de incides (i, j) crea una lista con sus correspondientes
  ;valores del laberint
(defun getValors (pos laberint) 
  (cond 
    ((null pos) nil)
    (t (cons (getIJLaberint (car pos) laberint) (getValors (cdr pos) laberint)))
  )
)
  ; Devuelve el valor de l = (i,j) del laberinto. Más explicación no puedo dar :)
(defun getIJLaberint (l laberint) 
  (let 
    ((i (car l)) 
      (j (cadr l))
    )
    (cond 
      ((/= 0 i)
       (getIJLaberint (list (- i 1) j) (cdr laberint))
      )
      (t (getElementI j (car laberint)))
    )
  )
)






  ; retorna un casella adyacente random a la posicion (currentI, currentJ)
(defun casellaAdjacentRandom (matrix currentI currentJ) 
  (let 
    ((newPos 
       (mapcar 
         (lambda (llista) 
           (list (+ currentI (car llista)) (+ currentJ (cadr llista)))
         )
         displacements
       )
     ) 
    )
    ; validPos és una llista de llistes
    (let 
      ((validPos (removeElementOFB newPos matrix)))
      validPos
    )
  )
)

  ; Elimina pares (x y) que etan fuera de indice (Out of Bound)
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