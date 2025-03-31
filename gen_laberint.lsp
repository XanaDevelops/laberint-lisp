
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
    ((laberint (create-matrix MIDA MIDA 'paret)))
    (setq pos (chooseRandomPos laberint))
    ; casella random como 'entrada
    (setq laberint (setMatrixValue laberint entrada pos))
    (setq currentI (car pos))
    (setq currentJ (cadr pos))
    ; (format t "currentI = ~A~%" currentI)
    ; (format t "currentJ= ~A~%" currentJ)

    (setq laberint (recursiveDFS currentI currentJ laberint))
    ;  (format t "laberint = ~A~%" laberint)
    (setq laberint (setMatrixValue laberint sortida (getRandomCamiPos laberint)))
    (setq laberint (setEdgesParet laberint))
    ; (format t "laberint = ~A~%" laberint)
    (writeToFile laberint "laberintGenerat.txt")
  )
)
; añade el borde superior
(defun setEdgesParet (laberint) 
  (cons (replicate (+ 2 mida) paret) (addParetToFila laberint))
)
; añade paret a los extremos de cada fila, y la fila final de paret
(defun addParetToFila (laberint) 
  (cond 
    ((null laberint) (list (replicate (+ 2 mida) paret)))
    (t
     (cons 
       (cons paret (snoc paret (car laberint)))
       (addParetToFila (cdr laberint))
     )
    )
  )
)
  

(defun getRandomCamiPos (laberint) 
  (let 
    ((i (random MIDA)) 
      (j (random MIDA))
    )

    (cond 

      ((equal (getIJLaberint (list i j) laberint) cami) (list i j))
      (t (getRandomCamiPos laberint))
    )
  )
)

(defun writeToFile (laberint file) 
  (let 
    ((stream 
       (open file :direction :output :if-exists :supersede :if-does-not-exist :create)
     ) 
    )
    (cond 
      (stream
       (writeRecursivly laberint stream)
       (close stream)
      )
    )
  )
)



(defun writeRecursivly (l stream) 
  (cond 
    ((null l) nil)
    (t
     (progn 

       (writeLine (car l) stream)
       (terpri stream)
       (writeRecursivly (cdr l) stream)
     )
    )
  )
)

(defun writeLine (line stream) 

  (cond 
    ((null line) nil)
    (t

     (progn 
       (write-char (convertirAChar (car line)) stream)
       (writeLine (cdr line) stream)
     )
    )
  )
)

(defun convertirAChar (valor) 

  (cond 
    ((equal valor sortida) Csortida)
    ((equal valor entrada) Centrada)
    ((equal valor paret) Cparet)
    ((equal valor cami) Ccami)
  )
)



; (defun recursiveDFS (currentI currentJ laberint) 
;   (cond 
;     ((not (paretIUnicCami (list currentI currentJ) laberint))
;      (format t "laberint = ~A~%" laberint)
;     )
;     (t
;      (let 
;        ; casella random adyacente
;        ((posActual (casellaAdjacentRandom laberint currentI currentJ)))
;        (format t "posActual = ~A~%" posActual)
;        (cond 
;          ((paretIUnicCami posActual laberint)
;           (setq laberint (setMatrixValue laberint 'canviat posActual))
;          )
;        )
;        ; llamada recursiva
;        (recursiveDFS (car posActual) (cadr posActual) laberint)
;      )
;     )
;   )
; )

(defun acabar (currentI currentJ laberint) 
  (not 
    (some 
      (lambda (pos) (paretIUnicCami pos laberint))
      (casellesAdjacents laberint currentI currentJ)
    )
  )
)

(defun recursiveDFS (currentI currentJ laberint) 
  (if (acabar currentI currentJ laberint) 
    ; (progn
    ;   (format t " laberint = ~A~%" laberint)
    laberint
    ; ) ; return laberint
    (let 
      ((posActual (casellaAdjacentRandom laberint currentI currentJ))) ; casella adjaçent random
      ; (format t "posActual = ~A~%" posActual)
      (if (paretIUnicCami posActual laberint) 
        (progn 
          (setq laberint (setMatrixValue laberint cami posActual))
          (recursiveDFS (car posActual) (cadr posActual) laberint)
        ) ; crida recursiva amb el mateix valor
        (recursiveDFS currentI currentJ laberint)
      )
    )
  )
)  ;; Si `posActual` no es válida, intenta con otra

; (defun acabar (currentI currentJ laberint) 
;   (every 

;     (lambda (pos) 
;       (not (paretIUnicCami pos laberint))
;     )
;     (casellesAdjacents laberint currentI currentJ)
;   )
; )

; true si ninguna casella comuple que :
; 1. su valor actual es paret
; 2. l'Unic cami adjcanet és la pos actual
(defun paretIUnicCami (posActual laberint) 

  (cond 

    ((and 
       (equal paret (getIJLaberint posActual laberint))
       (unicCami (car posActual) (cadr posActual) laberint)
     )
     t
    )

    (t nil)
  )
)




; la celda (i, j) tiene un unico camino/entrada adyacente?
(defun unicCami (i j laberint) 
  (let 
    ((adjacents 
       (casellesAdjacents laberint i j)
     ) 
    )

    (cond 
      ((= 1 
          (+ (repetitionsX cami (getValors adjacents laberint)) 
             (repetitionsX entrada (getValors adjacents laberint))
          )
       )
       t
      )
      (t nil)
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
(defun casellesAdjacents (matrix currentI currentJ) 
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
      ; devuelve una pos random
      (getElementI (random (length validPos)) validPos)
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
(defun prova () 

  (acabar 
    '0
    '0

    '((paret entrada paret paret paret)
      (paret paret cami paret paret)
      (paret paret paret paret paret)
      (paret paret paret cami paret)
      (paret paret paret paret paret)
     )
  )
)