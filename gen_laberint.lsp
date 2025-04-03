
(load "libs/listLib.lsp")
(load "VARSGLOBALS.lsp")
(load "fileManagement.lsp")
(load "tco.lsp")

; Crea una llista de llista i la inicialitza amb 'value'
;;; @param n nombre de files
;;; @param m nombre de columnes
;;; @return llista de llistes
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


; Retorna posició aleatoria (i,j) de la matriu
(defun chooseRandomPos (matrix) 
  (let 
    ((i (random (length matrix))) 
      (j (random (length (car matrix))))
    )

    (list i j)
  )
)



  ;Estableix el valor (i,j) de 'matrix' a 'value', sent 'pos'=  (i,j)

(defun setMatrixValue (matrix value pos) 

  (setq matrix (setValue matrix (car pos) (cadr pos) value))
)

; sets matrix[i][j] = value


;  Estableix el valor 'value' a la posició (i,j) de matrix

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





(defun acabar (currentI currentJ laberint) 
  (not 
    (some 
      (lambda (pos) (paretIUnicCami pos laberint))
      (casellesAdjacents laberint currentI currentJ)
    )
  )
)


(defun-tco 
  recursiveDFS
  (currentI currentJ laberint)
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
) 

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
      ((>= 1 
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


(defun acabarv2 (laberint) 

  (let 
    ((cords (generateCoordenates (- mida 1))))
    (not 
      (some 
        (lambda (pos) (acabar (car pos) (cadr pos) laberint))
        cords
      )
    )
  )
)



(defun createList (max) 
  (cond 
    ((= 0 max) (list 0))
    (t (append (createList (- max 1)) (list max)))
  )
)

(defun permutation (l1 l2) 
  (cond 
    ((null l1) nil)
    (t
     (append (subperm (car l1) l2) 
             (permutation (cdr l1) l2)
     )
    )
  )
)

(defun subperm (v l) 
  (cond 
    ((null l) nil)
    (t (cons (list v (car l)) (subperm v (cdr l))))
  )
)

(defun generateCoordenates (max) 
  ; generar una lista de 0..max
  (let 
    ((l1 (createList max)) (l2 (createList max)))
    (let 
      ((lparells (permutation l1 l2)))
      lparells
    )
  )
)


(defun randomizedPrim () 

  ;Inicialitzar el laberint a 'paret
  (let* 
    ((laberint (create-matrix mida mida paret)) 

      (pos (chooseRandomPos laberint))
      (laberint (setMatrixValue laberint cami pos))
      (paretsL (getParets laberint pos))
      (laberint (recursivePrim paretsL laberint))
    )
    (format t "POS: ~a~%" pos)
    (recursivePrim paretsL laberint)
  )



  ; )
  ; (setq )
  ; ;llista buida de parets
  ; (setq )
  ; ; triar posicio random i establir-la a 'cami
  ; (setq))
  ; (setq laberint (setMatrixValue laberint cami pos))
  ; ;afegir les partes de pos a la llista de parets
  ; (setq )

  ; (setq laberint )
)
(defun recursivePrim (paretsL laberint) 
  (cond 
    ((null paretsL)
      (writeToFile laberint "laberintGenerat.txt" )
    ;  laberint
    )
    (t
     (let* 
       ((randomWall (chooseRandomWall paretsL)) 
         (cell (oneAdjacentVisited randomWall laberint))
       )
      ;  (format t "randomWall:~a~%" randomWall)
      ;  (format t "laberint: ~a~%" laberint)
      ;  (format t "Parets:~a~%" paretsL)

       (cond 
         ((not (null cell))
          (let* 
            ((newLaberint (setMatrixValue laberint cami cell)) 

              (nl (setMatrixValue newLaberint cami randomWall))
              (newParetsL 
                (addParet 
                  paretsL
                  (getParets laberint cell)
                )
              )
            )
            (progn 

              ; (format t "CELL ~a~%" cell)
              (format t "newParets:~a~%" newParetsL)
              (format t "Newlaberint: ~a~%" nl)
              ; (progn
              (let 
                ((np (remove randomWall newParetsL)))
                (recursivePrim np nl)
              )
            )

            ; )
          )
         )


         (t (recursivePrim (remove randomWall paretsL) laberint))
       )
     )
    )
  )
)

(defun chooseRandomWall (walls) 
  (let 
    ((pos (random (length walls))))

    (nth pos walls)
  )
)



(defun addParet (l1 l2) 
  (cond 
    ((null l2) l1)
    ((pairMember (car l2) l1) (addParet l1 (cdr l2)))
    (t (addParet (snoc (car l2) l1) (cdr l2)))
  )
)

(defun pairMember (pair l) 
  (cond 
    ((null l) nil)
    ((equal pair (car l)) t)
    (t (pairMember pair (cdr l)))
  )
)

  ; crea una llista de les parets adjaçents a posActual
(defun getParets (laberint posActual) 

  (let 
    ((adjacents (casellesAdjacents laberint (car posActual) (cadr posActual))))
    (llistaParetsAdjacents adjacents laberint)
  )
)

  ;Donada la llista de posiciones adjacents, crea una llista de les que són parets
(defun llistaParetsAdjacents (adjacents laberint) 

  (cond 
    ((null adjacents) nil)
    ((equal paret (getIJLaberint (car adjacents) laberint))
     (cons (car adjacents) (llistaParetsAdjacents (cdr adjacents) laberint))
    )
    (t (llistaParetsAdjacents (cdr adjacents) laberint))
  )
)



  ; la paret actual té una unica cel·la adjaçent marcada com camí?
(defun oneAdjacentVisited (currentWallPos laberint) 

  (let 
    ((adjacents 
       (casellesAdjacents laberint (car currentWallPos) (cadr currentWallPos))
     ) 
    )
    (let 
      ((valors (getValors adjacents laberint)))
      (cond 
        ((/= 1 (repetitionsX cami valors)) nil)
        (t (getCell valors adjacents))
      )
    )
  )
)
(defun getCell (valors adjacents) 
  (cond 
    ((null valors) nil)
    ((equal (car valors) cami) (car adjacents))
    (t (getCell (cdr valors) (cdr adjacents)))
  )
)
(defun prova () 

  (getParets 



    '((paret cami paret paret paret)
      (paret paret cami paret paret)
      (paret paret paret paret paret)
      (paret paret paret cami paret)
      (paret paret paret paret paret)
     )
    '(3 3)
  )
)

