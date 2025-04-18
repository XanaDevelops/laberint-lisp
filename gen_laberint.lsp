(load "tco.lsp")
(load "libs/listLib.lsp")
(load "VARSGLOBALS.lsp")
(load "fitxer-io.lsp")


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

(defun getRandomPosition (lista) 
  (nth (random (length lista)) lista)
)

(defun prova () 
  (calcularPosSortida 
    '(0 0)
    (constructLlistaCamins 
      '((paret cami entrada cami) (cami paret paret paret) (cami cami cami cami))
    )
  )
)
  ; (setq laberint (create-matrix '3 '3 'paret))
(defun genera-laberint () 
  (let* 
    ((laberint (create-matrix MIDA MIDA paret))  ; inicialitzar el laberint amb parets
      (pos (getRandomPosition iniciEntrada))
      (laberint (setMatrixValue laberint entrada pos)) ;Establir una posició random com entrada

      (laberint (recursiveDFS pos laberint))
      (laberint 
        (setMatrixValue 
          laberint
          sortida
          (getCasellesMesLlunaya pos (constructLlistaCamins laberint))
        )
      ) ; Establir sortida
      (laberint (setEdgesParet laberint))
    ) ;Posar les parets enxternes

    (format t "posInicia = ~a~%" pos)
    (writeToFile laberint "laberintGenerat.txt")
  ) ;; Guardar el laberint en un arxiu
)


(defun getEdgesCamins (posActual laberint) 
  (let* 
    ((adjacents (casellesAdjacents laberint posActual)) 
      (values (getValors adjacents laberint))
    )
    (getCaminsAdjacents adjacents valors)
  )
)

(defun getCaminsAdjacents (adjacents valors) 

  (cond 
    ((null adjacents) nil)
    ((or (equal (car valors) cami) (equal (car valors) sortida))
     (cons (car adjacents) (getCaminsAdjacents (cdr adjacents) (cdr valors)))
    )
  )
  (t (getCaminsAdjacents (cdr adjacents) (cdr valors)))
)


(defun getCasellesMesLlunaya (posEntrada llistaCamins) 
  (let 
    ((distancies 
       (mapcar 
         (lambda (pos) 
           (+ (abs (- (car posEntrada) (car pos))) 
              (abs (- (cadr posEntrada) (cadr pos)))
           )
         )
         llistaCamins
       )
     ) 
    )
    (nth (getIndex (maxim distancies) distancies) llistaCamins)
  )
)
(defun getIndex (e llista &optional (i 0)) 
  (cond 
    ((null llista) (- 1))
    ((equal e (car llista)) i)
    (t (getIndex e (cdr llista) (+ i 1)))
  )
)

(defun constructLlistaCamins (laberint &optional (i 0) (j 0)) 
  (cond 
    ((and (= i (- mida 1)) (= j (- mida 1))) nil)
    ((equal cami (getIJLaberint (list i j) laberint))
     (cons (list i j) 
           (constructLlistaCamins 
             laberint
             (car (getNextIJ i j))
             (cadr (getNextIJ i j))
           )
     )
    )
    (t
     (constructLlistaCamins laberint (car (getNextIJ i j)) (cadr (getNextIJ i j)))
    )
  )
)

(defun getNextIJ (i j) 
  (cond 
    ((= j (- MIDA 1)) (list (+ i 1) 0))
    (t (list i (+ j 1)))
  )
)


; devuelve la posicion de una celda adyacente random
(defun getRandomAdjacent (pos laberint) 
  (let 
    ((adjacents 
       (casellesAdjacents laberint pos)
     ) 
    )

    (getElementI (random (length adjacents)) adjacents)
  )
)

(defun acabar (currentPos laberint) 
  (not 
    (some 
      (lambda (pos) (paretIUnicCami pos laberint))
      (casellesAdjacents laberint currentPos)
    )
  )
)

(defun swap (firstIndex secondIndex l) 

  (let* 
    ((firstValue (getElementI firstIndex l)) 
      (secondValue (getElementI secondIndex l))
      (newL (setIValue firstIndex l secondValue))
    )

    (setIValue secondIndex newL firstValue)
  )
)

(defun modernFisher-Yates (l &optional (lastIndex (- (length l) 1))) 
  (cond 
    ((= 0 lastIndex) l)
    (t
     (let* 
       ((randomIdx (random lastIndex)) 
         (newList (swap randomIdx lastIndex l))
       )
       (modernFisher-Yates newList (- lastIndex 1))
     )
    )
  )
)


(defun recursiveDFS (pos laberint) 
  (let 
    ((adjacents (modernFisher-Yates (casellesAdjacents laberint pos))))
    ; (format t "adjacents = ~a~%" adjacents)
    (recursiveDFSAuxiliar adjacents pos laberint)
  )
)

(defun recursiveDFSAuxiliar (adjacents pos laberint) 
  (if (null adjacents) 
    laberint
    (let* 
      ((next (car adjacents)) 
        (other (cdr adjacents))
      )
      (if (paretIUnicCami next laberint) 
        ; transformar en paret
        (let* 
          ((laberintAcual (setMatrixValue laberint cami next)) 
            (laberintDespres (recursiveDFS next laberintAcual))
          )
          ;seguir amb la resta de camins en tornar de la recursivitat
          (recursiveDFSAuxiliar other pos laberintDespres)
        )
        ; provar altres camins
        (recursiveDFSAuxiliar other pos laberint)
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
       (unicCami posActual laberint)
     )
     t
    )

    (t nil)
  )
)


;   pos = (i, j) té  un únic camí/entrada adjaçent?
(defun unicCami (pos laberint) 
  (let 
    ((adjacents 
       (casellesAdjacents laberint pos)
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

  ; Devuelve el valor de l = (i,j) del laberinto
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

(defun casellesAdjacents (matrix pos) 
  (let 
    ((newPos 
       (mapcar 
         (lambda (llista) 
           (list (+ (car pos) (car llista)) (+ (cadr pos) (cadr llista)))
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
  


(defun randomizedPrim () 

  ;Inicialitzar el laberint a 'paret
  (let* 
    ((laberint (create-matrix mida mida paret)) 
      ; casella inicial com a cami
      (pos (chooseRandomPos laberint))
      (laberint (setMatrixValue laberint cami pos))
      ; afegir els parets de la casella
      (paretsL (getLlistaAdjacentsX pos laberint paret))
      (newLaberint (recursivePrim paretsL laberint))
    )

    (writeToFile newLaberint "laberintGenerat.txt")
  )
)

(defun recursivePrim (parets laberint) 
  (if (null parets) 
    laberint
    (let* 
      ((randomWall (getRandomPosition parets)) 
       (visited (getLlistaAdjacentsX randomWall laberint cami))
      )
      (if (/= (length visited) 1) 
        ; Si no té exactamente 1 veïnat visitado, s'esborra completament de la llista de parets
        ; crida recursiva alternativa
        (recursivePrim (esborra-tot randomWall parets) laberint)

        (let* 
          ((newLaberint (setMatrixValue laberint cami randomWall)) 
            ; Obtenir les parets adjaçents de randomWall
           (newParets (getLlistaAdjacentsX randomWall newLaberint paret))
            ; Eliminar randomWall de la llista de parets, i es concatena amb newParets           
           (parets2 (append (esborra-tot randomWall parets) newParets))
          )
          (recursivePrim parets2 newLaberint)
        )
      )
    )
  )
)

; retorna una llista de les posiciones adjaçents a posActual que són X.
(defun getLlistaAdjacentsX (posActual laberint X)
(let 
    ((adjacents (casellesAdjacents laberint posActual)))
    (auxLlistaAdjacents adjacents laberint X)
  )
)


  ;Donada la llista de posiciones adjacents, crea una llista de les que són X
(defun auxLlistaAdjacents (adjacents laberint X) 

  (cond 
    ((null adjacents) nil)
    ((equal X (getIJLaberint (car adjacents) laberint))
     (cons (car adjacents) (auxLlistaAdjacents (cdr adjacents) laberint X))
    )
    (t (auxLlistaAdjacents (cdr adjacents) laberint X))
  )
)


