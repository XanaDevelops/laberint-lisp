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


(defun genera-laberint () 
  (cond 
    ((equal DFS algorismeGeneracio) (algoritmeDFS))
    ((equal PRIM algorismeGeneracio) (randomizedPrim))
    (t (divisioRecursiva)) ; extra, encara no s'ha implementat
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

(defun algoritmeDFS () 
  (let* 
    ((laberint (create-matrix FILES COLUMNES paret))  ; inicialitzar el laberint amb parets
      (pos (getRandomPosition iniciEntrada))
      (laberint (setMatrixValue laberint entrada pos)) ;Establir una posició random com entrada

      ; (laberint (recursiveDFS pos laberint))
      (adjacents  (modernFisher-Yates (casellesAdjacents laberint pos)))
      (laberint (dfs-tail adjacents pos laberint))
        
      
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
    (writeToFile laberint OutputFileName)
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
    ((or (equal i nil) (equal j nil)) nil)
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
    ; darrera columna i es pot avançar a la següent fila
    ((and (= j (- COLUMNES 1)) (< i (- FILES 1))) (list (+ i 1) 0))
    ; columna no final
    ((< j (- COLUMNES 1)) (list i (+ j 1)))

    (t nil) ; posició erronea
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


; (defun-tco 
;   recursiveDFS
;   (pos laberint)
;   (let 
;     ((adjacents (modernFisher-Yates (casellesAdjacents laberint pos))))
;     ; (format t "adjacents = ~a~%" adjacents)
;     (funcall 'recursiveDFSAuxiliar adjacents pos laberint)
;   )
; )

; (defun-tco  recursiveDFSAuxiliar (adjacents pos laberint)
;   (cond 
;     ((null adjacents) laberint)

;     ((paretIUnicCami (car adjacents) laberint)
;      ; transformar en paret
;      (let* 

;        ((laberintAcual (setMatrixValue laberint cami (car adjacents))) 
;          (laberintDespres (funcall 'recursiveDFS (car adjacents) laberintAcual))
;        )
     
;      ;seguir amb la resta de camins en tornar de la recursivitat
;      (recursiveDFSAuxiliar (cdr adjacents) pos laberintDespres)
;      )
;     )
  
;   ; provar altres camins
;   (t (recursiveDFSAuxiliar (cdr adjacents) pos laberint)))
; )

    
  


(defun-tco dfs-tail
  (adjacents pos laberint)
  (cond 
    ;Si no hi ha moviment adjaçent possible, tornem
    ((null adjacents)
     laberint
    )

    (t
     (let* 
       ((next (car adjacents)) ; triar veïnat adjaçent aleatori
         (other (cdr adjacents)) ;  resta de veïnats adjaçents
       )
       (cond  
         ((paretIUnicCami next laberint) ; és casella paret i té un únic camí adjaçent?
          (let* 
            ((newLaberint (setMatrixValue laberint cami next)) 
              ;; remanam els adjaçents de NEXT
              (parets2 (modernFisher-Yates 
                  (casellesAdjacents newLaberint next)
                )
              )
              ; Afegir els veïnats adjaçents de next a la llista 
              
              (newParets (append parets2 other))
            )
            ; crida recursiva per la casella triada
            (dfs-tail newParets next newLaberint)
          )
         )

        ; Provem amb la resta de veïnats
         (t
          (dfs-tail other pos laberint)
         )
       )
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
  (cons (replicate (+ 2 COLUMNES) paret) (addParetToFila laberint))
)

; añade paret a los extremos de cada fila, y la fila final de paret
(defun addParetToFila (laberint) 
  (cond 
    ((null laberint) (list (replicate (+ 2 COLUMNES) paret)))
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
    ((laberint (create-matrix FILES COLUMNES paret)) 
      ; casella inicial com a cami
      (pos (chooseRandomPos laberint))
      (laberint (setMatrixValue laberint cami pos))
      ; afegir els parets de la casella
      (paretsL (getLlistaAdjacentsX pos laberint paret))
      (newLaberint (recursivePrim paretsL laberint))
      (finalLaberint (setEdgesParet newLaberint))
    )

    ;set entrada i sortida

    (writeToFile finalLaberint OutputFileName)
  )
)

(defun-tco 
  recursivePrim
  (parets laberint)
  (cond 
    ((null parets)
     laberint
    )
    (t
     (let* 
       ((randomWall (getRandomPosition parets)) 
         (visited (getLlistaAdjacentsX randomWall laberint cami))
       )
       (cond 
         ((/= (length visited) 1)
          ; Si no té exactamente 1 veïnat visitado, s'esborra completament de la llista de parets
          ; crida recursiva alternativa
          (recursivePrim (esborra-tot randomWall parets) laberint)
         )

         (t
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


; tercer algorisme de la generació d'un laberint: DIVISIÓ RECURSIVA 

; n files, m columnes
(defun triaOrientacio (cols filas) 

  (cond 
    ((< cols filas) t)
    ((> cols filas) nil)
    (t (zerop (random 2))) ; aleatori: 0 --> T, 1 --> NIL
  )
)  


(defun divisioRecursiva () 
  (let* 
    ((laberint (create-matrix FILES COLUMNES paret)) 
      (orient-inicial (triaOrientacio COLUMNES FILES))
      (newLaberint 
        (divide-recursiu laberint FILES COLUMNES orient-inicial 0 0)
      )
      (laberintAmbParetsExteriors (setEdgesParet newLaberint))
    )
    (writeToFile laberintAmbParetsExteriors OutputFileName)
  )
)


(defun-tco 
  divide-recursiu
  (laberint filas cols orient posX posY)
  (cond 
    ((or (< cols 2) (< filas 2))
     laberint
    )

    (t
     (let* 
       (
        ;Iniciam les variables segons l'orientació actual de la subregió
        (horizontal orient)

        (wx 
          (cond 
            (horizontal posX)
            (t (+ posX (random (1- cols))))
          )
        )
        (wy 
          (cond 
            (horizontal (+ posY (random (1- filas))))
            (t posY)
          )
        )
        (px 
          (cond 
            (horizontal (+ wx (random cols)))
            (t wx)
          )
        )
        (py 
          (cond 
            (horizontal wy)
            (t (+ wy (random filas)))
          )
        )
        (dx 
          (cond 
            (horizontal 1)
            (t 0)
          )
        )
        (dy 
          (cond 
            (horizontal 0)
            (t 1)
          )
        )
        (longitud 
          (cond 
            (horizontal cols)
            (t filas)
          )
        )

        ; Dibuixarem les partes de
        (lab (draw-wall laberint wx wy px py dx dy longitud))

        ;; Subregió 1
        (f1 
          (cond 
            (horizontal (+ (- wy posY) 1))
            (t filas)
          )
        )
        (c1 
          (cond 
            (horizontal cols)
            (t (+ (- wx posX) 1))
          )
        )
        (x1 posX)
        (y1 posY)

        ;; Subregió 2
        (f2 
          (cond 
            (horizontal (- filas f1))
            (t filas)
          )
        )
        (c2 
          (cond 
            (horizontal cols)
            (t (- cols c1))
          )
        )
        (x2 
          (cond 
            (horizontal posX)
            (t (+ posX c1))
          )
        )
        (y2 
          (cond 
            (horizontal (+ posY f1))
            (t posY)
          )
        )

        (lab1 (funcall 'divide-recursiu lab f1 c1 (triaOrientacio c1 f1) x1 y1))
       )
       (divide-recursiu lab1 f2 c2 (triaOrientacio c2 f2) x2 y2)
     )
    )
  )
)

; inici de la linea (wx, wy)
; posicio del cami (px, py)
; fi de la linea (dx, dy)
; longitud: nombre de caselles a omplir
(defun draw-wall (laberint wx wy px py dx dy longitud &optional (i 0)) 
  (cond 
    ((= i longitud) laberint)
    (t
     (let* 
       ((cx (+ wx (* i dx))) 
         (cy (+ wy (* i dy)))
         (newLaberint 
           (cond 
             ((and (= cx px) (= cy py)) laberint)
             (t (setMatrixValue laberint cami (list cx cy)))
           )
         )
       )
       (draw-wall newLaberint wx wy px py dx dy longitud (+ i 1))
     )
    )
  )
)
