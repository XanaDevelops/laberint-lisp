(load "tco.lsp")
(load "libs/listLib.lsp")
(load "CONST.lsp")
(load "fitxer-io.lsp")

;========================================================================
; Classe principal per a la generació dels laberints
; Implementa les funciones necessàries per crear laberints, accedir i 
; modificar posiciones concretes del laberint, a més de totes les 
; funciones auxiliars concretes de cada algorisme de generació.
; EXTRES:
; Implementació de dos mètodes de generació de laberints:
;     * Algorsime de PRIM
;     * Algorisme basat en arbre binari
; Generació de laberints de mides superiors (fins 66x66) usant la macro defun-tco
; Generació de laberints no quadrats
; Selecció millorada d'entrada i sortida:
;   * Donada una posició d'entrada, calcula la posició més llunyana 
;     i l'estableix a sortida
; 
;========================================================================
;; =============================================================================
;; Funció: 'crea-matriu'
;; Crea una matriu representada como una llista de llistes de dimensió n x m 
;; inicializada amb el valor especificat en 'valor'. 
;;
;; Paràmetres:
;;   - n: Nombre de files
;;   - m: Nombre de columnes
;;   - valor: valor per establir
;;
;; Retorn:
;;   - una matriu (llista de llistes) de dimensió N*M, i inicialitzada a 'valor'
;;
;; =============================================================================


(defun crea-matriu (n m valor) 
  (cond 
    ((= n 0) nil)
    (t
     (cons (replicar m valor) 
           (crea-matriu (- n 1) m valor)
     )
    )
  )
)

;; =============================================================================
;; Funció: 'genera'
;;
;;  Genera un laberint segons un algorisme que es tria a través de l'interfície             
;;  del joc.El valor de l'algorisme per defecte es guarda a la variable global 'def-algorisme-generacio'         
;;
;; Paràmetres:
;;   - nom-fitxer: nom del fitxer on s'ha de guardar el laberint generat
;;   - (opcional) algorisme-generacio: algorisme específic per a la generació
;;  
;; =============================================================================

(defun genera (nom-fitxer &optional (algorisme-generacio def-algorisme-generacio))

  (cond 
    ((equal DFS algorisme-generacio) (algorisme-DFS nom-fitxer))
    ((equal PRIM algorisme-generacio) (algorisme-PRIM nom-fitxer))
    (t (binary-tree nom-fitxer))
  )
)


;; =============================================================================
;; Funció: 'algorisme-DFS'
;;  
;;  Genera un laberint basant-se en l'algorsime recursiu de DFS.            
;;  Tria una posició inicial, de forma aleatoria, com a entrada i comença la seva exploració
;;  obrint noves parets sempre que es compleix una condició.    
;;
;; Paràmetres:
;;  - nom-fitxer: nom del fitxer on s'ha de guardar el laberint generat
;;
;; =============================================================================
(defun algorisme-DFS (nom-fitxer) 
  (let* 
    ;; inicialitzar el laberint amb parets
    ((laberint-inicial (crea-matriu FILES COLUMNES paret)) 

      ;; triar posició aleatoria per la casella d'entrada
      (pos (obtenir-pos-random-Llista iniciEntrada))

      ;; Establir l'entrada del laberint
      (laberint-amb-entrada (establir-valor-matriu laberint-inicial entrada pos))

      ;; remanar la llista d'adjaçents
      (adjacents 
        (modernFisher-Yates (caselles-Adjacents laberint-amb-entrada pos))
      )

      (laberint-generat (dfs-tail adjacents pos laberint-amb-entrada))
    )


    (completar-laberint laberint-generat nom-fitxer pos)
  )
)


;; =============================================================================
;; Funció: 'algorisme-PRIM'
;;
;;  Implementa l'algorisme de Prim per generar un laberint. 
;;  Inicialitza una matriu de 'parets' i construeix un camí a partir 
;;  d'una posició inicial seleccionada aleatòriament.            
;;            
;;
;; Paràmetres:
;;  - nom-fitxer: Nom del fitxer on es desarà el laberint generat.
;;
;; =============================================================================
(defun algorisme-PRIM (nom-fitxer) 

  ;Inicialitzar el laberint a 'paret
  (let* 
    ((laberint (crea-matriu FILES COLUMNES paret)) 
      ; casella inicial com a cami
      (pos (tria-pos-random laberint))
      (laberint1 (establir-valor-matriu laberint cami pos))
      ; afegir els parets de la casella
      (paretsL (obtenir-llista-adjacentsX pos laberint1 paret))
      ; crida al metòde recursiu de PRIM
      (nou-laberint (PRIM-recursiu paretsL laberint1))
    )
    (completar-laberint nou-laberint nom-fitxer)
  )
)

;; =============================================================================
;; Funció: 'completar-laberint'
;;
;;  Finalitza la construcció del laberint afegint parets externes, 
;;  establint l'entrada i la sortida, i guardant el resultat en un fitxer.
;;
;; Paràmetres:
;;   - laberint: Matriu representant el laberint
;;   - posEntr (opcional): Posició de l'entrada; si no es proporciona, es tria automàticament
;;   - nom-fitxer: Nom del fitxer on es guardarà el laberint
;;
;; =============================================================================

(defun completar-laberint (laberint nom-fitxer &optional posEntr) 
  (let* 
    ((laberint-amb-parets-externes (establir-vores-AParet laberint)) 
      (camins (construir-llista-camins laberint-amb-parets-externes))
      (camins-accessibles 
        (obtenir-Camins-Accessibles laberint-amb-parets-externes camins)
      )
      ;Establir una posició random com entrada
      (pos-entrada 
        (cond 
          ((null posEntr) (obtenir-Primir-ODarrer camins-accessibles))
          (t posEntr)
        )
      )


      (laberint-amb-entrada 
        (cond 
          ((null posEntr)
           (establir-valor-matriu laberint-amb-parets-externes entrada pos-entrada)
          )
          ; ja està establerta la posició d'entrada
          (t laberint-amb-parets-externes)
        )
      )
      ;set sortida --> posició de les més llunyanes a posEntrada
      (laberint-complet 
        (establir-valor-matriu 
          laberint-amb-entrada
          sortida
          (obtenir-casella-mes-llunyana pos-entrada camins-accessibles)
        )
      )
    )

    (escriu-fitxer laberint-complet nom-fitxer)
  )
)


;; =============================================================================
;; Funció: 'tria-pos-random'
;; Retorna una posició aleatòria dins d'una matriu donada.
;;
;; Paràmetres:
;;   - matriu: Matriu(laberint) des de la qual es seleccionarà la posició aleatòria.
;;
;; Retorn:
;;   - Una llista amb les coordenades (i, j) de la posició seleccionada.
;; =============================================================================
(defun tria-pos-random (matriu) 
  (let 
    ((i (random (length matriu))) 
      (j (random (length (car matriu))))
    )

    (list i j)
  )
)



;; =============================================================================
;; Funció: 'establir-valor-matriu'
;; Estableix un valor específic en una posició determinada dins d'una matriu.
;;
;; Paràmetres:
;;   - matriu: Matriu en la qual es modificarà el valor
;;   - valor: Valor que es vol establir en la posició especificada
;;   - pos: Coordenades (i, j) on s'ha de establir el valor
;;
;; Retorn:
;;   - Matriu modificada amb el nou valor.
;; =============================================================================

(defun establir-valor-matriu (matriu valor pos) 

  (let 
    ((matriu-modificada (establir-valor matriu (car pos) (cadr pos) valor)))
    matriu-modificada
  )
)

;; =============================================================================
;; Funció: 'establir-valor'
;; Modifica el valor d'una cel·la dins d'una matriu.
;;
;; Paràmetres:
;;   - matriu: Matriu a modificar.
;;   - i: Índex de la fila.
;;   - j: Índex de la columna.
;;   - valor: Nou valor a assignar a la posició (i, j).
;;
;; Retorn:
;;   - Matriu modificada.
;; =============================================================================
(defun establir-valor (matriu i j valor) 
  (cond 
    ; avançar en les files
    ((/= 0 i)
     (cons (car matriu) (establir-valor (cdr matriu) (- i 1) j valor))
    )
    (t (cons (canviar-valor j (car matriu) valor) (cdr matriu)))
  )
)


;; =============================================================================
;; Funció: 'canviar-valor'
;;  Canvia el valor actual d'un element d'una llista, especificat pel seu índex,  per 'valor'.
;;             
;;            
;;
;; Paràmetres:
;;  - index : index de l'element de la llista de base zero.
;;  - llista la llista a la qual s'efectuará el canvi d'un dels seus elements.
;;  - valor : el valor nou a establir a l'element a posició index.
;;
;; Retorn:
;; - Llista amb l'element modificat.
;; =============================================================================
(defun canviar-valor (index llista valor) 
  (cond 
    ((= 0 index) (cons valor (cdr llista)))
    (t (cons (car llista) (canviar-valor (- index 1) (cdr llista) valor)))
  )
)

;; =============================================================================
;; Funció: 'obtenir-pos-random-Llista'
;;  Obté un element aleatori d'una llista donada.
;;             
;; Paràmetres:
;;  - lista : llista de la qual hem de triar un element aleatori.
;;
;; Retorn:
;; - Element aleatori triat.
;; =============================================================================
(defun obtenir-pos-random-Llista (lista) 
  (nth (random (length lista)) lista)
)

;; =============================================================================
;; Funció: 'obtenir-casella-mes-llunyana'
;; Donada la posició d'entrada, aquest funció cerca una de les possibles caselles
;; llunyanyes.             
;; A partir de la llista de camins donada, l'algorisme calcula les distàncies          
;; entre cada element d'aquella llista i la posició d'entrada.
;;
;; Paràmetres:
;;  - pos-entrada: posició d'entrada del laberint.
;;  - llista-camins: llista de totes les posicions del laberint amb un valor de 'cami.  
;;
;; Retorn:
;; - Una llista (i,j) que representa una de les possibles cel·les més llunyanes
;;  de pos-entrada.
;; =============================================================================
(defun obtenir-casella-mes-llunyana (pos-entrada llista-camins) 
  (let 
    ((distancies 
       (mapcar 
         (lambda (pos) 
           (+ (abs (- (car pos-entrada) (car pos))) 
              (abs (- (cadr pos-entrada) (cadr pos)))
           )
         )
         llista-camins
       )
     ) 
    )
    ;; s'aprofita el fet que els indexs de distàncies i de llista-camins
    ;; coincideixen
    (nth (index (maxim distancies) distancies) llista-camins)
  )
)

;; =============================================================================
;; Funció: 'index'
;;  Calcula l'index d'un element d'una llista.             
;;            
;;
;; Paràmetres:
;;  - e: element que volem cercar el seu índex dins llista
;;  - llista: llista en què es fa la cerca.
;;  - i(opcional): index de l'element actual que s'està processant
;; Retorn:
;;  - L'index de l'element e. En cas que e no és un membre de llista, es retorna -1.
;; =============================================================================
(defun index (e llista &optional (i 0)) 
  (cond 
    ((null llista) (- 1))
    ((equal e (car llista)) i)
    (t (index e (cdr llista) (+ i 1)))
  )
)

;; =============================================================================
;; Funció: 'construir-llista-camins'
;;  Genera una llista de les posicions amb el valor 'cami dins d'un laberint.
;;
;; Paràmetres:
;;   - laberint: Matriu que representa el laberint.
;;
;; Retorn:
;;   - Llista de posicions (i, j) de camins del laberint.
;; =============================================================================

  
(defun-tco 
  construir-llista-camins
  (laberint &optional (i 0) (j 0) (llista-camins '()))
  (cond 
    ((or (null i) (null j))
     llista-camins
    )
    (t
     (let* 
       ((next-pos (seguent-IJ i j laberint)) 
         (rows (length laberint))
         (cols (length (car laberint)))
       )
       (cond 
         ((or (>= i rows) (>= j cols) (null next-pos))
          llista-camins
         )
         ((equal cami (pos-IJ-laberint (list i j) laberint))
          (construir-llista-camins 
            laberint
            (car next-pos)
            (cadr next-pos)
            (cons (list i j) llista-camins)
          )
         )
         (t
          (construir-llista-camins 
            laberint
            (car next-pos)
            (cadr next-pos)
            llista-camins
          )
         )
       )
     )
    )
  )
)



;; =============================================================================
;; Funció: 'seguent-IJ'
;; Donada la posició (i,j) del laberint, se cerca la següent posició 
;; seguint un patró de recorregut fila per fila. Si es troba a la darrera 
;; columna, avança a la següent fila i torna a la primera columna.
;;
;; Paràmetres:
;;  - i: número de fila actual
;;  - j: número de columna actual
;;
;; Retorn:
;;  - Llista amb la nova posició (i, j) si és vàlida.
;;  - `nil` si la posició proporcionada no és vàlida.
;; =============================================================================

(defun seguent-IJ (i j laberint) 
  (let* 
    ((rows (length laberint)) 
      (cols 
        (cond 
          ((= rows 0) 0)
          (t (length (car laberint)))
        )
      )
    )
    (cond 
      ((or (>= i rows) (>= j cols)) nil)
      ((and (= j (- cols 1)) (< i (- rows 1)))
       (list (+ i 1) 0)
      )
      ((and (= j (- cols 1)) (>= i (- rows 1))) nil)
      (t (list i (+ j 1)))
    )
  )
)



;; =============================================================================
;; Funció: 'pos-random-adjacent'
;; Donada una posició dins el laberint, selecciona aleatòriament 
;; una cel·la adjacent.
;;
;; Paràmetres:
;;  - pos: posició actual dins el laberint.
;;  - laberint: laberint del joc.
;;
;; Retorn:
;;  - Llista(i,j) amb la posició d'una cel·la adjacent seleccionada aleatòriament.
;; =============================================================================
(defun pos-random-adjacent (pos laberint) 
  (let 
    ((adjacents 
       (caselles-Adjacents laberint pos)
     ) 
    )

    (obtenir-element-I (random (length adjacents)) adjacents)
  )
)

;; =============================================================================
;; Funció: 'acabar'
;; Determina si una posició dins el laberint és un punt d'aturada.
;; Si totes les caselles adjacents són parets o tenen un únic camí, 
;; es considera que el recorregut ha finalitzat.
;;
;; Paràmetres:
;;  - currentPos: posició actual dins el laberint.
;;  - laberint: representació del laberint.
;;
;; Retorn:
;;  - `t` si no hi ha més moviments possibles.
;;  - `nil` en cas contrari.
;; =============================================================================
(defun acabar (currentPos laberint) 
  (not 
    (some 
      (lambda (pos) (paretIUnicCami pos laberint))
      (caselles-Adjacents laberint currentPos)
    )
  )
)

;; =============================================================================
;; Funció: 'intercanvia'
;; Intercanvia els elements de dues posicions en una llista.
;; 
;; Paràmetres:
;;  - primer-index: índex del primer element.
;;  - segon-index: índex del segon element.
;;  - l: Llista sobre la qual es farà l'intercanvi.
;;
;; Retorn:
;;  - Nova llista amb els elements intercanviats.
;; =============================================================================

(defun intercanvia (primer-index segon-index l) 

  (let* 
    ((primer-valor (obtenir-element-I primer-index l)) 
      (segon-valor (obtenir-element-I segon-index l))
      (nova-l (establir-I-valor primer-index l segon-valor))
    )

    (establir-I-valor segon-index nova-l primer-valor)
  )
)
;; =============================================================================
;; Funció: modernFisher-Yates
;; Implementa l'algorisme de Fisher-Yates per barrejar una llista aleatòriament.
;;
;; Paràmetres:
;;   - l: Llista que es vol barrejar.
;;   - lastIndex (opcional): Índex de l'últim element a modificar; per defecte, 
;;     es pren com la mida de la llista menys 1.
;;
;; Retorn:
;;   - Llista barrejada aleatòriament.
;;
;; Funcionament de l'algorisme:
;;   - Es selecciona un element aleatori dins de la part no barrejada de la llista.
;;   - S'intercanvia (swap) la posició aleatòria amb l'últim element de la zona no barrejada.
;;   - Es repeteix el procés fins que s'hagi barrejat tota la llista
;; =============================================================================
(defun modernFisher-Yates (l &optional (lastIndex (- (length l) 1))) 
  (cond 
    ((= 0 lastIndex) l)
    (t
     (let* 
       ((randomIdx (random lastIndex)) 
         (newList (intercanvia randomIdx lastIndex l))
       )
       (modernFisher-Yates newList (- lastIndex 1))
     )
    )
  )
)

;; =============================================================================
;; Funció: dfs-tail
;; Implementa la cerca en profunditat (DFS) per generar un laberint 
;; de manera recursiva amb optimització de tail-call.
;;
;; Paràmetres:
;;   - adjacents: Llista de posicions veïnes disponibles per expandir el camí.
;;   - pos: Posició actual en el laberint.
;;   - laberint: Matriu que representa el laberint actual.
;;
;; Retorn:
;;   - Matriu del laberint completat després de l'execució de DFS.
;;
;; Funcionament:
;;   - S'obté el següent veí disponible.
;;   - Si aquest veí és una paret amb un únic camí adjacent, es converteix en camí.
;;   - Es reorganitzen els veïns de manera aleatòria (Fisher-Yates) per evitar 
;;     un patró determinista en la generació del laberint.
;;   - Es fa una crida recursiva per continuar la construcció del camí.
;; =============================================================================

(defun-tco 
  dfs-tail
  (adjacents pos laberint)
  (cond 
    ;Si no hi ha moviment adjaçent possible, s'atura
    ((null adjacents)
     laberint
    )

    (t
     (let* 
       ((next (car adjacents))  ; triar veïnat adjaçent aleatori
         (other (cdr adjacents)) ;  la resta de veïnats adjaçents
       )
       (cond 
         ((paretIUnicCami next laberint) ; és casella paret i té un únic camí adjaçent?
          (let* 
            ((nou-laberint (establir-valor-matriu laberint cami next)) 
              ;; remanam els adjaçents de NEXT
              (parets2 
                (modernFisher-Yates 
                  (caselles-Adjacents nou-laberint next)
                )
              )
              ; Afegir els veïnats adjaçents de next a la llista

              (newParets (append parets2 other))
            )
            ; crida recursiva per la casella triada
            (dfs-tail newParets next nou-laberint)
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



;; =============================================================================
;; Funció: 'paretIUnicCami'
;; Comprova si una casella compleix dues condicions:
;; 1. El seu valor actual és una paret.
;; 2. L'únic camí adjacent és la posició actual.
;;
;; Paràmetres:
;;  - posActual: posició actual dins del laberint.
;;  - laberint: representació del laberint.
;;
;; Retorn:
;;  - `t` si es compleixen ambdues condicions.
;;  - `nil` en cas contrari.
;; =============================================================================
(defun paretIUnicCami (posActual laberint) 

  (cond 
    ((and 
       (equal paret (pos-IJ-laberint posActual laberint))
       (unic-cami posActual laberint)
     )
     t
    )

    (t nil)
  )
)


;; =============================================================================
;; Funció: 'unicCami'
;; Determina si la posició donada té exactament un camí o entrada adjacent.
;;
;; Paràmetres:
;;  - pos: posició a analitzar.
;;  - laberint: laberint del joc.
;;
;; Retorn:
;;  - `t` si té un únic camí/entrada adjacent.
;;  - `nil` en cas contrari.
;; =============================================================================
(defun unic-cami (pos laberint) 
  (let 
    ((adjacents 
       (caselles-Adjacents laberint pos)
     ) 
    )
    (cond 
      ((= 1 
          (+ (repetitionsX cami (obte-valors adjacents laberint)) 
             (repetitionsX entrada (obte-valors adjacents laberint))
          )
       )
       t
      )
      (t nil)
    )
  )
)
;; =============================================================================
;; Funció: 'getValors'
;; Dada una llista de indexos (i, j) crea una lista dels seus corresponents
;; valors del laberint
;;
;; Paràmetres:
;;  - pos: Llista de posicions (i, j).
;;  - laberint: representació del laberint.
;;
;; Retorn:
;;  - Llista de valors associats a cada posició dins del laberint.
;; =============================================================================
  
(defun obte-valors (pos laberint) 
  (cond 
    ((null pos) nil)
    (t
     (cons (pos-IJ-laberint (car pos) laberint) (obte-valors (cdr pos) laberint))
    )
  )
)

; =============================================================================
;; Funció: 'pos-IJ-laberint'
;; Retorna el valor d'una posició (i,j) dins del laberint.
;;
;; Paràmetres:
;;  - l: posició (i, j).
;;  - laberint: representació del laberint.
;;
;; Retorn:
;;  - Valor de la posició (i, j) dins del laberint.
;; =============================================================================
(defun pos-IJ-laberint (l laberint) 
  (let 
    ((i (car l)) 
      (j (cadr l))
    )
    (cond 
      ; mentre que no hem trobat la fila
      ((/= 0 i)
       (pos-IJ-laberint (list (- i 1) j) (cdr laberint))
      )
      ; mateixa fila i
      (t (obtenir-element-I j (car laberint)))
    )
  )
)
;; =============================================================================
;; Funció: 'caselles-Adjacents'
;; Retorna la llista de posicions adjacents a una posició donada dins una matriu.
;; Desplacaments es una variable global.
;; Paràmetres:
;;  - matrix: matriu representant el laberint.
;;  - pos: Posició actual (i, j).
;;
;; Retorn:
;;  - Llista de posicions adjacents vàlides.
;; =============================================================================
(defun caselles-Adjacents (matrix pos ) 
  (let 
    ((pos-nova 
       (mapcar 
         (lambda (llista) 
           (list (+ (car pos) (car llista)) (+ (cadr pos) (cadr llista)))
         )
         displacements
       )
     ) 
    )
    ; crear una llista de posiciones adjaçents vàlides
    (let 
      ((posicions-valides (elimina-elemns-fora-rang pos-nova matrix)))
      posicions-valides
    )
  )
)


;; =============================================================================
;; Funció: 'eliminar-Elemens-ForaRang'
;; Filtra les posicions `(x,y)` que estan fora dels límits del laberint.
;;
;; Paràmetres:
;;  - llista: llista de posicions.
;;  - matriu: matriu representant el laberint.
;;
;; Retorn:
;;  - Llista de posicions vàlides de llista dins del laberint.
;; =============================================================================
(defun elimina-elemns-fora-rang (llista matriu) 
  (let 
    ((f (length matriu)) (c (length (car matriu))))
    (cond 
      ((null llista) nil)
      ((and (< (caar llista) f) 
            (< (cadar llista) c)
            (>= (caar llista) 0)
            (>= (cadar llista) 0)
       )
       (cons 
         (car llista)
         (elimina-elemns-fora-rang (cdr llista) matriu)
       )
      )
      (t (elimina-elemns-fora-rang (cdr llista) matriu))
    )
  )
)
; =============================================================================
;; Funció: 'establir-vores-AParet'
;; Afegeix la fila de parets superior al laberint
;;
;; Paràmetres:
;;  - laberint: Representació del laberint.
;;
;; Retorn:
;;  - Laberint amb un límit superior de parets afegit.
;; =============================================================================
(defun establir-vores-AParet (laberint) 
  (cons (replicar (+ 2 COLUMNES) paret) (afegir-paret-AFila laberint))
)

;; =============================================================================
;; Funció: 'afegir-paret-AFila'
;; Afegeix parets als extrems de cada fila i una fila de parets 
;; al final del laberint.
;;
;; Paràmetres:
;;  - laberint: representació del laberint.
;;
;; Retorn:
;;  - Laberint amb parets als extrems de cada fila i una fila final de parets.
;; =============================================================================
(defun afegir-paret-AFila (laberint) 
  (cond 
    ((null laberint) (list (replicar (+ 2 COLUMNES) paret)))
    (t
     (cons 
       (cons paret (snoc paret (car laberint)))
       (afegir-paret-AFila (cdr laberint))
     )
    )
  )
)


;; =============================================================================
;; Funció: 'PRIM-recursiu'
;; Implementa l'algorisme de Prim per generar un laberint de manera recursiva.
;;
;; Paràmetres:
;;   - parets: Llista de posicions de parets disponibles. 
;;   - laberint: Matriu que representa el laberint actual.
;;
;; Retorn:
;;   - Laberint completat després de l'execució de l'algorisme de Prim.
;;
;; Funcionament:
;;   - Es tria aleatòriament una paret de la llista de parets.
;;   - Es verifica si té exactament un veïnat que ja forma part del camí.
;;   - Si es compleix la condició, la paret es transforma en camí i s'afegeixen 
;;     les seves parets adjacents a la llista de parets candidates.
;;   - En cas contrari, es crida a l'algorisme amb la llista de parets de la 
;;        qual s'ha eliminat la 'paret-aleatoria'
;;   - Es fa una crida recursiva per continuar expandint el laberint.
;; =============================================================================

(defun-tco 
  PRIM-recursiu
  (parets laberint)
  (cond 
    ((null parets)
     laberint
    )
    (t
     (let* 
       ((paret-aleatoria (obtenir-pos-random-Llista parets)) 
         (visitats (obtenir-llista-adjacentsX paret-aleatoria laberint cami))
       )
       (cond 
         ((/= (length visitats) 1)
          ; Si no té exactamente 1 veïnat visitat, s'esborra completament de la llista de parets
          ; crida recursiva alternativa
          (PRIM-recursiu (esborra-tot paret-aleatoria parets) laberint)
         )

         (t
          (let* 
            ((nou-laberint (establir-valor-matriu laberint cami paret-aleatoria)) 
              ; Obtenir les parets adjaçents de paret-aleatoria
              (noves-parets 
                (obtenir-llista-adjacentsX paret-aleatoria nou-laberint paret)
              )
              ; Eliminar paret-aleatoria de la llista de parets, i s'afegeixen les noves parets a la
              ; llista
              (parets2 (append (esborra-tot paret-aleatoria parets) noves-parets))
            )
            (PRIM-recursiu parets2 nou-laberint)
          )
         )
       )
     )
    )
  )
)

;; =============================================================================
;; Funció: 'obtenir-llista-adjacentsX'
;; Retorna una llista de les posicions adjacents a `posActual` que tenen el valor `X`.
;;
;; Paràmetres:
;;  - posActual: Posició de referència dins el laberint.
;;  - laberint: Representació del laberint.
;;  - X: Valor que han de tenir les posicions adjacents seleccionades.
;;
;; Retorn:
;;  - Llista de posicions adjacents que coincideixen amb `X`.
;; =============================================================================
(defun obtenir-llista-adjacentsX (posActual laberint X) 
  (let 
    ((adjacents (caselles-Adjacents laberint posActual)))
    (llista-adjacents-Auxiliar adjacents laberint X)
  )
)


;; =============================================================================
;; Funció: 'llista-adjacents-Auxiliar'
;; Filtra una llista de posicions adjacents, seleccionant les que tenen el valor `X`.
;;
;; Paràmetres:
;;  - adjacents: Llista de posicions adjacents.
;;  - laberint: Representació del laberint.
;;  - X: Valor de les caselles adjacents que es volen obtenir.
;;
;; Retorn:
;;  - Llista amb les posicions adjacents que tenen el valor `X`.
;; =============================================================================
(defun llista-adjacents-Auxiliar (adjacents laberint X) 

  (cond 
    ((null adjacents) nil)
    ((equal X (pos-IJ-laberint (car adjacents) laberint))
     (cons (car adjacents) (llista-adjacents-Auxiliar (cdr adjacents) laberint X))
    )
    (t (llista-adjacents-Auxiliar (cdr adjacents) laberint X))
  )
)

; =============================================================================
;; Funció: 'obtenir-Primir-ODarrer'
;; Retorna aleatòriament el primer o l'últim element d'una llista.
;;
;; Paràmetres:
;;  - llista: Llista d'elements.
;;
;; Retorn:
;;  - Primer o últim element de la llista, escollit aleatòriament.
;; =============================================================================
(defun obtenir-Primir-ODarrer (llista) 

  (let 
    ((ran (random 2)))

    (cond 
      ((= ran 1) (car llista))
      (t (car (last llista)))
    )
  )
)
;; =============================================================================
;; Funció: 'obtenir-Camins-Accessibles'
;; Filtra la llista de camins accessibles dins el laberint.
;;
;; Paràmetres:
;;  - laberint: Representació del laberint.
;;  - llista-camins: Llista de camins possibles.
;;  - i: Índex actual (per defecte 0).
;;
;; Retorn:
;;  - Llista amb els camins accessibles (amb almenys dos camins veïns).
;; =============================================================================
(defun-tco 
  obtenir-Camins-Accessibles
  (laberint llista-camins &optional (i 0) (camins-accessibles '()))
  (cond 
    ((>= i (length llista-camins)) camins-accessibles)
    (t
     (let* 
       ((cami-actual (nth i llista-camins)) 
         (caselles-Adjacents (caselles-Adjacents laberint cami-actual))
       )
       (cond 
         ((>= (camins-veinats caselles-Adjacents llista-camins) 2)
          (let 
            ((camins-accessibles-actualitzada 
               (cons cami-actual camins-accessibles)
             ) 
            )
            (obtenir-Camins-Accessibles 
              laberint
              llista-camins
              (+ i 1)
              camins-accessibles-actualitzada
            )
          )
          ; (cons cami-actual
          ;       (obtenir-Camins-Accessibles laberint llista-camins (+ i 1))
          ; )
         )

         (t
          (obtenir-Camins-Accessibles 
            laberint
            llista-camins
            (+ i 1)
            camins-accessibles
          )
         )
       )
     )
    )
  )
)

;; =============================================================================
;; Funció: 'camins-veinats'
;; Compta quants veïns en `veinats` estan presents en `llistaCamins`.
;;
;; Paràmetres:
;;  - veinats: Llista de posicions veïnes.
;;  - llistaCamins: Llista de camins disponibles dins el laberint.
;;
;; Retorn:
;;  - Nombre de veïns que són camins.
;; =============================================================================
(defun camins-veinats (veinats llistaCamins) 
  (cond 
    ((null veinats) 0)
    ;; Comprova si el veí és un camí.
    ((memberL (car veinats) llistaCamins)
     (+ 1 (camins-veinats (cdr veinats) llistaCamins))
    )
    (t (camins-veinats (cdr veinats) llistaCamins))
  )
)

(defun pos-random-laberint (lab) 
  (let 
    ((f (length lab)) 
      (c (length (car lab)))
    )
    (list (random f) (random c))
  )
)

;; =============================================================================
;; Funció: 'binary-tree'
;; Genera un laberint utilitzant una variant de l'algorisme de l'arbre binari.
;; L'algorisme recorre el laberint de dalt a baix i de dreta a esquerra.
;; En cada posició, es mou aleatòriament a una casella adjacent entre les quatre
;; direccions possibles (sempre que estiguin disponibles).
;; Les caselles recorregudes s'estableixen com a camí, mentre que la resta
;; conserven el seu valor inicial: paret.
;; Donat aquest enfocament, la posició d'entrada i la de sortida són fixes.
;;
;; Paràmetres:
;;  - nom-fitxer: fitxer on es desarà el laberint generat.
;; =============================================================================

(defun binary-tree (nom-fitxer) 
  (let* 
    ((laberint (crea-matriu FILES COLUMNES paret)) 
      (pos-inici '(0 0))
      (pos-final (list (- FILES 1) (- COLUMNES 1)))
      (laberint-complet (binary-tree-aux pos-inici pos-final laberint))
    )
    (completar-laberint laberint-complet nom-fitxer)
  )
)

;;=============================================================================
;; Funció: 'binary-tree-aux'
;; Mètode que implementa l'algorisme recursiu de l'arbre binari.
;; Mentre la posició actual (pos-inici) no sigui igual a la posició final (pos-fi),
;; recorre el laberint i estableix les caselles que visita com a camí.
;;
;; Paràmetres:
;;  - n: valor mínim.
;;  - m: valor màxim.
;;  - pos-inici: posició actual dins del laberint
;;  - pos-fi: posició final del laberint
;;  - lab: laberint actual
;;
;; Retorn:
;;  - Nombre aleatori dins el rang (n, m).
;;  - el laberint generat 
;; =============================================================================

(defun-tco binary-tree-aux (pos-inici pos-fi lab) 
  (cond 
    ((equal pos-inici pos-fi) lab)
    (t
     (let* 
       ((caselles-adjacents (caselles-Adjacents lab pos-inici )) 
         (next (nth (random (length caselles-adjacents)) caselles-adjacents))
         (lab-actualitzat (establir-valor-matriu lab cami next))
         (seguent (seguent-IJ (car pos-inici) (cadr pos-inici) lab-actualitzat)) 
       )
       (binary-tree-aux seguent pos-fi lab-actualitzat)
     )
    )
  )
)

