(load "libs/listLib.lsp")
(load "CONST.lsp")
;========================================================================
;Aquesta classe serveix per poder guardar els jugados en el fitxer
; estadistiquesFile declarat a la classe CONST.lsp, obtenir jugador 
; d'aquell mateix fitxer i calcular la classificació d'un jugador donat
;========================================================================
;; =============================================================================
;; Estructura: 'jugador'
;; Defineix una estructura per representar un jugador dins el joc.
;;
;; Camps:
;;   - nom: Nom del jugador.
;;   - passos: Nombre de passos realitzats pel jugador.
;;   - laberint: Ruta del laberint
;; =============================================================================
(defstruct jugador nom passos laberint)

;; =============================================================================
;; Funció: 'equal-Jugador'
;; Compara si dos jugadors són iguals basant-se en els seus atributs.
;; Dos jugadors són iguals si tenen els mateixos atributs(tots).
;; Paràmetres:
;;   - j1: Primer jugador.
;;   - j2: Segon jugador.
;;
;; Retorn:
;;   - T si són iguals, NIL en cas contrari.
;; =============================================================================
(defun equal-Jugador (j1 j2) 
  (and (equal (jugador-nom j1) (jugador-nom j2)) 
       (equal (jugador-passos j1) (jugador-passos j2))
       (equal (jugador-laberint j1) (jugador-laberint j2))
  )
)

;; =============================================================================
;; Funció: 'guardarJugador'
;; Guarda la informació d'un jugador, en forma d'expressió, al fitxer d'estadístiques.
;;
;; Paràmetres:
;;   - jug: Estructura del jugador a desar.
;;
;; =============================================================================
(defun guardarJugador (jug) 
  (let 
    ((stream 
       (open estadistiquesFile :direction :output :if-exists :append 
             :if-does-not-exist :create
       )
     ) 
    )

    (prin1 jug stream)
    (terpri stream)
    (close stream)
  )
)

;; =============================================================================
;; Funció: 'getLlistaClassificacions'
;; Obté la llista ordenada de classificacions per un jugador donat.
;;
;; Paràmetres:
;;   - jug: Jugador del qual es volen obtenir les estadístiques.
;;
;; Retorn:
;;   - Llista de jugadors ordenada del mateix laberint.
;; =============================================================================
(defun getLlistaClassificacions (jug) 
  ; llegir el fitxer d'estadistiques
  (let* 
    ((stream (open estadistiquesFile :direction :input)) 
      (llista (getLlistaLab jug stream))
    )
    (close stream)
    (ordena llista)
  )
)

;; =============================================================================
;; Funció: 'getLlistaLab'
;; Filtra els jugadors del fitxer d'estadístiques segons el laberint.
;;
;; Paràmetres:
;;   - jug: Jugador de referència.
;;   - stream: Flux de lectura del fitxer.
;;   - llista(opcional): Llista acumuladora.
;;
;; Retorn:
;;   - Llista de jugadors que han jugat en el mateix laberint.
;; =============================================================================
(defun getLlistaLab (jug stream &optional (llista '())) 
  (let 
    ((jugadorActual (read stream nil nil)))
    (cond 
      ((null jugadorActual) llista) ; EOF
      ; mateixa dimensió del laberint
      ((and (equal (jugador-laberint jug) (jugador-laberint jugadorActual)) 
       )
       (getLlistaLab jug stream (cons jugadorActual llista))
      )
      (t (getLlistaLab jug stream llista))
    )
  )
)

;; =============================================================================
;; Funció: 'getClassificacio'
;; Obté la classificació i el nombre total de jugadors en el mateix laberint.
;; La classficació d'un jugador s'obté sobre la llista de jugadors que han jugat
;; a un laberint de igual dimensió.
;; Paràmetres:
;;   - jug: Jugador del qual es vol obtenir la classificació.
;;
;; Retorn:
;;   - Llista amb la classificació de 'jug' i la mida de la llista de jugadors competidors.
;;      
;; =============================================================================
; retorna la parella (classificació nº de jugadors) 
(defun getClassificacio (jug) 

  (let* 
    ((llista 
       (getLlistaClassificacions jug)
     ) 
      (llistaOrdenada (ordena llista))
    )

    (list (+ 1 (getPosJugador jug llistaOrdenada)) (length llistaOrdenada))
  )
)

;; =============================================================================
;; Funció: 'getPosJugador'
;; Troba la posició d'un jugador en la classificació ordenada.
;;
;; Paràmetres:
;;   - jug: Jugador a cercar.
;;   - llista: Llista de jugadors ordenada.
;;   - index(opcional): Índex actual.
;;
;; Retorn:
;;   - Posició del jugador en la classificació, o -1 si no es troba.
;; =============================================================================
(defun getPosJugador (jug llista &optional (index 0)) 
  (cond 
    ((null llista) (- 1))
    ((equal-Jugador jug (car llista)) index)
    (t
     (getPosJugador jug (cdr llista) (+ 1 index))
    )
  )
) 

;; =============================================================================
;; Funció: 'ordena'
;; Ordena la llista de jugadors, de forma ascendent, en funció dels passos realitzats.
;;
;; Paràmetres:
;;   - l: Llista de jugadors a ordenar.
;;
;; Retorn:
;;   - Llista ordenada per nombre de passos.
;; =============================================================================
(defun ordena (l) 
  (cond 
    ((null l) nil)
    (t (cons (minim l) (ordena (remove (minim l) l :test #'equal-Jugador))))
  )
)

;; =============================================================================
;; Funció: 'minim'
;; Cerca el jugador amb menys passos de la llista.
;;
;; Paràmetres:
;;   - l: Llista de jugadors.
;;
;; Retorn:
;;   - Jugador amb menor nombre de passos.
;; =============================================================================
(defun minim (l) 
  (cond 
    ((null (cdr l)) (car l))
    (t
     (let 
       ((mincdr (minim (cdr l))))
       (if (< (jugador-passos (car l)) (jugador-passos mincdr)) 
         (car l)
         mincdr
       )
     )
    )
  )
)

(defun capar-a(llista max)
  (cond 
    ((or (= max 0) (null llista))
      nil
    )
    (t (cons (car llista) (capar-a (cdr llista) (1- max))))
  )
)

