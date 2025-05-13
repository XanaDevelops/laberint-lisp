(load "libs/listLib.lsp")
(load "VARSGLOBALS.lsp")
; guarda en el fixter estadistiques.txt la jugada recent

(defstruct jugador nom passos f c)

(defun equal-Jugador (j1 j2) 
  (and (equal (jugador-nom j1) (jugador-nom j2)) 
       (equal (jugador-passos j1) (jugador-passos j2))
       (equal (jugador-f j1) (jugador-f j2))
       (equal (jugador-c j1) (jugador-c j2))
  )
)

(defun prova () 

  (let 
    ((jug (make-jugador :nom "johan" :passos 7 :f 20 :c 10)))
    ; (guardarJugador jug)
    (getClassificacio jug)
  )
)
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

(defun getLlistaClassificacions (jug) 
  ; llegir el fitxer d'estadistiques
  (let* 
    ((stream (open estadistiquesFile :direction :input)) 
      (llista (getLlistaLab jug stream))
    )
    (close stream)
    llista
  )
)

(defun getLlistaLab (jug stream &optional (llista '())) 
  (let 
    ((jugadorActual (read stream nil nil)))
    (cond 
      ((null jugadorActual) llista) ; EOF
      ; mateixa dimensió del laberint
      ((and (equal (jugador-f jug) (jugador-f jugadorActual)) 
            (equal (jugador-c jug) (jugador-c jugadorActual))
       )
       (getLlistaLab jug stream (cons jugadorActual llista))
      )
      (t (getLlistaLab jug stream llista))
    )
  )
)
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
(defun getPosJugador (jug llista &optional (index 0)) 
  (cond 
    ((null llista) (- 1))
    ((equal-Jugador jug (car llista)) index)
    (t
     (getPosJugador jug (cdr llista) (+ 1 index))
    )
  )
) 

(defun ordena (l) 
  (cond 
    ((null l) nil)
    (t (cons (minim l) (ordena (remove (minim l) l :test # 'equal-Jugador))))
  )
)

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

(defun posi)