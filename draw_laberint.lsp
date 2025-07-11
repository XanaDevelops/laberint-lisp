;========================================================================
; Classe encarregada de la gestió visual del laberint.
; Implementa funcions per llegir el laberint des d’un fitxer, pintar-lo a la
; pantalla, esborrar posicions del jugador, gestionar la conversió de
; coordenades, i dibuixar les claus.
;========================================================================

;; =========================================
;; Funció: "read-maze"
;; Llegeix un laberint des d'un fitxer
;;
;; Paràmetres:
;;  - fn: ruta al fitxer
;;  - (opcional) maze: dades del fitxer
;;  - (opcional) r, s: acumuladors de les files procesades i al fila actual, respectivament 
;;
;; Retorn:
;;  - maze[][]
;; ==========================================
(defun-tco read-maze (fn &optional (maze nil) (r nil) (s nil))
    (cond
    ((and (null maze) (null r))
        (read-maze fn (llegeix fn))
    )
    (t 
    (let ((c (car maze)))
        (cond
        ((null c)
            (cons s r)
        )
        ((eq c newline)
            (read-maze fn (cdr maze) (cond ((null r) (cond ((null (car s)) r) (t (list s))))
                                            (t (cons s r)) )
                            nil
            )
        )
        (t
            (read-maze fn (cdr maze) r (cons c s))
        )
        )
    ))
    )
)

;; =========================================
;; Funció: "paint-maze"
;;  Dibuixa el laberint a una coordenada concreta.
;;  Amb les limitacions de draw-tile, només pinta el viewport corresponent
;;
;; Paràmetres:
;;  - maze: laberint[][]
;;  - x,y: coordenades de la pantalla a dibuixar
;;  - (opcional) w,h: indexos per dibuixar
;;  - (opcional) row: fila actual
;;
;; Retorn:
;;  - t
;; ==========================================
(defun-tco paint-maze(maze x y &optional (w 0) (h 0) (row (car maze)))
    (cond 
        ((and (null maze) (null row))
            t
        )
        
        ((null row)
            (paint-maze (cdr maze) x y 0 (1- h))

        )
        (t 
        ; pinta el tile corresponent
        (let ((elem (car row)) (xtile (+ (* w TILESIZE) x)) (ytile (+ (* h TILESIZE) y)))
            (cond
                ((eq elem newline)
                    nil
                )
                (t
                    (draw-tile (get-strname elem) xtile ytile)
                )
            )
            ;(get-key)
            (paint-maze maze x y (1+ w) h (cdr row))
        )

        )
    )
)

;; =========================================
;; Funció: "get-strname"
;; Retorna el nom del string d'un tile per a (draw-tile)
;;
;; Paràmetres:
;;  - tile: el tile
;;
;; Retorn:
;;  - string amb el nom
;; ==========================================
(defun get-strname (tile)
    (cond
        ((eq tile Centrada)
            "start"
        )
        ((eq tile Csortida)
            "end"
        )
        ((eq tile Cparet)
            "wall"
        )
        ((eq tile Ccami)
            "path2"
        )
        (t
            "error"
        )
    )
)

;; =========================================
;; Funció: "cls-player"
;; "Borra" el sprite del jugador al laberint.
;; Realment pinta per damunt els tiles del laberint sobre el que jugador hi és.
;;
;; Paràmetres:
;;  - xpos, ypos: posició del jugador
;;  - maze: dades del laberint
;;
;; Retorn:
;;  - res
;; ==========================================
(defun cls-player(xpos ypos maze)
    (let* ((xtile (floor xpos TILESIZE)) (ytile (floor (- ypos) TILESIZE))
            (tile (get-in-maze (get maze 'data) xtile ytile))
            (draw-pos (tile-to-draw xtile ytile maze)))
        ; sempre esta sobre l'origen de arredonir les coordenades
        (draw-tile (get-strname tile) (car draw-pos) (cadr draw-pos))   
    ;comprova quina casella del costat
    ;casella dreta
    (cond 
        ((> (mod xpos TILESIZE) 0)
            (let ((newtpos (tile-to-draw (1+ xtile) ytile maze)))
                (draw-tile (get-strname (get-in-maze (get maze 'data) (1+ xtile) ytile))
                (car newtpos) (cadr newtpos))
            )
        )
    )
    
    ;casella abaix
    (cond
        ((> (mod (- ypos) TILESIZE) 0)
        (let ((newtpos (tile-to-draw xtile (1+ ytile) maze)))
            (draw-tile (get-strname (get-in-maze (get maze 'data) xtile (1+ ytile)))
                (car newtpos) (cadr newtpos))
        )
        )
    )
    ;diagonal abaix
    (cond 
        ((or (> (mod (- ypos) TILESIZE) 0) (> (mod xpos TILESIZE) 0))
        (let ((newtpos (tile-to-draw (1+ xtile) (1+ ytile) maze)))
            (draw-tile (get-strname (get-in-maze (get maze 'data) (1+ xtile) (1+ ytile)))
                (car newtpos) (cadr newtpos)))
        )
    )
    )
)

;; =========================================
;; Funció: "tile-to-draw"
;;  Transforma unes coordenades de tile a coordenades de dibuix del laberint
;;
;; Paràmetres:
;;  - xt, yt: coordenades tile
;;  - maze: dades del laberint
;;
;; Retorn:
;;  - (x,y) coordenades de dibuix laberint
;; ==========================================
(defun tile-to-draw(xt yt maze)
    (list 
        (+ (* xt TILESIZE) (car mazepos) (getx maze))
        (+ (* (- yt) TILESIZE) (cadr mazepos) (gety maze))
    )
)

;; =========================================
;; Funció: "tile-to-coord"
;;  Transforma unes coordenades de tile a coordenades de joc
;;
;; Paràmetres:
;;  - xt, yt: coordenades tile
;;
;; Retorn:
;;  - (x,y) coordenades de joc
;; ==========================================
(defun tile-to-coord(xt yt)
    (list 
        (+ (* xt TILESIZE))
        (+ (* yt TILESIZE))
    )
)

;; =========================================
;; Funció: "coord-to-tile"
;;  Transforma unes coordenades de joc a unes de tile, de forma exacta (floor) o no (round)
;;
;; Paràmetres:
;;  - x, y: coordenades tile
;;  - (opcional) exact: si usar floor o round
;;
;; Retorn:
;;  - (xt, yt) coordenades de tile
;; ==========================================
(defun coord-to-tile(x y &optional (exact t))
    (mapcar (lambda (x) (apply (cond ((eq exact t) 'floor) (t 'round)) x)) (list (list x TILESIZE) (list (- y) TILESIZE)))
)

;; =========================================
;; Funció: "draw-keys"
;;  Dibuixa al laberint les claus no recollides
;;
;; Paràmetres:
;;  - key-coords: llista amb les coordenades de les claus
;;  - maze: dades del laberint
;;
;; Retorn:
;;  - t
;; ==========================================
;dibuixa les claus
(defun draw-keys(key-coords maze)
    (cond 
    ((null key-coords)
        t
    )
    (t
        (let* ((coords (car key-coords)) (xt (car coords)) (yt (cadr coords))
                (draw-coords (tile-to-draw xt yt maze)) (drawx (car draw-coords)) (drawy (cadr draw-coords))
            )
            (draw-tile "llave" drawx drawy)
            (draw-keys (cdr key-coords) maze)
        )
    )
    )
)






