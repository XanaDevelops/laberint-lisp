(load 'CONST)
;========================================================================
; Classe usada per gestionar i dibuixar el minimapa.
; Permet mostrar, actualitzar i esborrar la posició del jugador,
; així com representar el laberint en format reduït i gestionar el
; recorregut i el camp de visió (Fog of War).
;========================================================================
;; =============================================================================
;; Funció: draw-mm-player
;;  Dibuixa al minimapa la posició del jugador.
;;
;; Paràmetres:
;;   - player: dades jugador
;;
;; Retorn:
;;   - res
;; =============================================================================
(defun draw-mm-player(player)
    (let* ((x (get player 'tilex)) (y (get player 'tiley))
            (coords (tile-to-mm-coord x y))
            )
        (draw-mm-player-intern (car coords) (cadr coords))
    )
)

;; =============================================================================
;; Funció: cls-mm-player
;;  Borra del minimapa la posició del jugador.
;;  Lo que fa es pintar-ho com a recorregut
;;
;; Paràmetres:
;;   - player: dades jugador
;;
;; Retorn:
;;   - res
;; =============================================================================
(defun cls-mm-player(player)
     (let* ((x (get player 'tilex)) (y (get player 'tiley))
            (coords (tile-to-mm-coord x y))
            )
        (draw-mm-recorregut (car coords) (cadr coords))
    )
)

;retorna nou extra
(defun update-recorregut(player extra)
    (let* ((x (get player 'tilex)) (y (get player 'tiley))
            (mmaze (get extra 'minimap))
        )
        (update-prop extra 'minimap
            (establir-I-valor y mmaze (establir-I-valor x (obtenir-element-I y mmaze) mm-recorregut)))
    )
)

;actualitza el FoW del minimapa
(defun update-minimap(maze px py mmaze &optional (dir nil))
    (cond
    ((null dir)
        (update-minimap maze px py 
            (update-minimap maze px py 
                (update-minimap maze px py 
                    (update-minimap maze px py mmaze '(0 -1)) '(0 1)) '(-1 0)) '(1 0))
        
        
        

    )
    (t 
        (let* ((tile (get-in-maze maze px py)) (mtile (get-in-maze mmaze px py)))
        (cond 
            ((eq tile Cparet)
                mmaze
            )
            (t 
                (cond 
                ((eq mtile mm-paret)
                    (draw-mm-cami (+ (car mmappos) (* px MM_TILESIZE)) (+ (cadr mmappos) (* (- py) MM_TILESIZE)))
                )
                (t t)
                )
                (update-minimap maze (+ px (car dir)) (+ py (cadr dir))
                    (cond 
                        ((eq mtile mm-recorregut) mmaze)
                        (t (establir-I-valor py mmaze (establir-I-valor px (obtenir-element-I py mmaze) mm-cami)))
                    )
                    dir)
            )
        )
        )
        )
    )
)

;genera doble array de mm-paret analog a maze
(defun-tco gen-minimap (maze &optional (row (car maze)) (mmap nil) (mrow nil) )
    (cond 
        ((and (null maze) (null row))
            (cons mrow mmap)
        )
        ((null row)
            (gen-minimap (cdr maze) (car maze) (cons mrow mmap))
        )
        (t 
            (gen-minimap maze (cdr row) mmap (cons mm-paret mrow)) 
        )
    )
)

; pinta el laberint a (x,y) SC
(defun-tco paint-minimap(maze x y &optional (w 0) (h 0) (row (car maze)))
    ;(princ maze)
    ;(get-key)
    (cond 
        ((and (null maze) (null row))
            t
        )
        
        ((null row)
            (paint-minimap (cdr maze) x y 0 (1- h))

        )
        (t 
        ; pinta el tile corresponent
        (let ((elem (car row)) (xtile (+ (* w MM_TILESIZE) x)) (ytile (+ (* h MM_TILESIZE) y)))
            (cond
                ((eq elem mm-paret)
                    (draw-mm-paret xtile ytile)
                )
                ((eq elem mm-cami)
                    (draw-mm-cami xtile ytile)
                )
                ((eq elem mm-recorregut)
                    (draw-mm-recorregut xtile ytile)
                )
                
                (t
                    nil
                )
            )
            ;(get-key)
            (paint-minimap maze x y (1+ w) h (cdr row))
        )
        )
    )
)

(defun draw-mm-paret (x y)
    (move x y)
    (color 180 180 180)
    (draw-mm-square)
)
(defun draw-mm-cami (x y)
    (move x y)
    (color 0 0 0)
    (draw-mm-square)
)
(defun draw-mm-recorregut (x y)
    (move x y)
    (color 255 255 255)
    (draw-mm-square)
)
(defun draw-mm-player-intern (x y)
    (move x y)
    (color 0 220 0)
    (draw-mm-square)
)

(defun draw-mm-square(&optional (i MM_TILESIZE))
    (cond 
    ((= i 0) t)
    (t (drawrel MM_TILESIZE 0)
        (moverel (- MM_TILESIZE) -1)
        (draw-mm-square (1- i))
    )
    )
)

(defun tile-to-mm-coord (tx ty)
    (list
        (+ (* tx MM_TILESIZE) (car mmappos)) (+ (* (- ty) MM_TILESIZE) (cadr mmappos))
    )
)