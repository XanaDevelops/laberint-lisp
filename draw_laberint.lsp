;README
; SC -> Screen Coords. Coordenades respecte la finestra
; GC -> Game Coords. Coordenades ingame
; TC -> Tile Coords. Coordenada del tile (index sobre el laberint)

; llegeix el laberint del fitxer i crea un doble array
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

; pinta el laberint a (x,y) SC
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
                ((eq elem paret)
                    (paint-paret xtile ytile)
                )
                ((eq elem cami)
                    (paint-cami xtile ytile)
                )
                ((eq elem entrada)
                    (draw-tile "start" xtile ytile)
                )
                ((eq elem sortida)
                    (draw-tile "end" xtile ytile)
                )
                ((eq elem newline)
                    nil
                )
                (t
                    (paint-unk xtile ytile)
                )
            )
            ;(get-key)
            (paint-maze maze x y (1+ w) h (cdr row))
        )

        )
    )
)

(defun paint-paret(x y)
    (draw-tile "wall" x y)
)
(defun paint-cami(x y)
    (draw-tile "path2" x y)
)
(defun paint-unk(x y)
    (draw-tile "error" x y)
)

; TODO: canviar a recorrer rec un array
(defun get-strname (tile)
    (cond
        ((eq tile entrada)
            "start"
        )
        ((eq tile sortida)
            "end"
        )
        ((eq tile paret)
            "wall"
        )
        ((eq tile cami)
            "path2"
        )
        (t
            "error"
        )
    )
)
; borra el tile del jugador de forma optima, mirant quines caselles hi és sobre
(defun cls-player(xpos ypos maze)
    (let* ((xtile (floor xpos TILESIZE)) (ytile (floor (- ypos) TILESIZE))
            (tile (get-in-maze (get maze 'data) xtile ytile)))
        ; sempre esta sobre l'origen de arredonir les coordenades
        (draw-tile (get-strname tile) (+ (* xtile TILESIZE) (car mazepos) (getx maze)) (+ (* (- ytile) TILESIZE) (cadr mazepos) (gety maze)))
    ;comprova quina casella del costat
    (cond 
        ((> (mod xpos TILESIZE) 0)
            (draw-tile (get-strname (get-in-maze (get maze 'data) (1+ xtile) ytile))
                (+ (* (1+ xtile) TILESIZE) (car mazepos) (getx maze)) (+ (* (- ytile) TILESIZE) (cadr mazepos) (gety maze)))
        )
        ((> (mod (- ypos) TILESIZE) 0)
            (draw-tile (get-strname (get-in-maze (get maze 'data) xtile (1+ ytile)))
                (+ (* xtile TILESIZE) (car mazepos) (getx maze)) (+ (* (- (1+ ytile)) TILESIZE) (cadr mazepos) (gety maze)))
        )
    )
    )
)

(defun tile-to-draw(xt yt maze)
    (list 
        (+ (* xt TILESIZE) (car mazepos) (getx maze))
        (+ (* (- yt) TILESIZE) (cadr mazepos) (gety maze))
    )
)

(defun tile-to-coord(xt yt)
    (list 
        (+ (* xt TILESIZE))
        (+ (* yt TILESIZE))
    )
)

(defun coord-to-tile(x y &optional (exact t))
    (mapcar (lambda (x) (apply (cond ((eq exact t) 'floor) (t 'round)) x)) (list (list x TILESIZE) (list (- y) TILESIZE)))
)

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






