;retorna t si ha guanyat, nil si no
(defun check-win (maze player extra)
    (let* ((xpos (getx player)) (ypos (* -1 (gety player))) (xsor (* (car (get maze 'sortida)) TILESIZE)) (ysor (* (cadr (get maze 'sortida)) TILESIZE))
            (nkeys (length (get extra 'keys)))
            )
        (cond
            ((and (< (+ (abs (- xpos xsor)) (abs (- ypos ysor)) ) (/ TILESIZE 2)) (eq nkeys 0))
                t
            )
            (t 
                nil
            )
        )
    )
)

; -- MAZE --

; cerca la primera occurencia de casella sobre maze[][]
; retorna (i,j) indexos sobre maze[][]
(defun-tco find-in-maze(maze casella &optional (i 0) (j 0) (aux nil))
    (let ((c (car maze)))
    (cond
        ((or (eq c casella) (and (null maze) (null aux)))
            (list i j)
        )
        ((null maze)
            (find-in-maze (car aux) casella 0 (1+ j) (cdr aux))
        )
        ((null aux)
            (find-in-maze (car maze) casella 0 0 (cdr maze))
        )
        (t
            (find-in-maze (cdr maze) casella (1+ i) j aux)
        )
    )
    )
)

; retorna la casella en maze[y][x]
;                                          pots fer això
(defun-tco get-in-maze(maze x y &optional (row (car maze)))
    (cond 
        ((and (= x 0) (= y 0))
            ; retornar valor
            (car row)
        )
        ((> y 0)
            (get-in-maze (cdr maze) x (1- y))
        )
        ((> x 0)
            (get-in-maze maze (1- x) y (cdr row))
        )
    )
)

; -- GENS ---

;genera les posicions de les claus
;retorna :((x1, y1), (x2, y2), ...)
(defun gen-keys(maze &optional (n 0) (keys '()))
    ;ja es un suplici un laberint de 50x50, un de 100x100...
    (let* ((x (random 100)) (y (random 100))
            (tile (get-in-maze maze x y))
        )
        (cond 
            ((eq tile cami)
                (cond 
                    ((< n key_per_maze)
                        (gen-keys maze (1+ n) (cons (list x y) keys))
                    )
                    (t
                        keys
                    )
                )
            )
            (t 
                (gen-keys maze n keys)
            )
        )
    )
)

; ------ UPDATES ----------

;Actualitza les claus
; retorna 'extra acutalitzat
(defun update-keys(player extra &optional (remain-keys (get extra 'keys)) (aux-keys '()))
    (cond 
        ((null remain-keys)
            (update-prop extra 'keys aux-keys)
        )
        (t
            (let* ((key (car remain-keys)) (kx (car key)) (ky (cadr key))
                    (pt (coord-to-tile (getx player) (gety player) nil)) (px (car pt)) (py (cadr pt))
                )
                (cond 
                    ((and (eq px kx) (eq py ky))
                        (update-keys player extra (cdr remain-keys) aux-keys)
                    )
                    (t 
                        (update-keys player extra (cdr remain-keys) (cons key aux-keys))
                    )
                )
            )
        )
    )
)

;Calcula si s'ha d'augmentar els steps
;retorna (newTileX, newTileY 0/1)
(defun update-steps (player)
    (let* ((px (getx player)) (py (gety player))
            (xtile (round px TILESIZE)) (ytile (round (- py) TILESIZE))
            (oxtile (get player 'tilex)) (oytile (get player 'tiley))
          )
        (list xtile ytile (cond ((and (eq xtile oxtile) (eq ytile oytile)) 0) (t 1)))
    )
)


(defun can-move-h (maze x y)
    (let ((xtile (floor x TILESIZE)) (ytile (floor (- y) TILESIZE)) (ytile2 (floor (+ (- y) (1- TILESIZE)) TILESIZE)))
        (not (or (eq (get-in-maze maze xtile ytile) paret) (eq (get-in-maze maze xtile ytile2) paret)))
    )
)

(defun can-move-v (maze x y)
    (let ((xtile (floor x TILESIZE)) (xtile2 (floor (+ x (1- TILESIZE)) TILESIZE)) (ytile (floor (- y) TILESIZE)))
        (not (or (eq (get-in-maze maze xtile ytile) paret) (eq (get-in-maze maze xtile2 ytile) paret)))
    )
)

; Calcula la nova posició del jugador
; retorna (newx, newy)
(defun new-player-pos (player maze input)
    (let* ((px (getx player)) (py (gety player)) (ps (get player 'speed)))
        (list
        ;new x 
        (cond ((and (eq input 'right) (can-move-h (get maze 'data) (+ px TILESIZE) py)) (+ px ps))
              ((and (eq input 'left) (can-move-h (get maze 'data) (- px ps) py)) (- px ps))
              (t px))
        ;new Y
        (cond ((and (eq input 'up) (can-move-v (get maze 'data) px (- py (- ps)))) (+ py ps))
              ((and (eq input 'down) (can-move-v (get maze 'data) px (+ py (- (1+ TILESIZE))))) (- py ps))
              (t py))
        )
    )
)


; calcula la nova posició del laberint
; retorna (newmx newmy repaint)
(defun new-maze-pos(newpx newpy maze input)
(let* 
    (
        (r (and (eq input 'right) (> (+ newpx (getx maze)) SCREENPIXEL-M1))) (l (and (eq input 'left) (< (+ newpx (getx maze)) TILESIZE)))
        (u (and (eq input 'up) (> (+ newpy (gety maze)) (- TILESIZE)))) (d (and (eq input 'down) (< (+ newpy (gety maze)) (- SCREENPIXEL-M1))))
    )
    (list (cond ((eq r t) (- (getx maze) SCREENPIXEL-M1)) ((eq l t) (+ (getx maze) SCREENPIXEL-M1)) (t (getx maze)))
          (cond ((eq u t) (- (gety maze) SCREENPIXEL-M1)) ((eq d t) (+ (gety maze) SCREENPIXEL-M1))(t (gety maze)))
          (cond ((or r l u d) t) (t nil))
    )
)
)