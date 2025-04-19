(load 'tco)

(setq paret #\#)
(setq cami #\.)
(setq entrada #\e)
(setq sortida #\s)
(setq newline #\NewLine)

(setq mazepos '(64 320))

(setq player-speed 2)

(load 'fitxer-io)
(load 'graphfx)
(load 'user-input)
(load 'prop-util)

(setq dbg nil)

;README
; SC -> Screen Coords. Coordenades respecte la finestra
; GC -> Game Coords. Coordenades ingame
; TC -> Tile Coords. Coordenada del tile (index sobre el laberint)


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
(defun cls-player(xpos ypos maze)
    (let* ((xtile (floor xpos TILESIZE)) (ytile (floor (- ypos) TILESIZE))
            (tile (get-in-maze (get maze 'data) xtile ytile)))
        (draw-tile (get-strname tile) (+ (* xtile TILESIZE) (car mazepos) (getx maze)) (+ (* (- ytile) TILESIZE) (cadr mazepos) (gety maze)))
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

(defun-tco game-loop(name &optional (maze 'maze) (player 'player) (steps 0) (repaint t))
    (cond
    ((null (get maze 'data))
        (let* ((maze-data (read-maze name)) (start-pos (find-in-maze maze-data entrada)) (x (* (car start-pos) 16)) (y (* (cadr start-pos) -16)))
        (putprop maze (* -240 (floor x 240)) 'x)
        (putprop maze (* 240 (floor y -240)) 'y)
        (putprop maze maze-data 'data)
        (putprop player x 'x)
        (putprop player y 'y)
        (pprint (get maze 'data))
        (get-key)
        (game-loop name maze player)
        )
    )
    (t 
        ; aprofitant \n es pot ignorar la longitud
    ; al pintar per zones, es pot "ignorar" el tamany
    (let (  (pdrawx (+ (getx player) (car mazepos) (getx maze)))
            (pdrawy (+ (gety player) (cadr mazepos) (gety maze))))
    (cond
    ((or (eq repaint t))
        (cls)
        (paint-maze (get maze 'data) (+ (car mazepos) (getx maze)) (+ (cadr mazepos) (gety maze)))
    )
    )
    ;repintar tiles caminables
    ;(si el camí te un tile propi refer això a repintar davall personatge)

    
    (draw-tile "luigi" pdrawx pdrawy)
    

    (color 0 0 0 255 255 255)
    (goto-xy 0 0)
    (princ "              \n")
    (princ "        \n")
    (princ "        \n")
    (princ "        \n")
    (princ "        \n")
    (princ "        \n")
    (princ "        \n")
    (princ "        \n")
    (goto-xy 0 0)
    (print (symbol-plist player))
    (print pdrawx)
    (print pdrawy)
    (print (get-in-maze (get maze 'data) (floor (getx player) TILESIZE) (- (floor (gety player) TILESIZE))))
    (print (find-in-maze (get maze 'data) sortida))
    (print (getx maze))
    (print (gety maze))

    (let* ((input (user-input)) (px (getx player)) (py (gety player))
                                (newpx (cond    ((and (eq input 'right) (can-move-h (get maze 'data) (+ px TILESIZE) py)) (+ px player-speed))
                                                ((and (eq input 'left) (can-move-h (get maze 'data) (- px player-speed) py)) (- px player-speed))
                                                (t px))
                                )
                                (newpy (cond    ((and (eq input 'up) (can-move-v (get maze 'data) px (- py (- player-speed)))) (+ py player-speed))
                                                ((and (eq input 'down) (can-move-v (get maze 'data) px (+ py -17))) (- py player-speed))
                                                (t py))
                                )
            )
        (cond
        ((eq input 'esq)
            steps 
        )
        (t

        (cls-player (getx player) (gety player) maze)

        (let* ((r (and (eq input 'right) (> (+ newpx (getx maze)) 240))) (l (and (eq input 'left) (< (+ newpx (getx maze)) 16)))
              (u (and (eq input 'up) (> (+ newpy (gety maze)) -16))) (d (and (eq input 'down) (< (+ newpy (gety maze)) -240)))
              (newmx (cond ((eq r t) (- (getx maze) 240)) ((eq l t) (+ (getx maze) 240)) (t (getx maze))))
              (newmy (cond ((eq u t) (- (gety maze) 240)) ((eq d t) (+ (gety maze) 240))(t (gety maze))))
            )
            (game-loop name 
                            (update-prop (update-prop maze 'x newmx) 'y newmy)
                            (update-prop (update-prop player 'x newpx) 'y newpy)
                            (1+ steps)
                            (cond
                                ((or r l u d)
                                    t
                                )
                                (t 
                                    nil
                                )
                            )

            )
        )
        )
        )
    )
    )
    )
    )
)

(defun can-move-h (maze x y)
    (let ((xtile (floor x TILESIZE)) (ytile (floor (- y) TILESIZE)) (ytile2 (floor (+ (- y) 15) TILESIZE)))
        (not (or (eq (get-in-maze maze xtile ytile) paret) (eq (get-in-maze maze xtile ytile2) paret)))
    )
)

(defun can-move-v (maze x y)
    (let ((xtile (floor x TILESIZE)) (xtile2 (floor (+ x 15) TILESIZE)) (ytile (floor (- y) TILESIZE)))
        (not (or (eq (get-in-maze maze xtile ytile) paret) (eq (get-in-maze maze xtile2 ytile) paret)))
    )
)

(cls)
;si peta algo descomentar
;(trace game-loop)
;(trace update-prop)
;(trace find-in-maze)
;(trace paint-maze)

(print (game-loop "test.txt"))
(color 0 0 0 255 255 255)
;(draw-maze "test.txt" 1 1 )
;(terpri)
;(draw-tile "rickroll" 250 250)