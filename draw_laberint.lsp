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


(defun read-maze(fn)
    (llegeix fn)
)

(defun-tco paint-maze(maze x y &optional (w 0) (h 0))
    (cond 
        ((null maze)
            nil
        )
        (t 
        (let ((elem (car maze)) (xtile (+ (* w TILESIZE) x)) (ytile (+ (* h TILESIZE) y)))
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
            (paint-maze 
                ;x y w h
                (cdr maze) x y (cond ((eq elem newline) 0) (t (1+ w))) (cond ((eq elem newline) (1- h)) (t h))
            )

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
(defun cls-player(x y)
    (draw-tile "white" x y)
)


(defun-tco find-in-maze(maze casella &optional (i 0) (j 0))
    (let ((c (car maze)))
    (cond
        ((or (eq c casella) (null maze))
            (list i j)
        )
        ((eq c newline)
            (find-in-maze (cdr maze) casella 0 (1+ j))
        )
        (t
            (find-in-maze (cdr maze) casella (1+ i) j)
        )
    )
    )
)

(defun-tco get-in-maze(maze x y)
    (let ((c (car maze)))
        (cond
        ((or (< x 0) (< y 0) (null maze))
            newline
        )
        ((and (= x 0) (= y 0))
            c
        )
        ((eq c newline)
            (get-in-maze (cdr maze) x (1- y))
        )
        ((> y 0)
            (get-in-maze (cdr maze) x y)
        )
        (t
            (get-in-maze (cdr maze) (1- x) y)
        )
        )
    )
)

(defun-tco game-loop(name &optional (maze 'maze) (player 'player) (repaint t))
    (cond
    ((null (get maze 'data))
        (let* ((maze-data (reverse (read-maze name))) (start-pos (find-in-maze maze-data entrada)) (x (* (car start-pos) 16)) (y (* (cadr start-pos) -16)))
        (putprop maze (* -240 (floor x 240)) 'x)
        (putprop maze (* 240 (floor y -240)) 'y)
        (putprop maze maze-data 'data)
        (putprop player x 'x)
        (putprop player y 'y)
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
    (let* ((pos (find-in-maze (get maze 'data) entrada)) (x (+ (* (car pos) TILESIZE) (car mazepos) (getx maze)))
                                                  (y (+ (* (- (cadr pos)) TILESIZE) (cadr mazepos) (gety maze))))
        (cond
        ((and (<= (abs (- pdrawx x)) 16) (<= (abs (- pdrawy y)) 16))
            (draw-tile "start" x y)
        )
        )
    )
    (let* ((pos (find-in-maze (get maze 'data) sortida)) (x (+ (* (car pos) TILESIZE) (car mazepos) (getx maze)))
                                                  (y (+ (* (- (cadr pos)) TILESIZE) (cadr mazepos) (gety maze))))
        (cond
        ((and (<= (abs (- pdrawx x)) 16) (<= (abs (- pdrawy y)) 16))
            (draw-tile "end" x y)
        )
        )
    )
    

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
                                (newpx (cond    ((and (eq input 'right) (can-move-h (get maze 'data) (+ px 16) py)) (+ px player-speed))
                                                ((and (eq input 'left) (can-move-h (get maze 'data) (- px player-speed) py)) (- px player-speed))
                                                (t px))
                                )
                                (newpy (cond    ((and (eq input 'up) (can-move-v (get maze 'data) px (- py -2))) (+ py player-speed))
                                                ((and (eq input 'down) (can-move-v (get maze 'data) px (+ py -17))) (- py player-speed))
                                                (t py))
                                )
            )
        (cond
        ((eq input 'esq)
            nil 
        )
        (t

        (cls-player pdrawx pdrawy)

        (let* ((r (and (eq input 'right) (> (+ newpx (getx maze)) 240))) (l (and (eq input 'left) (< (+ newpx (getx maze)) 16)))
              (u (and (eq input 'up) (> (+ newpy (gety maze)) -16))) (d (and (eq input 'down) (< (+ newpy (gety maze)) -240)))
              (newmx (cond ((eq r t) (- (getx maze) 240)) ((eq l t) (+ (getx maze) 240)) (t (getx maze))))
              (newmy (cond ((eq u t) (- (gety maze) 240)) ((eq d t) (+ (gety maze) 240))(t (gety maze))))
            )
            (game-loop name 
                            (update-prop (update-prop maze 'x newmx) 'y newmy)
                            (update-prop (update-prop player 'x newpx) 'y newpy)
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

(game-loop "laberints_exemple/40x30_1.txt")
(color 0 0 0 255 255 255)
;(draw-maze "test.txt" 1 1 )
;(terpri)
;(draw-tile "rickroll" 250 250)