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
    ;(princ " ")
)
(defun paint-unk(x y)
    (draw-tile "error" x y)
)
(defun cls-player(x y)
    (draw-tile "white" x y)
)

(defun-tco find-start(maze &optional (i 0) (j 0))
    (let ((casella (car maze)))
    (cond
        ((eq casella entrada)
            (list i j)
        )
        ((eq casella newline)
            (find-start (cdr maze) 0 (1+ j))
        )
        (t
            (find-start (cdr maze) (1+ i) j)
        )
    )
        
    )
)

(defun-tco game-loop(name &optional (maze-data nil) (maze-x 0) (maze-y 0) (player-x 32) (player-y -32) (repaint t))
    (cond
    ((null maze-data)
        (let* ((maze-data (reverse (read-maze name))) (start-pos (find-start maze-data)) (x (* (car start-pos) 16)) (y (* (cadr start-pos) -16)))
        (game-loop name maze-data (* -240 (floor x 240)) (* 240 (floor y -240)) x y)
        )
    )
    (t 
        ; aprofitant \n es pot ignorar la longitud
    ; al pintar per zones, es pot "ignorar" el tamany
    (let (  (pdrawx (+ player-x (car mazepos) maze-x))
            (pdrawy (+ player-y (cadr mazepos) maze-y)))
    (cond
    ((or (eq repaint t))
        (cls)
        (paint-maze maze-data (+ (car mazepos) maze-x) (+ (cadr mazepos) maze-y))
    )
    )
    (draw-tile "luigi" pdrawx pdrawy)
    
    (color 0 0 0 255 255 255)
    (goto-xy 0 0)
    (princ "        \n")
    (princ "        \n")
    (princ "        \n")
    (princ "        \n")
    (princ "        \n")
    (princ "        \n")
    (princ "        \n")
    (princ "        \n")
    (goto-xy 0 0)
    (print player-x)
    (print player-y)
    (print pdrawx)
    (print pdrawy)
    (print dbg)
    (print maze-x)
    (print maze-y)

    (let* ((input (user-input)) (newpx (cond ((eq input 'right) (+ player-x player-speed)) ((eq input 'left) (- player-x player-speed)) (t player-x)))
                                (newpy (cond ((eq input 'up) (+ player-y player-speed)) ((eq input 'down) (- player-y player-speed)) (t player-y))))
        (cond
        ((eq input 'esq)
            nil 
        )
        (t

        (cls-player pdrawx pdrawy)

        (let ((r (and (eq input 'right) (> (+ newpx maze-x) 240))) (l (and (eq input 'left) (< (+ newpx maze-x) 16)))
              (u (and (eq input 'up) (> (+ newpy maze-y) -16))) (d (and (eq input 'down) (< (+ newpy maze-y) -240)))
            )
            (game-loop name maze-data
                            (cond
                            ((eq r t)
                                (- maze-x 240)
                            )
                            ((eq l t)
                                (+ maze-x 240)
                            )
                            (t 
                                maze-x
                            )
                            )
                            (cond
                            ((eq u t)
                                (- maze-y 240)
                            )
                            ((eq d t)
                                (+ maze-y 240)
                            )
                            (t 
                                maze-y
                            )
                            )
                            newpx
                            newpy
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

(cls)
(game-loop "laberints_exemple/50x50_1.txt")
(color 0 0 0 255 255 255)
;(draw-maze "test.txt" 1 1 )
;(terpri)
;(draw-tile "rickroll" 250 250)

;                       (cond ((eq input 'right) (+ offset-tile-x TILESIZE)) ((eq input 'left) (- offset-tile-x TILESIZE)) (t offset-tile-x))
;                       (cond ((eq input 'up) (+ offset-tile-y TILESIZE)) ((eq input 'down) (- offset-tile-y TILESIZE)) (t offset-tile-y))
