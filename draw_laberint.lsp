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




(defun read-maze(fn)
    (llegeix fn)
)

(defun-tco paint-maze(maze x y w h offset-tile-x offset-tile-y)
    (cond 
        ((null maze)
            nil
        )
        (t 
        (let ((elem (car maze)) (xtile (+ (* (+ w offset-tile-x) TILESIZE) x)) (ytile (+ (* (+ h offset-tile-y) TILESIZE) y)))
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
                ;offset-tile-x offset-tile-y
                offset-tile-x offset-tile-y
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
(defun-tco game-loop(name &optional (maze-data nil) (offset-tile-x 0) (offset-tile-y 0) (player-x -1) (player-y -1))
    (cond
    ((null maze-data)
       (game-loop name (reverse (read-maze name)) offset-tile-x offset-tile-y player-x player-y)
    )
    (t 
        ; aprofitant \n es pot ignorar la longitud
    ; al pintar per zones, es pot "ignorar" el tamany
    (cond
    ((= player-x -1)
        (cls)
        (paint-maze maze-data (car mazepos) (cadr mazepos) 0 0 offset-tile-x offset-tile-y)
    )
    (t 
        (draw-tile "luigi" (+ player-x (car mazepos) (- (* offset-tile-x TILESIZE))) (+ player-y (cadr mazepos) (- (* offset-tile-y TILESIZE))))
    )
    )
    
    

    (let* ((input (user-input)) (newpx (cond ((eq input 'right) (+ player-x player-speed)) ((eq input 'left) (- player-x player-speed)) (t player-x)))
                                (newpy (cond ((eq input 'up) (+ player-y player-speed)) ((eq input 'down) (- player-y player-speed)) (t player-y))))
        (cond
        ((eq input 'esq)
            nil 
        )
        (t

        (cls-player (+ player-x (car mazepos) (- (* offset-tile-x TILESIZE))) (+ player-y (cadr mazepos) (- (* offset-tile-y TILESIZE))))

        (game-loop name maze-data
                            offset-tile-x offset-tile-y
                            newpx
                            newpy

        ))
        )
    )
    )
    )
)
(cls)
(game-loop "laberints_exemple/25x25_1.txt")
;(draw-maze "test.txt" 1 1 )
;(terpri)
;(draw-tile "rickroll" 250 250)

;                       (cond ((eq input 'right) (+ offset-tile-x TILESIZE)) ((eq input 'left) (- offset-tile-x TILESIZE)) (t offset-tile-x))
;                       (cond ((eq input 'up) (+ offset-tile-y TILESIZE)) ((eq input 'down) (- offset-tile-y TILESIZE)) (t offset-tile-y))
