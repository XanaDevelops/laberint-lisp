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
(defun-tco game-loop(name &optional (maze-data nil) (offset-tile-x 0) (offset-tile-y 0) (player-x 32) (player-y -32) (isfirst t))
    
    (cond
    ((null maze-data)
       (game-loop name (reverse (read-maze name)) offset-tile-x offset-tile-y player-x player-y)
    )
    (t 
        ; aprofitant \n es pot ignorar la longitud
    ; al pintar per zones, es pot "ignorar" el tamany
    (let (  (pdrawx (+ player-x (car mazepos) (+ (* offset-tile-x TILESIZE))))
            (pdrawy (+ player-y (cadr mazepos) (- (* offset-tile-y TILESIZE)))))
    (cond
    ((or (eq t t) (= (rem player-x (* TILESIZE TILESIZE)) 0) (= (rem (abs player-y) (* TILESIZE TILESIZE)) 0))
        (cls)
        (paint-maze maze-data (car mazepos) (cadr mazepos) 0 0 offset-tile-x offset-tile-y)
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
    (print offset-tile-x)
    (print offset-tile-y)

    (let* ((input (user-input)) (newpx (cond ((eq input 'right) (+ player-x player-speed)) ((eq input 'left) (- player-x player-speed)) (t player-x)))
                                (newpy (cond ((eq input 'up) (+ player-y player-speed)) ((eq input 'down) (- player-y player-speed)) (t player-y))))
        (cond
        ((eq input 'esq)
            nil 
        )
        (t

        (cls-player pdrawx pdrawy)

        (game-loop name maze-data
                            ;offset-tile-x offset-tile-y
                            (-  (let ((ox (- newpx (* TILESIZE offset-tile-x))))
                                (setq dbg ox)
                                (cond   ((> ox (* TILESIZE (1- TILESIZE)))
                                            (+ offset-tile-x TILESIZE)
                                        )
                                        ((< ox TILESIZE)
                                            (- offset-tile-x TILESIZE)
                                        )
                                        (t
                                            offset-tile-x
                                        )
                                )
                                )
                            )
                            ;(- (* (floor (abs newpy) (* TILESIZE TILESIZE)) TILESIZE))
                            offset-tile-y
                            newpx
                            newpy
                            nil

        ))
        )
    )
    )
    )
    )
)
(cls)
(game-loop "laberints_exemple/25x25_1.txt")
(color 0 0 0 255 255 255)
;(draw-maze "test.txt" 1 1 )
;(terpri)
;(draw-tile "rickroll" 250 250)

;                       (cond ((eq input 'right) (+ offset-tile-x TILESIZE)) ((eq input 'left) (- offset-tile-x TILESIZE)) (t offset-tile-x))
;                       (cond ((eq input 'up) (+ offset-tile-y TILESIZE)) ((eq input 'down) (- offset-tile-y TILESIZE)) (t offset-tile-y))
