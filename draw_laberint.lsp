(load 'tco)

(load 'fitxer-io)
(load 'graphfx)


(setq wall #\#)
(setq path #\.)
(setq newline #\NewLine)

(defun read-maze(fn)
    (llegeix fn)
)

(defun-tco paint-maze(maze x y w h)
    (cond 
        ((null maze)
            nil
        )
        (t 
        (let ((elem (car maze)))
            (cond
                ((eq elem wall)
                    (paint-wall (+ (* w TILESIZE) x) (+ (* h TILESIZE) y) )
                )
                ((eq elem path)
                    (paint-path (+ (* w TILESIZE) x) (+ (* h TILESIZE) y))
                )
                ((eq elem newline)
                    nil
                )
                (t
                    (paint-unk (+ (* w TILESIZE) x) (+ (* h TILESIZE) y))
                )
            )
            ;(get-key)
            (paint-maze (cdr maze) x y (cond ((eq elem newline) 0) (t (1+ w))) (cond ((eq elem newline) (1- h)) (t h)))

        )

        )
    )
)

(defun paint-wall(x y)
    (draw-tile "wall" x y)
)
(defun paint-path(x y)
    ;(princ " ")
)
(defun paint-unk(x y)
    (draw-tile "error" x y)
)
(defun draw-maze(name)
    (cls)
    ; aprofitant \n es pot ignorar la longitud
    ; la altura es pot suposar maxima, paint-maze atura al extinguir el maze
    ; o pintar de baix a dalt amb un reverse
    (paint-maze (reverse (read-maze name)) 25 350 0 0)
)
(draw-maze "laberints_exemple/10x10_massapetit_1.txt")
;(draw-maze "test.txt" 1 1 )
;(terpri)
;(draw-tile "rickroll" 250 250)
