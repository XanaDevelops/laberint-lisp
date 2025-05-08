(load 'tco)

(setq wall #\#)
(setq path #\.)
(setq newline #\NewLine)
(setq mazepos '(64 336))

(load 'fitxer-io)
(load 'graphfx)
(load 'user-input)




(defun read-maze(fn)
    (llegeix fn)
)

(defun-tco paint-maze(maze x y w h offsetx offsety)
    (cond 
        ((null maze)
            nil
        )
        (t 
        (let ((elem (car maze)))
            (cond
                ((eq elem wall)
                    (paint-wall (+ (* (+ w offsetx) TILESIZE) x) (+ (* (+ h offsety) TILESIZE) y))
                )
                ((eq elem path)
                    (paint-path (+ (* (+ w offsetx) TILESIZE) x) (+ (* (+ h offsety) TILESIZE) y))
                )
                ((eq elem newline)
                    nil
                )
                (t
                    (paint-unk (+ (* (+ w offsetx) TILESIZE) x) (+ (* (+ h offsety) TILESIZE) y))
                )
            )
            ;(get-key)
            (paint-maze 
            ;x y w h
            (cdr maze) x y (cond ((eq elem newline) 0) (t (1+ w))) (cond ((eq elem newline) (1- h)) (t h))
            ;offsetx offsety
            offsetx offsety
            )

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
(defun-tco draw-maze(name &optional (offsetx 0) (offsety 0))
    (cls)
    ; aprofitant \n es pot ignorar la longitud
    ; la altura es pot suposar maxima, paint-maze atura al extinguir el maze
    ; o pintar de baix a dalt amb un reverse
    (paint-maze (reverse (read-maze name)) (car mazepos) (cadr mazepos) 0 0 offsetx offsety)
    (let ((input (user-input)))
        (cond
        ((eq input 'esq)
            (top-level) ;;hacky way
        )
        )
        (draw-maze name (cond ((eq input 'right) (1+ offsetx)) ((eq input 'left) (1- offsetx)) (t offsetx))
                        (cond ((eq input 'up) (1+ offsety)) ((eq input 'down) (1- offsety)) (t offsety)))
    )
)
(draw-maze "laberint.txt")
;(draw-maze "test.txt" 1 1 )
;(terpri)
;(draw-tile "rickroll" 250 250)
