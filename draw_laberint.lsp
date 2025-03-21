(load 'fitxer-io)
(load 'graphfx)

(setq wall #\#)
(setq path #\.)

(defun read-maze(fn)
    (llegeix fn)
)

(defun paint-maze(maze current-w w h)
    (cond 
        ((null maze)
            nil
        )
        (t 
        (let ((elem (car maze)))
            (cond
                ((eq elem wall)
                    (paint-wall 50 50)
                )
                ((eq elem path)
                    (paint-path -1 -1)
                )
                ((eq elem #\NewLine)
                    nil
                )
                (t
                    (paint-unk -1 -1)
                )
            )
        )
        (paint-maze (cdr maze) (cond ((= current-w 0) (terpri) w) (t (- current-w 1))) w h)
        )
    )
    t
    
)

(defun paint-wall(x y)
    (draw-tile "face" x y)
)
(defun paint-path(x y)
    (princ " ")
)
(defun paint-unk(x y)
    (princ "?")
)
(defun draw-maze(name w h)
    (cls)
    (paint-maze (read-maze name) w w h)
)
;(draw-maze "laberints_exemple/10x10_massapetit_1.txt" 10 10)
(cls)
;(draw-maze "test.txt" 1 1 )
;(terpri)
(draw-tile "face" 200 200)