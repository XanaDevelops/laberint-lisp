(load 'CONST)

(defun draw-minimap(x y maze player extra)

)

(defun-tco gen-minimap (maze &optional (row (car maze)))

)

; pinta el laberint a (x,y) SC
(defun-tco paint-minimap(maze x y &optional (w 0) (h 0) (row (car maze)))
    (cond 
        ((and (null maze) (null row))
            t
        )
        
        ((null row)
            (paint-minimap (cdr maze) x y 0 (1- h))

        )
        (t 
        ; pinta el tile corresponent
        (let ((elem (car row)) (xtile (+ (* w MM_TILESIZE) x)) (ytile (+ (* h MM_TILESIZE) y)))
            (cond
                ((eq elem Cparet)
                    (draw-mm-paret xtile ytile)
                )
                ((eq elem Ccami)
                    (draw-mm-cami xtile ytile)
                )
                
                ((eq elem newline)
                    nil
                )
                (t
                    nil
                )
            )
            ;(get-key)
            (paint-minimap maze x y (1+ w) h (cdr row))
        )
        )
    )
)

(defun draw-mm-paret (x y)
    (move x y)
    (color 180 180 180)
    (draw-mm-square)
)
(defun draw-mm-cami (x y)
    (move x y)
    (color 0 0 0)
    (draw-mm-square)
)
(defun draw-mm-recorregut (x y)
    (move x y)
    (color 50 0 0)
    (draw-mm-square)
)
(defun draw-mm-player (x y)
    (move x y)
    (color 255 0 255)
    (draw-mm-square)
)

(defun draw-mm-square()
    (drawrel MM_TILESIZE 0)
    (moverel (- MM_TILESIZE) -1)
    (drawrel MM_TILESIZE 0)
    (moverel (- MM_TILESIZE) -1)
    (drawrel MM_TILESIZE 0)
    (moverel (- MM_TILESIZE) -1)
    (drawrel MM_TILESIZE 0)

)