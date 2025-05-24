(load 'CONST)
(load 'draw_border)

(defun draw-hud (extra steps)
    (color 255 255 255 0 0 0)
    (goto-xy 50 17)
    (princ "                       ")
    (goto-xy 50 17)
    (format t "claus restants: ")
    (goto-xy 50 18)
    (format t "passes realitzades: ~D" steps)
    (draw-hud-keys (length (get extra 'keys)))
    (draw-border (- (car mmappos) 12) (- (cadr mmappos) (* TILESIZE 13) -0) 13 3)
)

(defun draw-hud-keys(remain &optional (x 520) (y 120))
    (cond
    ((= remain 0) t)
    (t 
        (draw-tile-nocheck "llave" x y)
        (draw-hud-keys (1- remain) (+ x TILESIZE) y)
    )
    )
)
