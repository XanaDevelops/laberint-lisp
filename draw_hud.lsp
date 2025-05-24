(load 'CONST)
(load 'draw_border)

(defun draw-hud (extra steps)
    (color 255 255 255 0 0 0)
    (goto-xy 50 17)
    (format t "claus restants: ")
    (goto-xy 50 18)
    (format t "passes realitzades: ~D" steps)
    (draw-border (- (car mmappos) 12) (- (cadr mmappos) (* TILESIZE 13) -0) 13 3)

)