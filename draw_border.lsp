;; =============================================================================
;; Funció: draw-border
;;  Utilitzant la tecnica del "9-slicing", dibuixa un borde rectangular de tamany (w,h)
;;  a (x, y).
;;  S'ha de tenir en compte que no dibuixa per dins, només el borde en si.
;;
;; Paràmetres:
;;   - x,y: posició de pantalla a pintar
;;   - w,h: tamany del borde
;;   - (optional) cw, ch: iteradors per a la recursivitat
;; 
;; Retorn:
;;   - res
;; =============================================================================
(defun draw-border (x y w h &optional (cw 0) (ch 0))
    (let* ((drawx (+ x (* cw TILESIZE))) (drawy (+ y (* (- ch) TILESIZE))))
        (cond
        ((and (eq cw w) (eq ch h))
            (draw-tile-nocheck "border_D_R" drawx drawy)
        )
        ((and (eq cw 0) (eq ch 0))
            (draw-tile-nocheck "border_U_L" drawx drawy)
            (draw-border x y w h (1+ cw) ch)
        )
        ((and (eq cw 0) (eq ch h))
            (draw-tile-nocheck "border_D_L" drawx drawy)
            (draw-border x y w h (1+ cw) ch)
        )
        ((and (eq cw w) (eq ch 0))
            (draw-tile-nocheck "border_U_R" drawx drawy)
            (draw-border x y w h 0 (1+ ch))
        )
        ((eq cw w)
            (draw-tile-nocheck "border_X_R" drawx drawy)
            (draw-border x y w h 0 (1+ ch))
        )
        ((eq cw 0)
            (draw-tile-nocheck "border_X_L" drawx drawy)
            (draw-border x y w h (1+ cw) ch)
        )
        ((eq ch 0)
            (draw-tile-nocheck "border_X_U" drawx drawy)
            (draw-border x y w h (1+ cw) ch)        
        )
        ((eq ch h)
            (draw-tile-nocheck "border_X_D" drawx drawy)
            (draw-border x y w h (1+ cw) ch)        
        )
        (t
            (draw-border x y w h w ch)
        )
        )
    )
)