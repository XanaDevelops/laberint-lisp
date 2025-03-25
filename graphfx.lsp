

(setq TILESIZE 16)
(setq SCREEN_W 649)
(setq SCREEN_H 360) ;???

(defun draw-tile (tilename x y)
    (cond
    ((and (>= (+ x TILESIZE) 0) (< (- x TILESIZE) SCREEN_W) (>= (+ y TILESIZE) 0) (< (- y TILESIZE) SCREEN_H))
        (move x y)
        (mapcar 'eval (llegeix-exp (strcat (strcat "tiles/" tilename) ".lsp")))
    )
    )
    
)