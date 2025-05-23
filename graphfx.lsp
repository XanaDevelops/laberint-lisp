(load 'CONST)

(defun draw-tile (tilename x y)
    (cond
    ;a ver, si queremos pintar tiles fuera del marco del laberinto como que esto aqui no....
    ((and (> (+ x TILESIZE) (car mazepos)) (< (- x TILESIZE) SCREEN_W) (< (- y TILESIZE) (cadr mazepos)) (>= (- y TILESIZE) SCREEN_H))
        (draw-tile-nocheck tilename x y)
    )
    )
    t
)

(defun draw-tile-nocheck(tilename x y)
    (move x y)
    (mapcar 'eval (llegeix-exp (strcat (strcat "tiles/" tilename) ".lsp")))
)