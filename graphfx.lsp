(setq TILESIZE 16)
(setq TILE_W 16)
(setq TILE_H 16)
(setq SCREENPIXEL-M1 (* TILESIZE (1- TILESIZE)))
(setq SCREEN_W (+ (car mazepos) (* TILESIZE TILE_W)))
(setq SCREEN_H (+ (cadr mazepos) (* TILESIZE (- -1 TILE_H)))) ;???

(defun draw-tile (tilename x y)
    (cond
    ;a ver, si queremos pintar tiles fuera del marco del laberinto como que esto aqui no....
    ((and (> (+ x TILESIZE) (car mazepos)) (< (- x TILESIZE) SCREEN_W) (< (- y TILESIZE) (cadr mazepos)) (>= (- y TILESIZE) SCREEN_H))
        (move x y)
        (mapcar 'eval (llegeix-exp (strcat (strcat "tiles/" tilename) ".lsp")))
    )
    )
    t
)