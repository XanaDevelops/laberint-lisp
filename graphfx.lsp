

(setq TILESIZE 16)


(defun draw-tile (tilename x y)
    (move x y)
    (mapcar 'eval (llegeix-exp (strcat (strcat "tiles/" tilename) ".lsp")))
)