;========================================================================
; Classe que s'encarrega de dibuixar els elements gràfics del joc.
; Inclou funcions per pintar 'tiles' amb o sense comprovació de límits 
; de pantalla.
;========================================================================

(load 'CONST)

;; =============================================================================
;; Funció: draw-tile
;;  Dibuixa un tile a (x, y) limitat al viewport del laberint
;;
;; Paràmetres:
;;   - tilename: nom del tile
;;   - x,y: posició de pantalla a pintar
;;
;; =============================================================================
(defun draw-tile (tilename x y)
    (cond
    ;a ver, si queremos pintar tiles fuera del marco del laberinto como que esto aqui no....
    ((and (> (+ x TILESIZE) (car mazepos)) (< (- x TILESIZE) SCREEN_W) (< (- y TILESIZE) (cadr mazepos)) (>= (- y TILESIZE) SCREEN_H))
        (draw-tile-nocheck tilename x y)
    )
    )
    t
)

;; =============================================================================
;; Funció: draw-tile-nocheck
;;  Dibuixa un tile a (x, y) sense limitacions
;;
;; Paràmetres:
;;   - tilename: nom del tile
;;   - x,y: posició de pantalla a pintar
;;
;; =============================================================================
(defun draw-tile-nocheck(tilename x y)
    (move x y)
    (mapcar 'eval (llegeix-exp (strcat (strcat "tiles/" tilename) ".lsp")))
)