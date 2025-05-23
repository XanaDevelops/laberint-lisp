(load 'CONST)
(load 'tco)
(load 'fitxer-io)
(load 'graphfx)
(load 'user-input)
(load 'prop-util)
(load 'aux-game)
(load 'draw_laberint)
(load 'draw_border)
(load 'draw_minimap)


;loop principal del joc
(defun-tco game-loop(name &optional (maze (gensym "maze-")) (player (gensym "player-")) (steps 0) (repaint t) (extra (gensym "extra-")))
    (cond
    ((null (get maze 'data))
        ; en la primera cridada inicialitza els valors per defecte
        (let* ((maze-data (read-maze name)) (start-pos (find-in-maze maze-data Centrada)) (x (* (car start-pos) TILESIZE)) (y (* (cadr start-pos) (* -1 TILESIZE))))
        (putprop maze (* (- SCREENPIXEL-M1) (floor x SCREENPIXEL-M1)) 'x)
        (putprop maze (* SCREENPIXEL-M1 (floor y (- SCREENPIXEL-M1))) 'y)
        (putprop maze maze-data 'data)
        (putprop maze (find-in-maze (get maze 'data) Csortida) 'pos-sortida)
        ;player
        (putprop player x 'x)
        (putprop player y 'y)
        (putprop player 4 'speed) ; que sigui par
        (putprop player (floor x TILESIZE) 'tilex)
        (putprop player (floor (- y) TILESIZE) 'tiley)
        ;extra
        (putprop extra (gen-keys maze-data) 'keys)
        (putprop extra (gen-minimap (get maze 'data)) 'minimap)

        (game-loop name maze player 0 t extra)
        )
    )
    (t 
    ; obte les posicions de dibuixat del jugador
    (let (  (pdrawx (+ (getx player) (car mazepos) (getx maze)))
            (pdrawy (+ (gety player) (cadr mazepos) (gety maze))))
    (cond
    ((or (eq repaint t)) ;principalment al canviar de pantalla
        (color 255 255 255 0 0 0)
        (cls)
        (paint-maze (get maze 'data) (+ (car mazepos) (getx maze)) (+ (cadr mazepos) (gety maze)))
        (draw-border (- (car mazepos) 8) (+ (cadr mazepos) 8) (1+ TILE_W) (1+ TILE_H))
        (paint-minimap (get maze 'data) 400 (- (cadr mazepos) 4))
        (draw-border 388 (+ (cadr mazepos) 8) 13 13)
    )
    )

    ;dibuixa la sortida tancada, si escau
    (let* ((sx (car (get maze 'pos-sortida))) (sy (cadr (get maze 'pos-sortida)))
                (sd (tile-to-draw sx sy maze)) (sdx (car sd)) (sdy (cadr sd))
            )
        (cond 
        ((> (length (get extra 'keys)) 0)
            (draw-tile "salida_cerrada" sdx sdy)
        )
        (t 
            (draw-tile "end" sdx sdy)
        )
        )
    )
    
    ; dibuixa el jugador    
    (draw-tile "luigi" pdrawx pdrawy)

    ;dibuixa extres
    ; claus
    (draw-keys (get extra 'keys) maze)

    ; DEBUG
    (color 255 255 255 0 0 0)
    (goto-xy 0 0)
    (princ "                    \n")
    (princ "                    \n")
    (princ "      \n")
    (princ "      \n")
    (princ "      \n")
    (princ "      \n")
    (princ "      \n")
    (princ "      \n")
    (princ "      \n")
    (princ "      \n")
    (goto-xy 0 0)
    (print (symbol-plist player))
    (print (get extra 'keys))
    (print pdrawx)
    (print pdrawy)
    (print (get-in-maze (get maze 'data) (floor (getx player) TILESIZE) (- (floor (gety player) TILESIZE))))
    (print (get maze 'pos-sortida))
    (print (getx maze))
    (print (gety maze))
    (print steps)
    ; DEBUG

    ; llegeix entrada i calcula nova posició, comproba colisions
    (let* ((input (user-input)) (px (getx player)) (py (gety player)))
        (cond
        ; sortir del joc
        ((eq input 'esq)
            steps 
        )
        ((check-win maze player extra)
            (princ "HAS GUANYAT!!!!!!!!!!!\n") ; missatge provisional
            steps
        )
        ((eq input 'admin)
            (eval (read))
            (read)
        )
        (t

        ;borra tile del jugador
        (cls-player px py maze)

        ; calcula nova posició del laberint, depenent de a quina direcció es vol anar i la posició del jugador respecte la pantala
        ; fa scroll o no
        (let* (
            (newpcoords (new-player-pos player maze input))
             (newpx (car newpcoords)) (newpy (cadr newpcoords))
            
            (newmazecoords (new-maze-pos newpx newpy maze input))
              (newmx (car newmazecoords))
              (newmy (cadr newmazecoords))
              (do-repaint (caddr newmazecoords))
            (newtileplayer (update-steps player))
              (newtilex (car newtileplayer))
              (newtiley (cadr newtileplayer))
              (addsteps (caddr newtileplayer))
            )
            ; nou estat de la partida
            (game-loop name 
                (update-prop (update-prop maze 'x newmx) 'y newmy)
                (update-prop (update-prop (update-prop (update-prop player 'x newpx) 'y newpy) 'tilex newtilex) 'tiley newtiley)
                (+ steps addsteps)
                do-repaint 
                (update-keys player extra)
            )
        )
        )
        )
    )
    )
    )
    )
)


(cls)
;si peta algo descomentar
;(trace game-loop)
;(trace update-prop)
;(trace find-in-maze)
;(trace paint-maze)
;(trace tile-to-draw)
;(trace draw-keys)
;(trace draw-tile)
;(trace draw-border)
;(print (game-loop "test.txt"))
(setq *random-state* (make-random-state t)) ;revisar seeds
(print (game-loop "laberints_exemple/25x25_2.txt"))
(color 0 0 0 255 255 255)
;(draw-maze "test.txt" 1 1 )
;(terpri)
;(draw-tile "rickroll" 250 250)