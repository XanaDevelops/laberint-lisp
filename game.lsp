(load 'CONST)
(load 'tco)
(load 'fitxer-io)
(load 'graphfx)
(load 'music)
(load 'user-input)
(load 'prop-util)
(load 'aux-game)
(load 'draw_laberint)
(load 'draw_border)
(load 'draw_minimap)
(load 'draw_hud)
(load 'libs\\listLib)

;========================================================================
;; Aquesta classe implementa la funció principal `explora`, que implementa
;; el bucle de joc per explorar un laberint, incloent la inicialització, 
;; moviment del jugador, dibuix del laberint, gestió d'objectes 
;; especials (claus, sortida), actualització del HUD, i lògica de 
;; guany o sortida.
;========================================================================

;; =========================================
;; Funció: "explora"
;; La funcio principal per explorar un laberint.
;; amb els parametres opcionals, detecta si es la primera cridada, si es així, inicialitza els valors.
;; Amb succesius let es poden crear variables per no recalcular valors, així, es pot jugar al laberint
;; Per dibuixar cada vegada que s'arriba a una altra secció del laberint, es repinta tot, però 
;; si no fa falta, només es repinta el jugador i s'actualitza el minimapa, si escau.
;; Per a fer el codi més llegible, nombroses funcions auxiliars s'han fet servir
;;
;; Extres:
;;  - Us d'sprites detallats + GUI
;;  - Minimapa
;;  - Claus
;;  - Fog of War (al minimapa)
;;  - Pintat de recorregut (al minimapa)

;; Paràmetres:
;;  - name: ruta del laberint
;;  - (opcional) maze: emmagatzema la informació relativa al laberint
;;  - (opcional) player: emmagatzema la informació relativa al jugador
;;  - (opcional) steps: comptador de passos
;;  - (opcional) repaint: si t, fa repintar tot
;;  - (opcional) extra: emmagatzema la informació relativa als extres o miscelanea
;;
;; Retorn:
;;  - Jugador amb el resultat de la partida o nil si abortat
;; ==========================================
(defun-tco explora(name &optional (maze (gensym "maze-")) (player (gensym "player-")) (steps 0) (repaint t) (extra (gensym "extra-")))
    (cond
    ((null (get maze 'data))
        (color 255 255 255 0 0 0)
        (cls)
        (stop-all)
        ; en la primera cridada inicialitza els valors per defecte
        (let* ((maze-data (read-maze name)) (start-pos (find-in-maze maze-data Centrada)) (x (* (car start-pos) TILESIZE)) (y (* (cadr start-pos) (* -1 TILESIZE))))
        (putprop maze (* (- SCREENPIXEL-M1) (floor x SCREENPIXEL-M1)) 'x)
        (putprop maze (* SCREENPIXEL-M1 (floor y (- SCREENPIXEL-M1))) 'y)
        (putprop maze maze-data 'data)
        (putprop maze (find-in-maze (get maze 'data) Csortida) 'pos-sortida)
        ;player
        (goto-xy 27 5)
        (princ "Introdueix el teu nom:\n")
        (goto-xy 24 6)
        (draw-border 180 310 15 3)
        (putprop player (read) 'pname)
        (putprop player x 'x)
        (putprop player y 'y)
        (putprop player 4 'speed) ; que sigui par
        (putprop player (floor x TILESIZE) 'tilex)
        (putprop player (floor (- y) TILESIZE) 'tiley)
        ;extra
        (putprop extra (gen-keys maze-data) 'keys)
        (putprop extra (update-minimap maze-data (get player 'tilex) (get player 'tiley) (gen-minimap (get maze 'data))) 'minimap)
        (play-song (+ (random 2) 1) t)
        (explora name maze player 0 t extra)
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
        ;minimapa
        (paint-minimap (get extra 'minimap) (car mmappos) (cadr mmappos))
        (draw-border (- (car mmappos) 12) (+ (cadr mmappos) 12) 13 13)    
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
    ;mmap
    (draw-mm-player player)
    ;HUD
    (draw-hud extra steps)


; DEBUG
    (cond 
    ((eq dbg t)
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
        )
    )
; DEBUG

    ; llegeix entrada i calcula nova posició, comproba colisions
    (let* ((input (user-input)) (px (getx player)) (py (gety player)))
        (cond
        ; sortir del joc
        ((eq input 'esq)
            nil 
        )
        ((check-win maze player extra)
            (you-win (get player 'pname) steps)
            (make-jugador :nom (get player 'pname) :passos steps :laberint name)
        )
        ((eq input 'admin)
            (eval (read)) ;permet modificar constants, però no pot llegir 
            (explora name maze player steps nil extra)
        )
        (t

        ;borra tile del jugador
        (cls-player px py maze)

        (cls-mm-player player)

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
            (newminimap (update-minimap (get maze 'data) newtilex newtiley (get extra 'minimap)))
            )
            ; nou estat de la partida
            (explora name 
                (update-prop (update-prop maze 'x newmx) 'y newmy)
                (update-prop (update-prop (update-prop (update-prop player 'x newpx) 'y newpy) 'tilex newtilex) 'tiley newtiley)
                (+ steps addsteps)
                do-repaint 
                (update-recorregut player (update-prop (update-keys player extra) 'minimap newminimap))
            )
        )
        )
        )
    )
    )
    )
    )
)


;(cls)
;si peta algo descomentar
;(trace explora)
;(trace update-prop)
;(trace find-in-maze)
;(trace paint-maze)
;(trace tile-to-draw)
;(trace draw-keys)
;(trace draw-tile)
;(trace draw-border)
;(print (explora "test.txt"))
;
;(print (explora "laberints_exemple/25x25_2.txt"))
;(print (explora "laberints_exemple/10x10_massapetit_1.txt"))
;(color 0 0 0 255 255 255)
;(draw-maze "test.txt" 1 1 )
;(terpri)
;(draw-tile "rickroll" 250 250)