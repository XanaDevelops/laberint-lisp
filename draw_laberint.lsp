(load 'tco)

; constants
(setq paret #\#)
(setq cami #\.)
(setq entrada #\e)
(setq sortida #\s)
(setq newline #\NewLine)

(setq mazepos '(64 320))

(load 'fitxer-io)
(load 'graphfx)
(load 'user-input)
(load 'prop-util)

(setq dbg nil)

;README
; SC -> Screen Coords. Coordenades respecte la finestra
; GC -> Game Coords. Coordenades ingame
; TC -> Tile Coords. Coordenada del tile (index sobre el laberint)

; llegeix el laberint del fitxer i crea un doble array
(defun-tco read-maze (fn &optional (maze nil) (r nil) (s nil))
    (cond
    ((and (null maze) (null r))
        (read-maze fn (llegeix fn))
    )
    (t 
    (let ((c (car maze)))
        (cond
        ((null c)
            (cons s r)
        )
        ((eq c newline)
            (read-maze fn (cdr maze) (cond ((null r) (cond ((null (car s)) r) (t (list s))))
                                            (t (cons s r)) )
                            nil
            )
        )
        (t
            (read-maze fn (cdr maze) r (cons c s))
        )
        )
    ))
    )
)

; pinta el laberint a (x,y) SC
(defun-tco paint-maze(maze x y &optional (w 0) (h 0) (row (car maze)))
    (cond 
        ((and (null maze) (null row))
            t
        )
        
        ((null row)
            (paint-maze (cdr maze) x y 0 (1- h))

        )
        (t 
        ; pinta el tile corresponent
        (let ((elem (car row)) (xtile (+ (* w TILESIZE) x)) (ytile (+ (* h TILESIZE) y)))
            (cond
                ((eq elem paret)
                    (paint-paret xtile ytile)
                )
                ((eq elem cami)
                    (paint-cami xtile ytile)
                )
                ((eq elem entrada)
                    (draw-tile "start" xtile ytile)
                )
                ((eq elem sortida)
                    (draw-tile "end" xtile ytile)
                )
                ((eq elem newline)
                    nil
                )
                (t
                    (paint-unk xtile ytile)
                )
            )
            ;(get-key)
            (paint-maze maze x y (1+ w) h (cdr row))
        )

        )
    )
)

(defun paint-paret(x y)
    (draw-tile "wall" x y)
)
(defun paint-cami(x y)
    (draw-tile "path2" x y)
)
(defun paint-unk(x y)
    (draw-tile "error" x y)
)

; TODO: canviar a recorrer rec un array
(defun get-strname (tile)
    (cond
        ((eq tile entrada)
            "start"
        )
        ((eq tile sortida)
            "end"
        )
        ((eq tile paret)
            "wall"
        )
        ((eq tile cami)
            "path2"
        )
        (t
            "error"
        )
    )
)
; borra el tile del jugador de forma optima, mirant quines caselles hi és sobre
(defun cls-player(xpos ypos maze)
    (let* ((xtile (floor xpos TILESIZE)) (ytile (floor (- ypos) TILESIZE))
            (tile (get-in-maze (get maze 'data) xtile ytile)))
        ; sempre esta sobre l'origen de arredonir les coordenades
        (draw-tile (get-strname tile) (+ (* xtile TILESIZE) (car mazepos) (getx maze)) (+ (* (- ytile) TILESIZE) (cadr mazepos) (gety maze)))
    ;comprova quina casella del costat
    (cond 
        ((> (mod xpos TILESIZE) 0)
            (draw-tile (get-strname (get-in-maze (get maze 'data) (1+ xtile) ytile))
                (+ (* (1+ xtile) TILESIZE) (car mazepos) (getx maze)) (+ (* (- ytile) TILESIZE) (cadr mazepos) (gety maze)))
        )
        ((> (mod (- ypos) TILESIZE) 0)
            (draw-tile (get-strname (get-in-maze (get maze 'data) xtile (1+ ytile)))
                (+ (* xtile TILESIZE) (car mazepos) (getx maze)) (+ (* (- (1+ ytile)) TILESIZE) (cadr mazepos) (gety maze)))
        )
    )
    )
)

;retorna t si ha guanyat, nil si no
(defun check-win (maze player)
    (let* ((xpos (getx player)) (ypos (* -1 (gety player))) (xsor (* (car (get maze 'sortida)) TILESIZE)) (ysor (* (cadr (get maze 'sortida)) TILESIZE)))
        (cond
            ((< (+ (abs (- xpos xsor)) (abs (- ypos ysor)) ) (/ TILESIZE 2))
                t
            )
            (t 
                nil
            )
        )
    )
)

; cerca la primera occurencia de casella sobre maze[][]
; retorna (i,j) indexos sobre maze[][]
(defun-tco find-in-maze(maze casella &optional (i 0) (j 0) (aux nil))
    (let ((c (car maze)))
    (cond
        ((or (eq c casella) (and (null maze) (null aux)))
            (list i j)
        )
        ((null maze)
            (find-in-maze (car aux) casella 0 (1+ j) (cdr aux))
        )
        ((null aux)
            (find-in-maze (car maze) casella 0 0 (cdr maze))
        )
        (t
            (find-in-maze (cdr maze) casella (1+ i) j aux)
        )
    )
    )
)

; retorna la casella en maze[y][x]
;                                          pots fer això
(defun-tco get-in-maze(maze x y &optional (row (car maze)))
    (cond 
        ((and (= x 0) (= y 0))
            ; retornar valor
            (car row)
        )
        ((> y 0)
            (get-in-maze (cdr maze) x (1- y))
        )
        ((> x 0)
            (get-in-maze maze (1- x) y (cdr row))
        )
    )
)

;loop principal del joc
(defun-tco game-loop(name &optional (maze 'maze) (player 'player) (steps 0) (repaint t))
    (cond
    ((null (get maze 'data))
        ; en la primera cridada inicialitza els valors per defecte
        (let* ((maze-data (read-maze name)) (start-pos (find-in-maze maze-data entrada)) (x (* (car start-pos) TILESIZE)) (y (* (cadr start-pos) (* -1 TILESIZE))))
        (putprop maze (* -240 (floor x 240)) 'x)
        (putprop maze (* 240 (floor y -240)) 'y)
        (putprop maze maze-data 'data)
        (putprop maze (find-in-maze (get maze 'data) sortida) 'sortida)
        ;player
        (putprop player x 'x)
        (putprop player y 'y)
        (putprop player 2 'speed)
        (game-loop name maze player)
        )
    )
    (t 
    ; obte les posicions de dibuixat del jugador
    (let (  (pdrawx (+ (getx player) (car mazepos) (getx maze)))
            (pdrawy (+ (gety player) (cadr mazepos) (gety maze))))
    (cond
    ((or (eq repaint t)) ;principalment al canviar de pantalla
        (cls)
        (paint-maze (get maze 'data) (+ (car mazepos) (getx maze)) (+ (cadr mazepos) (gety maze)))
    )
    )

    ; dibuixa el jugador    
    (draw-tile "luigi" pdrawx pdrawy)
    
    ; DEBUG
    (color 0 0 0 255 255 255)
    (goto-xy 0 0)
    (princ "              \n")
    (princ "        \n")
    (princ "        \n")
    (princ "        \n")
    (princ "        \n")
    (princ "        \n")
    (princ "        \n")
    (princ "        \n")
    (goto-xy 0 0)
    (print (symbol-plist player))
    (print pdrawx)
    (print pdrawy)
    (print (get-in-maze (get maze 'data) (floor (getx player) TILESIZE) (- (floor (gety player) TILESIZE))))
    (print (get maze 'sortida))
    (print (getx maze))
    (print (gety maze))
    ; DEBUG

    ; llegeix entrada i calcula nova posició, comproba colisions
    (let* ((input (user-input)) (px (getx player)) (py (gety player))
            )
        (cond
        ; sortir del joc
        ((eq input 'esq)
            steps 
        )
        ((check-win maze player)
            (princ "HAS GUANYAT!!!!!!!!!!!\n") ; missatge provisional
            steps
        )
        (t

        ;borra tile del jugador
        (cls-player px py maze)

        ; calcula nova posició del laberint, depenent de a quina direcció es vol anar i la posició del jugador respecte la pantala
        ; fa scroll o no
        (let* (
            (newpcoords (new-player-pos player maze input))
            (newpx (car newpcoords)) (newpy (cadr newpcoords))
            
            (r (and (eq input 'right) (> (+ newpx (getx maze)) 240))) (l (and (eq input 'left) (< (+ newpx (getx maze)) 16)))
              (u (and (eq input 'up) (> (+ newpy (gety maze)) -16))) (d (and (eq input 'down) (< (+ newpy (gety maze)) -240)))
              (newmx (cond ((eq r t) (- (getx maze) 240)) ((eq l t) (+ (getx maze) 240)) (t (getx maze))))
              (newmy (cond ((eq u t) (- (gety maze) 240)) ((eq d t) (+ (gety maze) 240))(t (gety maze))))

              
            )
            ; nou estat de la partida
            (game-loop name 
                            (update-prop (update-prop maze 'x newmx) 'y newmy)
                            (update-prop (update-prop player 'x newpx) 'y newpy)
                            (1+ steps)
                            (cond ((or r l u d) t) (t nil))

            )
        )
        )
        )
    )
    )
    )
    )
)
; Calcula la nova posició del jugador
; retorna (newx, newy)
(defun new-player-pos (player maze input)
    (let* ((px (getx player)) (py (gety player)) (ps (get player 'speed)))
        (list
        ;new x 
        (cond ((and (eq input 'right) (can-move-h (get maze 'data) (+ px TILESIZE) py)) (+ px ps))
              ((and (eq input 'left) (can-move-h (get maze 'data) (- px ps) py)) (- px ps))
              (t px))
        ;new Y
        (cond ((and (eq input 'up) (can-move-v (get maze 'data) px (- py (- ps)))) (+ py ps))
              ((and (eq input 'down) (can-move-v (get maze 'data) px (+ py -17))) (- py ps))
              (t py))
        )
    )
)

(defun can-move-h (maze x y)
    (let ((xtile (floor x TILESIZE)) (ytile (floor (- y) TILESIZE)) (ytile2 (floor (+ (- y) 15) TILESIZE)))
        (not (or (eq (get-in-maze maze xtile ytile) paret) (eq (get-in-maze maze xtile ytile2) paret)))
    )
)

(defun can-move-v (maze x y)
    (let ((xtile (floor x TILESIZE)) (xtile2 (floor (+ x 15) TILESIZE)) (ytile (floor (- y) TILESIZE)))
        (not (or (eq (get-in-maze maze xtile ytile) paret) (eq (get-in-maze maze xtile2 ytile) paret)))
    )
)

(cls)
;si peta algo descomentar
;(trace game-loop)
;(trace update-prop)
;(trace find-in-maze)
;(trace paint-maze)

;(print (game-loop "test.txt"))
(print (game-loop "laberints_exemple/40x30_2.txt"))
(color 0 0 0 255 255 255)
;(draw-maze "test.txt" 1 1 )
;(terpri)
;(draw-tile "rickroll" 250 250)