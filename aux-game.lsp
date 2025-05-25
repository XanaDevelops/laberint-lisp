(load 'CONST)
(load 'prop-util)

;; =========================================
;; Funció: "check-win"
;; Comproba si es compleixen les condicions per guanyar
;; (en casella de sortida + tenir totes les claus)
;;
;; Paràmetres:
;;  - maze: propietats del laberint
;;  - player: propietats del jugador
;;  - extra: propietats extra
;;
;; Retorn:
;;  - t o nil si s'ha guanyat o no
;; ==========================================
(defun check-win (maze player extra)
    (let* ((xpos (getx player)) (ypos (* -1 (gety player))) (xsor (* (car (get maze 'pos-sortida)) TILESIZE)) (ysor (* (cadr (get maze 'pos-sortida)) TILESIZE))
            (nkeys (length (get extra 'keys)))
            )
        (cond
            ((and (< (+ (abs (- xpos xsor)) (abs (- ypos ysor)) ) (/ TILESIZE 2)) (eq nkeys 0))
                t
            )
            (t 
                nil
            )
        )
    )
)

;; =========================================
;; Funció: "you win"
;; Mostra el missatge de victoria
;;
;; Paràmetres:
;;  - name: nom del jugador
;;  - steps: passos realitzats
;;
;; Retorn:
;;  - res
;; ==========================================
(defun you-win(name steps)
    (clear-text 25 8 34 6)

    (goto-xy 26 9)
    (princ "           HAS GUANYAT\n")
    (goto-xy 26 11)
    (format t "~S ha fet: ~S passes\n" name steps)
    (draw-border 192 258 17 5)
    (stop-all)
    (play-song snd-win)
    (sleep 2)
    (goto-xy 26 12)
    (princ "premi qualsevol tecla per sortir")
    (get-key)
)

;; =========================================
;; Funció: "find-in-maze"
;; Cerca la primera occurencia de casella sobre maze[][]
;;
;; Paràmetres:
;;  - maze: doble llista de caselles
;;  - casella: casella a cercar
;;  - (opcional) i,j: indexos per a maze
;;  - (opcional) aux: emmagatzema la fila actual
;;
;; Retorn:
;;  - (i, j) amb la posició de la primera casella
;; ==========================================
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

;; =========================================
;; Funció: "get-in-maze"
;; Retorna maze[y][x]
;;
;; Paràmetres:
;;  - maze: doble llista de caselles
;;  - x, y: indexos
;;  - (opcional) aux: emmagatzema la fila actual
;;
;; Retorn:
;;  - casella = maze[y][x], nil si OoB
;; ==========================================
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

;; =========================================
;; Funció: "gen-keys"
;; Genera aleatoriament key_per_maze claus al laberint.
;; Aquests ha de ser totalment conex si no es vol generar un laberint
;; irresoluble.
;; Existeix la posibilitat no nula de generar dues claus a la mateixa casella (pero és molt poc problable)
;;
;; Paràmetres:
;;  - maze: doble llista de caselles
;;  - (opcional) n: nº claus a generar
;;  - (opcional) keys: llista amb les claus generades
;;
;; Retorn:
;;  - keys
;; ==========================================
(defun gen-keys(maze &optional (n key_per_maze) (keys '()))
    ;ja es un suplici resoldre un laberint de 50x50, un de 100x100...
    (let* ((x (random 100)) (y (random 100))
            (tile (get-in-maze maze x y))
        )
        (cond 
            ((eq tile Ccami)
                (cond 
                    ((> n 0)
                        (gen-keys maze (1- n) (cons (list x y) keys))
                    )
                    (t
                        keys
                    )
                )
            )
            (t 
                (gen-keys maze n keys)
            )
        )
    )
)

;; =========================================
;; Funció: "update-keys"
;; Elimina (de forma no destructiva) les claus que el jugador ha recollit (per proximitat)
;;
;; Paràmetres:
;;  - player: dades del jugador
;;  - extra: dades de les claus
;;  - (opcional) remain-keys: claus a comprovar
;;  - (opcional) aux-keys: claus no recollides
;;
;; Retorn:
;;  - extra actualitzat
;; ==========================================
(defun update-keys(player extra &optional (remain-keys (get extra 'keys)) (aux-keys '()))
    (cond 
        ((null remain-keys)
            (update-prop extra 'keys aux-keys)
        )
        (t
            (let* ((key (car remain-keys)) (kx (car key)) (ky (cadr key))
                    (pt (coord-to-tile (getx player) (gety player) nil)) (px (car pt)) (py (cadr pt))
                )
                (cond 
                    ((and (eq px kx) (eq py ky))
                        (play-song sfx-key)
                        (update-keys player extra (cdr remain-keys) aux-keys)
                    )
                    (t 
                        (update-keys player extra (cdr remain-keys) (cons key aux-keys))
                    )
                )
            )
        )
    )
)


;; =========================================
;; Funció: "update-steps"
;; Comprova si el jugador es troba en un nou tile
;;
;; Paràmetres:
;;  - player: dades del jugador
;;
;; Retorn:
;;  - (nou tile actual, valor a sumar a steps)
;;  - (newTileX, newTileY, 0/1)
;; ==========================================
(defun update-steps (player)
    (let* ((px (getx player)) (py (gety player))
            (xtile (round px TILESIZE)) (ytile (round (- py) TILESIZE))
            (oxtile (get player 'tilex)) (oytile (get player 'tiley))
          )
        (list xtile ytile (cond ((and (eq xtile oxtile) (eq ytile oytile)) 0) (t 1)))
    )
)


;; =========================================
;; Funció: "can-move-h/v"
;; Comprova si el jugador pot moure-se'n en un eix de coordenades.
;; Per lograr-ho mira si els tiles posteriors son paret o no

;; Paràmetres:
;;  - maze: dades del laberint
;;  - x,y: posicio del jugador
;;
;; Retorn:
;;  - extra actualitzat
;; ==========================================
(defun can-move-h (maze x y)
    (let ((xtile (floor x TILESIZE)) (ytile (floor (- y) TILESIZE)) (ytile2 (floor (+ (- y) (1- TILESIZE)) TILESIZE)))
        (not (or (eq (get-in-maze maze xtile ytile) Cparet) (eq (get-in-maze maze xtile ytile2) Cparet)))
    )
)
(defun can-move-v (maze x y)
    (let ((xtile (floor x TILESIZE)) (xtile2 (floor (+ x (1- TILESIZE)) TILESIZE)) (ytile (floor (- y) TILESIZE)))
        (not (or (eq (get-in-maze maze xtile ytile) Cparet) (eq (get-in-maze maze xtile2 ytile) Cparet)))
    )
)

;; =========================================
;; Funció: "new-player-pos"
;; Segons un input i la posició actual del jugador respecte al laberint
;; retorna la nova posició
;;
;; Paràmetres:
;;  - player: dades del jugador
;;  - maze: dades del laberint
;;  - input: direcció de moviment
;;
;; Retorn:
;;  - (newX, newY)
;; ==========================================
(defun new-player-pos (player maze input)
    (let* ((px (getx player)) (py (gety player)) (ps (get player 'speed)))
        (list
        ;new x 
        (cond ((and (eq input 'right) (can-move-h (get maze 'data) (+ px TILESIZE) py)) (+ px ps))
              ((and (eq input 'left) (can-move-h (get maze 'data) (- px ps) py)) (- px ps))
              (t px))
        ;new Y
        (cond ((and (eq input 'up) (can-move-v (get maze 'data) px (- py (- ps)))) (+ py ps))
              ((and (eq input 'down) (can-move-v (get maze 'data) px (+ py (- (1+ TILESIZE))))) (- py ps))
              (t py))
        )
    )
)


;; =========================================
;; Funció: "new-maze-pos"
;; Segons un input i la nova posició del jugador respecte al laberint
;; decideix si fa falta fer scroll del laberint, (si el jugador arriba a un borde de la pantalla)
;;
;; Paràmetres:
;;  - newpx, newpy: nova posició del jugador
;;  - maze: dades del laberint
;;  - input: direcció de moviment
;;
;; Retorn:
;;  - (newX, newY, repaint?)
;; ==========================================
(defun new-maze-pos(newpx newpy maze input)
(let* 
    (
        (r (and (eq input 'right) (> (+ newpx (getx maze)) SCREENPIXEL-M1))) (l (and (eq input 'left) (< (+ newpx (getx maze)) TILESIZE)))
        (u (and (eq input 'up) (> (+ newpy (gety maze)) (- TILESIZE)))) (d (and (eq input 'down) (< (+ newpy (gety maze)) (- SCREENPIXEL-M1))))
    )
    (list (cond ((eq r t) (- (getx maze) SCREENPIXEL-M1)) ((eq l t) (+ (getx maze) SCREENPIXEL-M1)) (t (getx maze)))
          (cond ((eq u t) (- (gety maze) SCREENPIXEL-M1)) ((eq d t) (+ (gety maze) SCREENPIXEL-M1))(t (gety maze)))
          (cond ((or r l u d) t) (t nil))
    )
)
)
































































;; =========================================
;; Funció: "easter-egg"
;; :)
;;
;; Paràmetres:
;;  - cap
;;
;; Retorn:
;;  - res
;; ==========================================
(defun easter-egg()
    (let ((val (random 5)))
        (cond 
            ((= val 0)
                (draw-tile "rickroll" 100 300)
                (color 0 0 0 255 255 255)
            )
            (t t)
        )
    )
)