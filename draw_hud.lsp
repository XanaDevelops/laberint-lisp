;========================================================================
; Classe encarregada de gestionar el HUD (Head-Up Display) del joc.
; Aquesta classe inclou funcionalitats per dibuixar la interfície 
; gràfica que mostra informació rellevant al jugador durant la partida,
; com ara el nombre de claus restants per recollir i els passos realitzats.
;========================================================================

(load 'CONST)
(load 'draw_border)

;; =============================================================================
;; Funció: draw-hud
;;  Dibuixa el hud a la pantalla.
;;  Aquest mostra les claus restants a recollir i el numero de passos fets
;;
;; Paràmetres:
;;   - extra: dades extra
;;   - steps: nº de passos
;; Retorn:
;;   - res
;; =============================================================================
(defun draw-hud (extra steps)
    (color 255 255 255 0 0 0)
    (goto-xy 50 17)
    (princ "                       ")
    (goto-xy 50 17)
    (format t "claus restants: ")
    (goto-xy 50 18)
    (format t "passes realitzades: ~D" steps)
    (draw-hud-keys (length (get extra 'keys)))
    (draw-border (- (car mmappos) 12) (- (cadr mmappos) (* TILESIZE 13) -0) 13 3)
)

;; =============================================================================
;; Funció: draw-hud-keys
;;  Dibuixa les claus restants al hud.
;;
;; Paràmetres:
;;   - remain: nº de claus restants
;;   - (opcional) x,y: coordenades per a dibuixar la següent clau
;; Retorn:
;;   - res
;; =============================================================================
(defun draw-hud-keys(remain &optional (x 520) (y 120))
    (cond
    ((= remain 0) t)
    (t 
        (draw-tile-nocheck "llave" x y)
        (draw-hud-keys (1- remain) (+ x TILESIZE) y)
    )
    )
)
