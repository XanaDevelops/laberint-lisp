(load 'CONST)
(load 'tco)
(load 'fitxer-io)
(load 'graphfx)
(load 'music)
(load 'draw_laberint)
(load 'draw_border)
(load 'game)

(load 'partida)
(load 'gen_laberint)

;; ========================================
;; Funció: "main"
;; Funció d'entrada de laberints.lsp
;; 
;; Paràmetres:
;;  - (opcional) id: override del id de menú a visualitzar
;;
;; Retorn:
;;  - res
;; ======================================
(defun-tco main(&optional (id 0))
    (stop-all)
    (play-song snd-menu t)
    (menu-loop id)
)

;; =========================================
;; Funció: "menu-loop"
;; El loop del menu
;;
;; Paràmetres:
;;  - id: id del menu actual
;;  - (opcional) aux: informació extra auxiliar
;;
;; Retorn:
;;  - res
;; ==========================================
(defun-tco menu-loop(id &optional (aux nil))
    (color 255 255 255 0 0 0)
    (cls)
    (draw-tile-nocheck "banner" 0 375)
    (clear-text 20 15 40 7)

    (draw-border 150 150 20 6)

    (menu-text id aux)
    
    (goto-xy 0 0)
        
    (menu-loop (update-menu id (get-key)))
)

;; =========================================
;; Funció: "menu-text"
;; Mostra el text (o funcionalitat extra) del menú concret
;;
;; Paràmetres:
;;  - id: id del menu actual
;;  - (opcional) aux: informació extra auxiliar
;;
;; Retorn:
;;  - res
;; ==========================================
(defun menu-text(id &optional (aux nil))
    (color 255 255 255 0 0 0)
    (cond
        ((eq id id-menu)
            (show-text '("1) Generar laberint"
                            "2) Explorar laberint"
                            "3) Veure records"
                            ""
                            "0) Sortir"
            ))

        )
        ((eq id id-gen)
            (show-text '("              Tria l'algorisme"
                        "1) DFS"
                        "2) PRIM"
                        "3) RDV"
                        "0) Tornar enrera"
            ))
        )
        ((eq id id-gen-name)
            (goto-xy 20 16)
            (princ "         Escriu el nom de l'arxiu:")
            (goto-xy 20 17)
            (let ((path (read)))
                (goto-xy 20 18)
                (princ "Generant...")
                (genera path (cond ((eq aux opt1) DFS)((eq aux opt2) PRIM)((eq aux opt3) RDV)))
            )
            (goto-xy 20 19)
            (princ "OK")
            (menu-loop id-menu)
            
        )
        ((eq id id-explora)
            (goto-xy 20 16)
            (princ "         Escriu el nom de l'arxiu:")
            (goto-xy 20 17)
            (let ((jugador (explora (read))))
                (cond
                    ((not (null jugador))
                        (guardarJugador jugador)
                    )
                )
                
                (main) ;retornam al menu principal per reestablir la musica
            )
        )
        ((eq id id-stats)
            (goto-xy 20 16)
            (princ "         Escriu el nom de l'arxiu:")
            (goto-xy 20 17)
            (let* ((laberint (read)) (stats (getLlistaClassificacions (make-jugador :laberint laberint))))
                (clear-text 26 3 29 17)
                (show-text (append (list "       ESTADISTIQUES"
                                (format nil "~S" laberint) ""
                                )
                                (mapcar (lambda (x) (format nil "~S: ~S passos" (jugador-nom x) (jugador-passos x))) (capar-a stats 10))
                                (list "" "Premi una tecla per sortir") 
                            )
                27 3)
                (draw-border 204 350 14 17)
                

                (get-key)
                (menu-loop 0)
            )
        )
        (t 
            (goto-xy 20 16)
            (princ "IM A ERROR!!!!")) ;+1 punt a la nota final si pilles la referència :)
    )
)

;; =========================================
;; Funció: "update-menu"
;; Actualitza el menu segons l'entrada de l'usuari
;; Segons el menu (id) va comprovant amb opt les diferents entrades del menu
;; i retorna el id del proxim menú
;;
;; Paràmetres:
;;  - id: id del menu actual
;;  - opt: opció triada per l'usuari
;;
;; Retorn:
;;  - nou_id: el nou id per al menú
;; ==========================================
(defun update-menu(id opt)
    (cond
        ((eq id id-menu)
            (cond 
                ((eq opt opt1)
                    id-gen
                )
                ((eq opt opt2)
                    id-explora   
                )
                ((eq opt opt3)
                    id-stats
                )
                ((eq opt opt0)
                    (stop-all)
                    (color 0 0 0 255 255 255)
                    (cls)
                    (princ "ADEU\n\n")
                    (easter-egg)
                    (kill-server)
                    (top-level)
                )
                (t id-menu)
            )
        )
        ((eq id id-gen)
            (cond 
                ((or (eq opt opt1) (eq opt opt2) (eq opt opt3))
                    (menu-loop id-gen-name opt)
                    id-menu
                )
                ((eq opt opt0)
                    id-menu
                )
                (t id-gen)
            )
        )
        ((eq id id-explora)
            id-menu

        )
        (t id-menu)
    )
)

;; =========================================
;; Funció: "show-text"
;; Mostra el text en una columna a partir de les coordenades (de text)
;;
;; Paràmetres:
;;  - text: llista amb les cadenes de text a mostrar
;;  - (opcional) x,y: coordenades (text) on començar
;;
;; Retorn:
;;  - res
;; ==========================================
(defun show-text(text &optional (x 20) (y 16))
    (cond 
        ((null text)
            t
        )
        (t 
            (goto-xy x y)
            (princ (car text))
            (show-text (cdr text) x (1+ y))
        )
    )
)


;; =========================================
;; Funció: "clear-text"
;; Borra una secció rectangular de la pantalla amb el color de fons gràcies
;; al " "
;;
;; Paràmetres:
;;  - x,y: coordenades text de l'esquina superior del rectangle
;;  - w,h: tamany del rectangle
;;
;; Retorn:
;;  - res
;; ==========================================
(defun clear-text(x y w h)
    ;; =========================================
    ;; Funció: "gen-white"
    ;; Crea " "*n 
    ;;
    ;; Paràmetres:
    ;;  - n: nº d'espais
    ;;
    ;; Retorn:
    ;;  - " "*n
    ;; ==========================================
    (defun gen-white(n)
        (cond 
            ((= n 0) "")
            (t (strcat " " (gen-white (1- n))))
        )
    )
    (cond 
        ((= h 0)
            t
        )
        (t 
            (goto-xy x y)
            (princ (gen-white w))
            (clear-text x (1+ y) w (1- h))
        )
    )
)

(setq *random-state* (make-random-state t)) ;revisar seeds
(main)
