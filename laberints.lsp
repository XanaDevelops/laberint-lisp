;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>HEADER>>>>>>>>>>>>>>>>>>>>>>>>>
; Autors: Daniel García Vázquez, Khaoula Ikkene.
; Data de començament: 20 Març, 2025
; Dara d'entrega : 03 Juny, 2025
; Assignatura: 21721 - Llenguatges de Programació. 
; Grup: PF1-13
; Professors: Antoni Oliver Tomàs, Francesc Xavier Gayà Morey
; Convocatòria Ordinària
;
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

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

;========================================================================
; Classe principal del joc.
; És la classe que s'ha de cridar des del programa per iniciar el joc.
; Carrega el menú principal, comunica a l'usuari la informació necessària
; segons l'opció triada, i crida als mètodes corresponents per 
; generar/explorar laberints, mostrar estadístiques, actualitzar dades, etc.
;========================================================================

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
    (start-server) ; per si de cas
    (play-song snd-menu t)
    (color 255 255 255 0 0 0)

    
    
    (menu-loop id optRepaint)

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
    (cond 
        ((eq aux optRepaint)
            (cls)
            (draw-tile-nocheck "banner" 0 375)
        )
        (t t)
    )
    (clear-text 19 15 41 7)

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
                        "3) BTR"
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
                (genera path (cond ((eq aux opt1) DFS)((eq aux opt2) PRIM)((eq aux opt3) BTR)))
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
                (clear-text 26 2 29 18)
                (show-text (append (list "       ESTADISTIQUES"
                                (format nil "~S" laberint) ""
                                )
                                (mapcar (lambda (x) (format nil "~S: ~S passos" (jugador-nom x) (jugador-passos x))) (capar-a stats 10))
                                (list "" "Premi una tecla per sortir") 
                            )
                27 3)
                (draw-border 204 350 14 17)
                

                (get-key)
                (menu-loop 0 optRepaint)
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


(setq *random-state* (make-random-state t)) ;revisar seeds
(main)
