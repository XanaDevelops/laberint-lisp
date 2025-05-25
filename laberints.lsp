(load 'CONST)
(load 'tco)
(load 'fitxer-io)
(load 'graphfx)
(load 'music)
(load 'draw_laberint)
(load 'draw_border)
(load 'game)
(load 'gen_laberint)

(defun-tco main(&optional (id 0))
    (stop-all)
    (play-song snd-menu t)
    (menu-loop id)
)
(defun-tco menu-loop(id &optional (aux nil))
    (color 255 255 255 0 0 0)
    (cls)
    (draw-tile "banner" 100 340)
    (draw-border 150 150 20 6)

    (menu-text id aux)
    
    (goto-xy 0 0)
        
    (menu-loop (update-menu id (get-key)))
)

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
            (main)
            
        )
        ((eq id id-explora)
            (goto-xy 20 16)
            (princ "         Escriu el nom de l'arxiu:")
            (goto-xy 20 17)
            (let ((steps (game-loop (read))))
                (main)
            )
        )
        (t 
            (goto-xy 20 16)
            (princ "IM A ERROR!!!!")) ;+1 punt a la nota final si pilles la referència :)
    )
)

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
                ((eq opt opt0)
                    (stop-all)
                    (color 0 0 0 255 255 255)
                    (cls)
                    (princ "ADEU\n\n")
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

(main)