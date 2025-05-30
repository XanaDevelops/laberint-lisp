
;========================================================================
; Aquesta classe obté les tecles que pitja el jugador durant el joc
;========================================================================


(defun user-input()
    (goto-xy 0 0)
    (let ((key (get-key)))
        (cond
        ((some (lambda (x) (= x key)) '(87 119 328))
            'up
        )
        ((some (lambda (x) (= x key)) '(83 115 336))
            'down
        )
        ((some (lambda (x) (= x key)) '(65 97 331))
            'left
        )
        ((some (lambda (x) (= x key)) '(68 100 333))
            'right
        )
        ((some (lambda (x) (= x key)) '(80 112)) ;Permet fer un eval dins el joc
            'admin
        )
        ((some (lambda (x) (= x key)) '(82 114)) ;Repinta la pantalla dins el joc
            'redraw
        )
        ((= key 27)
            'esq
        )

        (t
            nil
        )
        )
    )
)
