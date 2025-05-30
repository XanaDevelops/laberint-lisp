;; =============================================================================
;; Funció: update-prop
;;  Crea un nou simbol amb les dades que conté l'anterior, però modificant un atribut d'aquest per
;;  la modificació desitgada
;;
;; Paràmetres:
;;   - c: simbol a actualitzar
;;   - prop: propietat a actualitzar
;;   - val: nou valor
;;   - (opcional) props: llistat original del simbol
;;
;; =============================================================================
(defun update-prop (c prop val &optional (props -1))
    (cond
        ((eq props -1)
            (update-prop (gensym "NEW-") prop val (symbol-plist c))
        )
        ((null props)
            c
        )
        (t
        (let ((p (car props)) (v (cadr props)))
            (cond
            ((eq prop p)
                (putprop c val p)
                (update-prop c prop val (cddr props))
            )
            (t
                (putprop c v p)
                (update-prop c prop val (cddr props))
            )
        )
        )
    )
)
)

(defun getx (a)
    (get a 'x)
)
(defun gety (a)
    (get a 'y)
)


