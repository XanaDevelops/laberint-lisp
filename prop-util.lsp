(defun update-prop (c prop val &optional (props -1))
    (cond
        ((eq props -1)
            (update-prop 'new prop val (symbol-plist c))
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

(putprop 'player 30 'z)
(putprop 'player 10 'x)
(putprop 'player 15 'y)
(print (symbol-plist 'player))
(print (symbol-plist (update-prop 'player 'x 20)))

