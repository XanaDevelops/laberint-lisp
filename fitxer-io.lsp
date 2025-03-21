(defun llegeix-exp (nom)
    (let* ((fp (open nom)) (e (llegeix-exp-intern fp)))
        (close fp)
        e
    )
)

(defun-tco llegeix-exp-intern(fp &optional (r nil))
    (let ((e (read fp nil nil)))
        (cond
        ((null e)
            (reverse r)
        )
        (t
            (llegeix-exp-intern fp (cons e r))
        )
        )
    )
)

(defun llegeix (nom)
    (let* ((fp (open nom)) (contingut (llegeix-intern fp)))
        (close fp)
        contingut
    )
)
(defun llegeix-intern (fp)
    (let ((c (read-char fp nil nil)))
        (cond
            ((null c) '())
        (t 
            (cons c (llegeix-intern fp))
        )
        )
    )
)