(defun llegeix-exp (nom)
    (let* ((fp (open nom)) (e (llegeix-exp-intern fp)))
        (close fp)
        e
    )
)

(defun llegeix-exp-intern(fp)
    (let ((e (read fp nil nil)))
        (cond
        ((null e)
            nil
        )
        (t
            (cons e (llegeix-exp-intern fp))
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