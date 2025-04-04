(load "VARSGLOBALS.lsp")
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
(defun-tco llegeix-intern (fp &optional r)
    (let ((c (read-char fp nil nil)))
        (cond
            ((null c) r)
        (t 
            (llegeix-intern fp (cons c r))
        )
        )
    )
)


(defun writeToFile (laberint file) 
  (let 
    ((stream 
       (open file :direction :output :if-exists :supersede :if-does-not-exist :create)
     ) 
    )
    (cond 
      (stream
       (writeRecursivly laberint stream)
       (close stream)
      )
    )
  )
)



(defun writeRecursivly (l stream) 
  (cond 
    ((null l) nil)
    (t
     (progn 

       (writeLine (car l) stream)
       (terpri stream)
       (writeRecursivly (cdr l) stream)
     )
    )
  )
)

(defun writeLine (line stream) 

  (cond 
    ((null line) nil)
    (t

     (progn 
       (write-char (convertirAChar (car line)) stream)
       (writeLine (cdr line) stream)
     )
    )
  )
)

(defun convertirAChar (valor) 

  (cond 
    ((equal valor sortida) Csortida)
    ((equal valor entrada) Centrada)
    ((equal valor paret) Cparet)
    ((equal valor cami) Ccami)
  )
)
