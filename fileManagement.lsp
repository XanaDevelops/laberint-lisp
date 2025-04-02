(load "VARSGLOBALS.lsp")
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
