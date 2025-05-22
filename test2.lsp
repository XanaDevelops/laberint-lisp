(load 'sleep)
(defun hola(&optional (n 10))
    (cond
        ((= n 0)
            t
        )
        (t 
            (princ "hola\n")
            (sleep 1)
            (hola (1- n))
        )
    )
    
)
(defun main()
;(system "pythonw music.py --kill")
(princ "kill\n")
(system "python music.py --start")
(princ "start\n")
(system "python music.py --play 1 true")
(hola)
(system "python music.py --kill")

)