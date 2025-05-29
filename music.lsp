(load 'sleep)

(defun start-server()
    (system "python music.py server")
)   

(defun play-song (id &optional (n nil))
    (system (format nil "pythonw music.py play -i ~D ~A" id
                  (cond (n "-l")
                        (t ""))))
)

(defun stop-all()
    (system "pythonw music.py stop")
)

(defun kill-server()
    (system "pythonw music.py kill")
)

;(kill-server)
(stop-all)
(cond 
    (do-music (start-server))
)
