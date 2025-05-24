(load 'sleep)

(defun start-server()
    (system "python music.py --start")
)   

(defun play-song (id &optional (n nil))
    (system (format nil "python music.py --play ~D ~A" id
                  (cond (n "true")
                        (t "false"))))
)

(defun stop-all()
    (system "python music.py --stop")
)

(defun kill-server()
    (system "python music.py --kill")
)

;(kill-server)
(start-server)