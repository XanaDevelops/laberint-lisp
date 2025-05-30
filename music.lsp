(load 'sleep)
;========================================================================
; Classe que maneja el so del joc.
; Permet iniciar i aturar el servidor de música, reproduir cançons
; aturar tota la música i finalitzar el servidor.
; Utilitza un script extern en Python per controlar la reproducció d'àudio.
;========================================================================

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
