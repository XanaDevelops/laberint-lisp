(load 'tco)

(setq paret #\#)
(setq cami #\.)
(setq entrada #\e)
(setq sortida #\s)
(setq newline #\NewLine)

(setq mazepos '(64 320))

(setq player-speed 2)

(load 'fitxer-io)
(load 'graphfx)
(load 'user-input)
(load 'prop-util)

(setq dbg nil)

(defun-tco read-maze (fn &optional (maze nil) (r nil) (s nil))
    (cond
    ((and (null maze) (null r))
        (read-maze fn (llegeix fn))
    )
    (t 
    (let ((c (car maze)))
        (cond
        ((null c)
            (cons s r)
        )
        ((eq c newline)
            (read-maze fn (cdr maze) (cond ((null r) (cond ((null (car s)) r) (t (list s))))
                                            (t (cons s r))
                                    )
                            nil
            )
        )
        (t
            (read-maze fn (cdr maze) r (cons c s))
        )
        )
    ))
    )
)


(setq q (read-maze "test.txt"))
(pprint q)