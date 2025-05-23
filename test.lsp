(setq TILESIZE 16)

(defun coord-to-tile(x y &optional (exact t))
    (mapcar (lambda (x) (apply (cond ((eq exact t) 'floor) (t 'round)) x)) (list (list x TILESIZE) (list (- y) TILESIZE)))
)

