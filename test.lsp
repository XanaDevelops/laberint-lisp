(setq TILESIZE 16)

(defmacro leval-str (string)
  ;; read-from-string se ejecuta al expandir la macro
  (let ((form (read-from-string string)))
    `(funcall (lambda () ,form))))

(let ((a 3))
(print (eval (read)))
)