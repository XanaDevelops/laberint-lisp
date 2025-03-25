(setq left 0)
(setq right 1)
(setq up 2)
(setq down 3)

(defun user-input()
    (let ((get-key) key)
        (cond
        ((some '(lambda (x) (= x key)) '(87) '(119) '(328))
            'up
        )
        (t
            'down
        )
        )
    )
)

(user-input)