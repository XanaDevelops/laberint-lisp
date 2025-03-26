(setq left 0)
(setq right 1)
(setq up 2)
(setq down 3)
(setq esc -1)

;optimitzar recursio +3?
(defun user-input()
    (let ((key (get-key)))
        (cond
        ((some (lambda (x) (= x key)) '(87 119 328))
            'up
        )
        ((some (lambda (x) (= x key)) '(83 115 336))
            'down
        )
        ((some (lambda (x) (= x key)) '(65 97 331))
            'left
        )
        ((some (lambda (x) (= x key)) '(68 100 333))
            'right
        )
        ((= key 27)
            'esq
        )
        )
    )
)
