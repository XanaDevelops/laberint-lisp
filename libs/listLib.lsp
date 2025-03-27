

(load "mathlib.lsp")
;basic list functions

; returns length of a giving list
(defun lengthL (l) 
  (cond 
    ((null (car l)) 0)
    (t (+ 1 (lengthL (cdr l))))
  )
)
; is e a member f list l?
(defun memberL (e l) 
  (cond 
    ((null (car l)) nil)
    ((equal e (car l)) t)
    (t (memberL e (cdr l)))
  )
)

; adds element e at the end f list l

(defun snoc (e l) 
  (cond 
    ((null l) (cons e nil))
    (t (cons (car l) (snoc e (cdr l))))
  )
)

; removes the first ocurrence of element e
(defun esborra (e l) 
  (cond 
    ((null l) nil)
    ((equal e (car l)) (cdr l))
    (t (cons (car l) (esborra e (cdr l))))
  )
)
; removes all the ocurrencies of element e
(defun esborra-tot (e l) 
  (cond 
    ((null l) nil)
    ((equal e (car l)) (esborra-tot e (cdr l)))
    (t (cons (car l) (esborra-tot e (cdr l))))
  )
)


; returns all elements of list l, excepte
; the last one
(defun rdc (l) 

  (cond 
    ((null (cdr l)) nil)
    (t (cons (car l) (rdc (cdr l))))
  )
)


; multiplies each element of l by e
(defun escala (e l) 

  (cond 
    ((null l) nil)

    (t (cons (* e (car l)) (escala e (cdr l))))
  )
)

; How many times does e appears in the list l? 
(defun repetitionsX (e l) 

  (cond 
    ((null l) 0)
    ((null (car l)) 0)
    (t
     (cond 
       ((equal e (car l)) (+ 1 (repetitionsX e (cdr l))))
       (t (repetitionsX e (cdr l)))
     )
    )
  )
)


; returns l(index)
(defun getElementI (index l &optional (tmp 0)) 

  (cond 
    ((null l) nil)
    (t
     (cond 
       ((= tmp index) (car l))
       (t (getElementI index (cdr l) (+ 1 tmp)))
     )
    )
  )
)

; reverses the list l
(defun reverseL (l) 

  (cond 
    ((null l) nil)
    (t (append (reverseL (cdr l)) (list (car l))))
  )
)

; appends l1 to l2 ... to ln --> elements = l1, l2...,ln
(defun appendL (&rest elements) 
  (cond 
    ((null elements) nil)
    ((null (cdr elements)) elements)
    (t (append (car elements) (car (appendL (cdr elements)))))
  )
)


; --------------- 2 ways to remove an element by his  index------------
; removes list (index)
(defun remove_Elem_ByIndex (index l &optional (current 0)) 
  (cond 

    ((null l) nil)
    ((= index current) (cdr l))
    (t (cons (car l) (remove_Elem_ByIndex index (cdr l) (+ 1 current))))
  )
)

; remove-n. Index starts at 1
(defun remove-n (n l) 

  (cond 
    ((null l) nil)
    ((= n 1) (cdr l))
    (t (cons (car l) (remove-n (- n 1) (cdr l))))
  )
)
;----------------------------------------------------------------------


; sorts the list l ascendingly
(defun ordena (l) 
  (cond 
    ((null l) nil)
    (t (cons (minim l) (ordena (esborra (minim l) l))))
  )
)


(defun aplana (llista) 
  (cond 
    ((null llista) nil)
    ((listp (car llista))
     (append (aplana (car llista)) (aplana (cdr llista)))
    )
    (t (cons (car llista) (aplana (cdr llista))))
  )
)

; counts the total number of atomic elements at the given list l
(defun atoms (l) 
  (cond 
    ((null l) 0)
    ((listp (car l)) (+ (atoms (car l)) (atoms (cdr l))))
    (t (+ 1 (atoms (cdr l))))
  )
)


; --------------- 2 possible ways to implement the function treureprimers------------

; returns the given list without the fist n elements
(defun treureprimers (n l) 
  (cond 
    ((= 1 n) (cdr l))
    (t (treureprimers (- n 1) (cdr l)))
  )
)

(defun treureprimers2 (n l) 
  (cond 
    ((= 0 n) l)
    (t (treureprimers2 (- n 1) (cdr l)))
  )
)
;----------------------------------------------------------------------

; returns the fisrt n elements of the list l
(defun tornaprimers (n l) 
  (cond 
    ((= n 0) nil)
    (t (cons (car l) (tornaprimers (- n 1) (cdr l))))
  )
)

; inserts the element e at list(index)
(defun insereix (e index l) 
  (cond 
    ((= 1 index) (cons e l))
    (t (cons (car l) (insereix e (- index 1) (cdr l))))
  )
)

; sets l(n) to e. n starts at 1, God have mercy on me! 
(defun canvia (n l e) 
  (cond 
    ((= n 1) (cons e (cdr l)))
    (t (cons (car l) (canvia (- n 1) (cdr l) e)))
  )
)


;inserts newE to elementL's right
(defun insereix-dreta (elementL newE l) 
  (cond 
    ((equal elementL (car l)) (aplana (list (car l) newE (cdr l))))
    (t (cons (car l) (insereix-dreta elementL newE (cdr l))))
  )
)

;inserts newE to elementL's left
(defun insereix-esquerra (elementL newE l) 
  (cond 
    ((equal elementL (car l)) (cons newE l))
    (t (cons (car l) (insereix-esquerra elementL newE (cdr l))))
  )
)



; calculats total sum of the elements in odd positions 
(defun sumarparells (llista &optional (i 1)) 
  (cond 
    ((null llista) 0)
    ((= 0 (mod i 2))
     (+ (car llista) 
        (sumarparells (cdr llista) (+ i 1))
     )
    )
    (t (sumarparells (cdr llista) (+ i 1)))
  )
)

; calculats total sum of the elements in even positions 
(defun sumarsenars (llista &optional (i 1)) 
  (cond 
    ((null llista) 0)
    ((= 0 (mod i 2))
     (sumarsenars (cdr llista) (+ i 1))
    )
    (t (+ (car llista) (sumarsenars (cdr llista) (+ i 1))))
  )
)

;returns index of the element e
(defun posicio (e llista) 
  (cond 
    ((null llista) (- 1))
    ((equal e (car llista)) 1)
    (t
     (let 
       ((res (posicio e (cdr llista))))
       (if (= res (- 1)) (- 1) (+ res 1))
     )
    )
  )
)



; Right shift of the list's elements
; a b c --> c b a 
(defun rotardreta (llista) 
  (cond 
    ((null (cdr llista)) (car llista))
    (t)
  )
  (cons (getElementI (- (lengthL llista) 1) llista) (rdc llista))
)

(defun right-shift-list (lst) 
  (if (or (null lst) (null (cdr lst))) 
    lst
    (cons (car (last lst)) (butlast lst))
  )
)

; Left shift of the list's elements
(defun rotaresquerra (llista) 

  (append (cdr llista) (list (car llista)))
)

; Create a list with n repetitions of the element e
(defun replicar (n e) 
  (cond 
    ((= 0 n) nil)
    (t (cons e (replicar (- n 1) e)))
  )
)

; extract elements of a list with the form: (3 a 5 t 8 b..)
(defun descomprimir (llista) 
  (cond 
    ((null llista) nil)
    (t
     (append (replicar (car llista) (cadr llista)) 
             (descomprimir (cddr llista))
     )
    )
  )
)
; compress the list.
; (a a b c c d d d) --> (3 a 1 b 2 c 3 d)
;-----------------------Using intermediate parameter
(defun comprimir (llista &optional (current 1)) 
  (cond 
    ((null (cdr llista)) (list current (car llista)))
    ((equal (car llista) (cadr llista)) ; llista(i).equals(llista(i+1))
     (comprimir (cdr llista) (+ 1 current))
    )
    (t (cons current (cons (car llista) (comprimir (cdr llista) 1))))
  )
)

;-----------------------Using local variable


  ;Given two lists, write the elements of the second one indexed by the first one
(defun index (indices llista) 

  (cond 
    ((null indices) nil)
    ; Get the element at the 1-based index (subtract 1 for 0-based lists).
    (t
     (cons (getElementI (- (car indices) 1) llista) (index (cdr indices) llista))
    )
  )
)
(defun invertirtot (llista) 
  (cond 
    ((null llista) nil)
    ((listp (car llista))
     (append (invertirtot (cdr llista)) (invertirtot (car llista)))
    )
    (t (append (invertirtot (cdr llista)) (list (car llista))))
  )
)
(defun borrarl (x llista) 

  (cond 


    ((null llista) nil)
    ((listp (car llista))
     (cons (borrarl x (car llista)) (borrarl x (cdr llista)))
    )
    ((equal x (car llista)) (borrarl x (cdr llista)))
    (t (cons (car llista) (borrarl x (cdr llista))))
  )
)


  ; converts decimal value to a binary one
(defun binari (x) 

  (cond 
    ((< x 2) (list x))
    (t (append (binari (div x 2)) (list (mod x 2))))
  )
)

  ; converts binary value to decimal value.
  ; x is expressed as a list
(defun decimal (x) 
  (cond 
    ((null x) 0)
    (t
     (+ (* (car x) (expM 2 (- (lengthL x) 1))) 
        (decimal (cdr x))
     )
    )
  )
)
; esborra un element d'una llista si es compleix una condició determinada
(defun esborra-si (condition llista) 

  (cond 

    ((null llista) nil)
    ((funcall condition (car llista)) (esborra-si condition (cdr llista)))
    (t (cons (car llista) (esborra-si condition (cdr llista))))
  )
)

;retorna una llista amb els elements de les coes de totes les llistes (els seus cdr).
(defun afegeix-coes (llista) 
  (cond 
    ((null llista) nil)
    (t (mapcar (lambda (l) (cdr l)) llista))
  )
)

(defun pescalar (l1 l2) 
  (cond 
    ((null l1) 0)
    (t (+ (* (car l1) (car l2)) (pescalar (cdr l1) (cdr l2))))
  )
)
(defun suma1 (x) 
  (+ x 1)
)
; fa el mateix amb mapcar, però amb només una llista
(defun meu-mapcar (f llista) 

  (cond 
    ((null llista) nil)
    (t (cons (funcall f (car llista)) (meu-mapcar f (cdr llista))))
  )
)

; segons versió de la funció producte escalar, usant expressió lambda

(defun pescalar2 (llista) 
  (apply '+ (mapcar (lambda (l) (apply '* l)) (transposta llista)))
)


;(defun transposta (l) 
 ; (apply 'mapcar #'list l)
;)

(defun transposta (l) 
  (cond 
    ((null (car l)) nil)
    (t (cons (mapcar 'car l) (transposta (mapcar 'cdr l))))
  )
)
