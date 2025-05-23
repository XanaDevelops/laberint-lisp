(load "VARSGLOBALS.lsp")
;; =============================================================================
;; Funció: 'llegeix-exp'
;; Llegeix i retorna el contingut d'un fitxer en forma d'expressions.
;;
;; Paràmetres:
;;   - nom: Nom del fitxer que es vol llegir.
;;
;; Retorn:
;;   - Una llista amb les expressions llegides del fitxer.
;; =============================================================================
(defun llegeix-exp (nom)
    (let* ((fp (open nom)) (e (llegeix-exp-intern fp)))
        (close fp)
        e
    )
)

;; =============================================================================
;; Funció: 'llegeix-exp-intern'
;; Funció recursiva per llegir les expressions d'un fitxer.
;;
;; Paràmetres:
;;   - fp: Flux de lectura del fitxer.
;;   - r(opcional): Llista acumuladora per les expressions llegides.
;;
;; Retorn:
;;   - Una llista amb les expressions.
;; =============================================================================
(defun-tco llegeix-exp-intern(fp &optional (r nil))
    (let ((e (read fp nil nil)))
        (cond
        ((null e)
            (reverse r)
        )
        (t
            (llegeix-exp-intern fp (cons e r))
        )
        )
    )
)
;; =============================================================================
;; Funció: 'llegeix'
;; Llegeix i retorna el contingut d'un fitxer caràcter per caràcter.
;;
;; Paràmetres:
;;   - nom: Nom del fitxer que es vol llegir.
;;
;; Retorn:
;;   - Una llista amb els caràcters llegits del fitxer.
;; =============================================================================
(defun llegeix (nom)
    (let* ((fp (open nom)) (contingut (llegeix-intern fp)))
        (close fp)
        contingut
    )
)
;; =============================================================================
;; Funció: 'llegeix-intern'
;; Funció recursiva per llegir caràcter per caràcter d'un fitxer.
;;
;; Paràmetres:
;;   - fp: Flux de lectura del fitxer.
;;   - r(opcional): Llista acumuladora de caràcters llegits.
;;
;; Retorn:
;;   - Una llista amb els caràcters.
;; =============================================================================
(defun-tco llegeix-intern (fp &optional r)
    (let ((c (read-char fp nil nil)))
        (cond
            ((null c) r)
        (t 
            (llegeix-intern fp (cons c r))
        )
        )
    )
)

;; =============================================================================
;; Funció: 'escriu-fitxer'
;; Escriu el contingut d'un laberint en un fitxer donat.
;;
;; Paràmetres:
;;   - laberint: Matriu que representa el laberint.
;;   - file: Nom del fitxer de destí.
;; =============================================================================
(defun escriu-fitxer (laberint file) 
  (let 
    ((stream 
       (open file :direction :output :if-exists :supersede :if-does-not-exist :create)
     ) 
    )
    (cond 
      (stream
       (escriu-rec laberint stream)
       (close stream)
      )
    )
  )
)


;; =============================================================================
;; Funció: 'escriu-rec'
;; Escriu recursivament cada línia del laberint en un fitxer.
;;
;; Paràmetres:
;;   - l: Llista de línies que representen el laberint.
;;   - stream: Flux de sortida cap al fitxer.
;;
;; =============================================================================
(defun escriu-rec (l stream) 
  (cond 
    ((null l) nil)
    (t
     (progn 

       (escriu-linia (car l) stream)
       (terpri stream)
       (escriu-rec (cdr l) stream)
     )
    )
  )
)

;; =============================================================================
;; Funció: 'escriu-linia'
;; Escriu recursivament els caràcters d'una línia en el fitxer.
;;
;; Paràmetres:
;;   - line: Llista de caràcters que formen una línia del laberint.
;;   - stream: Flux de sortida cap al fitxer.
;;
;; =============================================================================
(defun escriu-linia (line stream) 

  (cond 
    ((null line) nil)
    (t

     (progn 
       (write-char (convertir-char (car line)) stream)
       (escriu-linia (cdr line) stream)
     )
    )
  )
)

;; =============================================================================
;; Funció: 'convertir-char'
;; Converteix un valor de la matriu del laberint en un caràcter corresponent d'acord
;; amb la notació especificada per l'enunciat.
;;
;; Paràmetres:
;;   - valor: Valor d'una posició del laberint
;;
;; Retorn:
;;   - Caràcter corresponent al valor donat.
;; =============================================================================

(defun convertir-char (valor) 

  (cond 
    ((equal valor sortida) Csortida)
    ((equal valor entrada) Centrada)
    ((equal valor paret) Cparet)
    ((equal valor cami) Ccami)
  )
)
