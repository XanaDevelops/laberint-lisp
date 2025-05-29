(setq paret 'paret)
(setq entrada 'entrada)
(setq cami 'cami)
(setq sortida 'sortida)
; configurables
(setq DFS 'DFS)
(setq PRIM 'PRIM)
(setq RDV 'RDV) ; recursive division
(setq algorisme-generacio RDV)
(setq HORIZONTAL 1)
(setq VERTICAL 0)


(setq FILES (- 25 2)) ; 2 de files extra de parets que s'afegeixen
(setq COLUMNES (- 25 2))
(setq iniciEntrada (list (list 0 0) (list (- FILES 1) 0)))

(setq displacements '((1 0) (-1 0) (0 1) (0 -1)))
(setq dreta-baix '((1 0) (0 1)))
(setq Cparet #\#)
(setq Ccami #\.)
(setq Centrada #\e)
(setq Csortida #\s)
(setq laberintOutputFile "laberintGenerat.txt")
(setq estadistiquesFile "estadistiques.txt")
(setq ID 0)
; (setq salt newline)