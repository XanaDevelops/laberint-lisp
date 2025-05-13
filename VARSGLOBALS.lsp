(setq paret 'paret)
(setq entrada 'entrada)
(setq cami 'cami)
(setq sortida 'sortida)
; configurables
(setq DFS 'DFS)
(setq PRIM 'PRIM)
(setq RDV 'RDV) ; recursive division
(setq algorismeGeneracio RDV)
(setq HORIZONTAL 1)
(setq VERTICAL 0)


(setq FILES (- 20 2)) ; 2 de files extra de parets que s'afegeixen
(setq COLUMNES (- 10 2))
(setq iniciEntrada (list (list 1 1) (list FILES 1)))

(setq displacements '((1 0) (-1 0) (0 1) (0 -1)))

(setq Cparet #\#)
(setq Ccami #\.)
(setq Centrada #\e)
(setq Csortida #\s)
(setq laberintOutputFile "laberintGenerat.txt")
(setq estadistiquesFile "estadistiques.txt")
(setq ID 0)
; (setq salt newline)