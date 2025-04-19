(setq paret 'paret)
(setq entrada 'entrada)
(setq cami 'cami)
(setq sortida 'sortida)
; configurables
(setq DFS 'DFS)
(setq PRIM 'PRIM)
(setq RDV 'RDV) ; recursive division
(setq algorismeGeneracio PRIM)
(setq HORIZONTAL 1)
(setq VERTICAL 0)


(setq FILES 30)
(setq COLUMNES 50)
(setq iniciEntrada (list (list 0 0) (list (- FILES 1) 0)))

(setq displacements '((1 0) (-1 0) (0 1) (0 -1)))

(setq Cparet #\#)
(setq Ccami #\.)
(setq Centrada #\e)
(setq Csortida #\s)
(setq OutputFileName "laberintGenerat.txt")
; (setq salt newline)