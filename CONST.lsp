(setq dbg nil)

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


(setq FILES (- 50 2)) ; 2 de files extra de parets que s'afegeixen
(setq COLUMNES (- 50 2))
(setq iniciEntrada (list (list 0 0) (list (- FILES 1) 0)))

(setq displacements '((1 0) (-1 0) (0 1) (0 -1)))

(setq Cparet #\#)
(setq Ccami #\.)
(setq Centrada #\e)
(setq Csortida #\s)
(setq newline #\NewLine)

(setq laberintOutputFile "laberintGenerat.txt")
(setq estadistiquesFile "estadistiques.txt")
(setq ID 0)

(setq key_per_maze 3)

(setq mazepos '(64 332))
(setq mmappos (list 400 (- (cadr mazepos) 4)))

(setq TILESIZE 16)
(setq TILE_W 16)
(setq TILE_H 16)
(setq SCREENPIXEL-M1 (* TILESIZE (1- TILESIZE)))
(setq SCREEN_W (+ (car mazepos) (* TILESIZE TILE_W)))
(setq SCREEN_H (+ (cadr mazepos) (* TILESIZE (- -1 TILE_H))))

(setq MM_TILESIZE 4)

(setq mm-paret 0)
(setq mm-cami 1)
(setq mm-recorregut 2)
(setq mm-player 3)