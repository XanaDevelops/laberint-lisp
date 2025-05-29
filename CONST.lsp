(setq dbg nil)

(setq paret 'paret)
(setq entrada 'entrada)
(setq cami 'cami)
(setq sortida 'sortida)
; configurables
(setq DFS 'DFS)
(setq PRIM 'PRIM)
(setq BTR 'BTR) ; recursive division
(setq algorisme-generacio DFS)

(setq FILES (- 66 2)) ; 2 de files extra de parets que s'afegeixen
(setq COLUMNES (- 66 2))
(setq iniciEntrada (list (list 0 0) (list (- FILES 1) 0)))

(setq displacements '((1 0) (-1 0) (0 1) (0 -1)))
(setq dreta-baix '((1 0) (0 1)))

(setq Cparet #\#)
(setq Ccami #\.)
(setq Centrada #\e)
(setq Csortida #\s)
(setq newline #\NewLine)

(setq laberintOutputFile "laberintGenerat.txt")
(setq estadistiquesFile "estadistiques.txt")

(setq left 0)
(setq right 1)
(setq up 2)
(setq down 3)
(setq admin 666)
(setq esc -1)


(setq key_per_maze 3)

(setq mazepos '(64 348))
(setq mmappos (list 400 (- (cadr mazepos) 4)))

(setq TILESIZE 16)
(setq TILE_W 16)
(setq TILE_H 16)
(setq SCREENPIXEL-M1 (* TILESIZE (1- TILESIZE)))
(setq SCREEN_W (+ (car mazepos) (* TILESIZE TILE_W)))
(setq SCREEN_H (+ (cadr mazepos) (* TILESIZE (- -1 TILE_H))))

;(setq MM_TILESIZE 2) ;permet mostrar 100x100
(setq MM_TILESIZE 3) ;permet mostrar aprox 66x66
;(setq MM_TILESIZE 4) ;permet mostrar 50x50

(setq mm-paret 0)
(setq mm-cami 1)
(setq mm-recorregut 2)
(setq mm-player 3)

(setq snd-fw 1)
(setq snd-water 2)
(setq snd-win 3)
(setq snd-menu 5)
(setq sfx-key 4)

(setq do-music t)

(setq id-menu 0)
(setq id-explora 1)
(setq id-gen 2)
(setq id-gen-name 3)
(setq id-stats 4)

(setq opt0 48)
(setq opt1 49)
(setq opt2 50)
(setq opt3 51)
(setq opt4 52)

(setq optRepaint 1)