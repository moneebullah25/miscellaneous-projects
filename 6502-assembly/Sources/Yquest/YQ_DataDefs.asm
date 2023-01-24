
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Starting values for player object
PlayerObjectBak:
	db 0
	ifdef PlayerHsprite
		db PlayerHsprite
	else
		db 0
	endif 
	
	db ScreenObjWidth/2,ScreenObjHeight/2,0,0,0,0



ObjectByteSize equ 8	;8 bytes per object
;SpriteAddress,Xpos,Ypos,Xpos2,Ypos2,Xaccel,Yaccel,Program,Collision
Enemies equ 40			;40 Enemies onscreen MAX (including Mines / Crystals)
BulletCount equ 8		;Bullets (Player + Enemy)
OnscreenCrystals equ 5 	;Crystals onscreen (on collection more respawn up to level count)


LevelMap:			;Pointers to level data
	dw Level1
	dw Level2
	dw Level3
	dw Level4
	dw Level5
	dw Level6
	dw Level7
	dw Level8
	dw Level9
	dw Level10
	dw Level11
	dw Level12
	dw Level13
	dw Level14
	dw Level15
	dw Level16
	
Level1:	 
;Header
	db 3,0			;CrystalCount , unused
;Object List - (Type,Count)
	db 1,3			;Slow purplee
	db 2,3			;Crystals
	db 4,5			;Mines
	db 0,255		;End of list
;Level Definition Ends

Level2: ;(Type,Count)
	db 5,0			;CrystalCount , unused
	db 3,1			;FastBlue
	db 1,3			;Slow purplee
	db 2,5			;Crystals
	db 4,5			;Mines
	db 0,255		;End of list

Level3: ;(Type,Count)
	db 8,0			;CrystalCount , unused
	db 3,1			;FastBlue
	db 1,3			;Slow purplee
	db 2,5			;Crystals
	db 4,7			;Mines
	db 0,255		;End of list

Level4: ;(Type,Count)
	db 8,0			;CrystalCount , unused
	db 3,2			;FastBlue
	db 1,2			;Slow purplee
	db 5,1			;Toothy
	db 2,5			;Crystals
	db 4,7			;Mines
	db 0,255		;End of list

Level5: ;(Type,Count)
	db 10,0			;CrystalCount , unused
	db 3,1			;FastBlue
	db 1,1			;Slow purplee
	db 5,1			;Toothy
	db 6,3			;Wiggle
	db 2,5			;Crystals
	db 4,10			;Mines
	db 0,255		;End of list

Level6: ;(Type,Count)
	db 10,0			;CrystalCount , unused
	db 3,1			;FastBlue
	db 7,1			;Jelly
	db 5,1			;Toothy
	db 1,2			;Slow purplee
	db 6,2			;Wiggle
	db 2,5			;Crystals
	db 4,10			;Mines
	db 0,255		;End of list


Level7: ;(Type,Count)
	db 15,0			;CrystalCount , unused
	db 3,1			;FastBlue
	db 7,1			;Jelly
	db 5,1			;Toothy
	db 1,2			;Slow purplee
	db 6,1			;Wiggle
	db 8,1			;Moody
	db 2,5			;Crystals
	db 4,13			;Mines
	db 0,255		;End of list


Level8: ;(Type,Count)
	db 15,0			;CrystalCount , unused
	db 3,1			;FastBlue
	db 7,1			;Jelly
	db 5,1			;Toothy
	db 1,1			;Slow purplee
	db 6,1			;Wiggle
	db 8,1			;Moody
	db 9,1			;Vbar
	db 2,5			;Crystals
	db 4,13			;Mines
	db 0,255		;End of list

Level9: ;(Type,Count)
	db 20,0			;CrystalCount , unused
	db 3,1			;FastBlue
	db 7,1			;Jelly
	db 5,1			;Toothy
	db 1,1			;Slow purplee
	db 10,1			;Three Eyes
	db 8,1			;Moody
	db 9,1			;Vbar
	db 2,5			;Crystals
	db 4,13			;Mines
	db 0,255		;End of list

Level10: ;(Type,Count)
	db 20,0			;CrystalCount , unused
	db 11,1			;JoinedDuo
	db 7,1			;Jelly
	db 5,1			;Toothy
	db 1,1			;Slow purplee
	db 10,1			;Three Eyes
	db 8,1			;Moody
	db 9,1			;Vbar
	db 2,5			;Crystals
	db 4,13			;Mines
	db 0,255		;End of list


Level11: ;(Type,Count)
	db 25,0			;CrystalCount , unused
	db 11,1			;JoinedDuo
	db 7,1			;Jelly
	db 5,1			;Toothy
	db 1,3			;Slow purplee
	db 10,1			;Three Eyes
	db 8,1			;Moody
	db 6,1			;Wiggle
	db 9,1			;Vbar
	db 2,5			;Crystals
	db 4,15			;Mines
	db 0,255		;End of list

Level12: ;(Type,Count)
	db 25,0			;CrystalCount , unused
	db 11,1			;JoinedDuo
	db 7,1			;Jelly
	db 1,3			;Slow purplee
	db 10,1			;Three Eyes
	db 8,1			;Moody
	db 9,1			;Vbar
	db 6,1			;Wiggle
	db 2,5			;Crystals
	db 4,15			;Mines
	db 12,1			;Clouder
	db 0,255		;End of list



Level13: ;(Type,Count)
	db 25,0			;CrystalCount , unused
	db 11,1			;JoinedDuo
	db 7,1			;Jelly
	db 1,3			;Slow purplee
	db 10,1			;Three Eyes
	db 8,1			;Moody
	db 9,1			;Vbar
	db 6,1			;Wiggle
	db 2,5			;Crystals
	db 4,15			;Mines
	db 12,2			;Clouder
	db 0,255		;End of list

Level14: ;(Type,Count)
	db 25,0			;CrystalCount , unused
	db 11,1			;JoinedDuo
	db 7,1			;Jelly
	db 1,2			;Slow purplee
	db 10,1			;Three Eyes
	db 8,1			;Moody
	db 9,1			;Vbar
	db 6,1			;Wiggle
	db 2,5			;Crystals
	db 4,17			;Mines
	db 12,3			;Clouder
	db 0,255		;End of list


Level15: ;(Type,Count)
	db 25,0			;CrystalCount , unused
	db 11,1			;JoinedDuo
	db 7,1			;Jelly
	db 1,1			;Slow purplee
	db 10,1			;Three Eyes
	db 8,1			;Moody
	db 9,1			;Vbar
	db 6,1			;Wiggle
	db 2,5			;Crystals
	db 4,17			;Mines
	db 12,4			;Clouder
	db 0,255		;End of list

Level16: ;(Type,Count)
	db 30,0			;CrystalCount , unused
	db 2,5			;Crystals
	db 4,25			;Mines
	db 12,5			;Clouder
	db 1,5			;Slow purplee
	db 0,255		;End of list


;Template Objects for leves
;	db 1,1			;Slow purplee
;	db 2,5			;Crystals
;	db 3,1			;FastBlue
;	db 5,1			;Toothy
;	db 6,1			;Wiggle
;	db 7,1			;Jelly
;	db 8,1			;Moody
;	db 9,1			;Vbar
;	db 10,1			;Three Eyes
;	db 11,1			;JoinedDuo
;	db 12,1			;Clouder

;	db 0,255		;End of list

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Enemy Definition latout
D_SprNum equ 0				;Sprite Number
D_CollProg equ 1 			;Collision program (1=Crystal 0=anything 3=Bullet (player) 255=nothing 254=dead)
D_Program equ 2 			;0=static 1+=moving
D_Xacc equ 3				;Acceleration X
D_Yacc equ 4				;Acceleration Y
;5,6,7 						;Unused

EnemyDefinitions:
	;  S,COL,P,X,Y,-,-,-
	db 0,255,0,0,0,0,0,0	;Empty (0)
	db 1,0  ,1,1,1,0,0,0	;Slow Purple (1)
	db 2,1  ,0,0,0,0,0,0	;Crystal (2)
	db 3,0  ,2,2,2,0,0,0	;Fast Blue (3)
	db 4,0  ,1,0,0,0,0,0	;Mine (4)
	db 8,0  ,3,1,1,0,0,0	;Toothy (5)
	db 9,0  ,4,2,2,0,0,0	;Wiggle (6)
	db 10,0 ,5,2,2,0,0,0	;Jelly (7)
	db 11,0 ,5,3,3,0,0,0	;Moody (8)
	db 12,0  ,3,2,2,0,0,0	;Vbar (9)
	db 13,0  ,3,3,3,0,0,0	;Three-Eyes (10)
	db 14,0  ,1,3,3,0,0,0	;JoinedDuo (11)
	db 15,0  ,6,3,3,0,0,0	;Clouder (11)

	ifdef VASM
		align 4			;for Vasm
	else
		align 16		;for Winape
	endif
;Random number Lookup tables
Randoms1:
	db $0A,$9F,$F0,$1B,$69,$3D,$E8,$52,$C6,$41,$B7,$74,$23,$AC,$8E,$D5
Randoms2:
	db $9C,$EE,$B5,$CA,$AF,$F0,$DB,$69,$3D,$58,$22,$06,$41,$17,$74,$83

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
;Text Strings

;Title Screen Strings
txtUrl: db "LearnAsm.Net",255

	ifdef ScreenWidth20
txtFire: db "Press Fire",255
	else
txtFire: db "Press Fire To Start",255
	endif
	
TxtHiScore:   db "HiScore:",255

;Game Over Strings
	ifdef ScreenWidth20
txtDead: db "Game Over!",255
	else
txtDead: db "Game Over... You're Dead! :-(",255
	endif
	
TxtNewHiscore: db "New Hiscore!",255

;Level Complete Strings
txtComplete: db "Level Complete!",255

;Ingame Strings
	ifdef ScreenWidth20
TxtCrystals: db 'Cr',255
TxtLevel: db 'Lv',255
TxtLives: db 'Li',255	
TxtScore:   db "",255
	else
	
TxtCrystals: db 'Crystal:',255
TxtLevel: db 'Level:',255
TxtLives: db 'Lives:',255	
TxtScore:   db "Score:",255
	endif



;Binary Coded Decimal Score Additions

BCD1: db $01,$00,$00,$00	;1 		Score 'adders' for BCD... 
BCD5: db $05,$00,$00,$00	;5
BCDX: db $30,$00,$00,$00	;3		my BCD requires value to add to match in length destination score

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Title Screen Logo - Different versions depending on screen size
TitlePic:
	ifdef ScreenWidth32
		db 0,10,10,0,0,6,0,0,0,0,0,0,10,10,10,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
		db 0,10,10,10,0,0,0,4,4,4,4,0,10,10,10,0,0,0,0,0,0,0,0,0,0,0,0,0,6,0,0,0
		db 0,10,10,10,10,0,4,4,4,4,0,10,10,10,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
		db 0,0,10,10,10,0,0,4,0,0,0,10,10,0,0,0,0,0,0,0,0,0,6,0,0,0,0,0,0,0,0,0
		db 6,0,0,10,10,10,0,0,0,0,10,10,0,4,4,0,0,0,0,0,0,0,0,0,0,0,0,6,0,0,0,0
		db 0,0,0,10,10,10,0,0,0,10,10,15,15,4,4,0,0,0,0,0,0,0,0,0,0,0,0,15,15,0,0,0
		db 0,4,0,0,10,10,0,0,0,15,15,15,15,15,15,0,0,0,0,0,0,0,0,0,0,15,15,15,15,15,15,0
		db 4,4,4,0,10,10,10,0,10,10,10,0,15,15,15,15,0,0,0,0,0,0,0,0,15,15,15,15,0,0,0,0
		db 4,4,0,0,0,10,10,10,10,10,0,0,0,4,15,15,0,0,0,0,0,0,0,0,15,15,0,0,0,0,6,0
		db 4,4,0,6,0,0,10,10,10,0,0,0,0,4,0,15,0,0,15,15,15,15,0,0,15,0,0,0,0,0,0,0
		db 4,4,0,0,0,0,10,10,10,0,6,0,0,4,0,0,15,15,15,15,15,15,15,15,0,0,0,0,0,0,0,0
		db 4,4,0,0,0,0,10,10,10,0,0,0,4,4,0,15,15,15,15,15,15,15,15,15,15,0,0,0,0,0,0,0
		db 4,4,4,0,0,0,10,10,10,0,0,4,4,0,15,15,2,2,15,15,15,15,2,2,15,15,0,0,0,0,6,0
		db 0,4,4,4,0,0,0,0,0,0,4,4,0,0,15,15,15,2,15,15,15,15,2,15,15,15,0,0,0,0,0,0
		db 0,0,4,4,4,4,4,4,4,4,4,0,0,0,15,15,15,15,15,15,15,15,15,15,15,15,0,0,0,0,0,0
		db 0,0,0,0,8,8,4,4,4,0,0,0,8,0,15,15,0,0,0,0,0,0,0,0,15,15,0,0,0,0,0,0
		db 0,0,0,8,8,8,8,0,0,0,0,8,8,0,15,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0
		db 6,0,8,8,0,10,8,8,0,0,8,8,0,0,0,0,0,0,8,8,8,0,0,8,8,8,0,0,0,0,8,0
		db 0,8,8,0,0,0,0,8,8,0,8,8,0,0,8,0,0,8,0,0,8,0,8,8,8,0,0,8,8,8,8,8
		db 0,8,0,0,6,0,0,8,8,0,8,8,0,8,8,0,8,8,0,8,8,0,8,8,0,0,8,8,8,8,8,8
		db 0,8,0,0,8,8,0,8,0,8,8,0,0,8,8,0,8,8,8,8,0,0,0,8,8,0,0,0,8,8,0,0
		db 0,8,8,0,8,8,8,8,0,8,8,0,8,8,8,8,8,8,0,0,0,0,0,0,8,8,0,0,8,8,0,0
		db 0,0,8,8,8,8,8,0,0,0,8,8,8,0,0,0,8,8,0,8,8,8,0,8,8,0,0,0,8,8,0,0
		db 0,0,0,0,0,8,8,0,0,0,0,0,0,0,6,0,0,8,8,8,0,0,8,8,0,0,0,0,8,0,0,0
	endif
	ifdef ScreenWidth20
		db 6,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
		db 0,10,10,0,0,0,0,10,10,0,0,6,0,0,0,0,0,0,0,0
		db 0,10,10,0,4,4,0,10,10,0,0,0,0,0,0,0,0,6,0,0
		db 0,0,10,10,0,0,10,10,0,0,0,0,0,0,0,0,0,0,0,0
		db 4,0,10,10,0,10,10,10,4,0,0,0,0,0,6,0,0,0,0,0
		db 4,0,0,10,10,10,10,0,0,4,0,0,0,0,0,0,0,0,0,0
		db 4,0,0,10,10,10,0,0,0,4,0,15,15,15,0,0,15,15,15,0
		db 4,4,0,10,10,0,0,0,4,4,15,0,0,15,15,15,15,0,0,15
		db 4,4,4,10,10,0,0,4,4,0,0,0,15,15,15,15,15,15,0,0
		db 0,4,4,4,4,4,4,4,0,0,0,15,2,2,15,15,2,2,15,0
		db 0,0,10,4,4,4,4,0,0,6,0,15,15,15,15,15,15,15,15,0
		db 0,0,10,10,0,0,0,0,0,0,0,15,0,0,0,0,0,0,15,0
		db 6,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
		db 0,8,8,0,0,8,0,0,8,0,8,8,8,0,8,8,0,8,8,8
		db 8,0,0,8,0,8,0,0,8,0,8,8,0,0,8,0,0,0,8,0
		db 8,0,8,8,0,8,0,0,8,0,8,0,0,0,0,8,0,0,8,0
		db 0,8,8,0,0,0,8,8,0,0,8,8,8,0,8,8,0,0,8,0
		db 0,0,8,8,0,6,0,0,0,0,0,0,0,0,0,0,0,6,0,0
	endif
	ifdef ScreenWidth40
		db 0,10,10, 0, 0, 6, 0, 0, 0, 0, 0, 0,10,10,10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
		db 0,10,10,10, 0, 0, 0, 4, 4, 4, 4, 0,10,10,10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
		db 0,10,10,10,10, 0, 4, 4, 4, 4, 0,10,10,10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
		db 0, 0,10,10,10, 0, 0, 4, 0, 0, 0,10,10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
		db 6, 0, 0,10,10,10, 0, 0, 0, 0,10,10, 0, 4, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0
		db 0, 0, 0,10,10,10, 0, 0, 0,10,10,10, 0, 4, 4, 0, 0, 0, 0,15,15, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,15,15, 0, 0, 0
		db 0, 4, 0, 0,10,10, 0, 0, 0,10,10, 0, 0, 0, 4, 4, 0,15,15,15,15,15,15, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,15,15,15,15,15,15, 0
		db 4, 4, 4, 0,10,10,10, 0,10,10,10, 0, 0, 0, 4, 4, 0, 0, 0, 0,15,15,15,15, 0, 0, 0, 0, 0, 0, 0, 0,15,15,15,15, 0, 0, 0, 0
		db 4, 4, 0, 0, 0,10,10,10,10,10, 0, 0, 0, 4, 4, 0, 0, 0, 0, 0, 0, 0,15,15, 0, 0, 0, 0, 0, 0, 0, 0,15,15, 0, 0, 0, 0, 6, 0
		db 4, 4, 0, 6, 0, 0,10,10,10, 0, 0, 0, 0, 4, 4, 0, 0, 0, 0, 0, 0, 0, 0,15, 0, 0,15,15,15,15, 0, 0,15, 0, 0, 0, 0, 0, 0, 0
		db 4, 4, 0, 0, 0, 0,10,10,10, 0, 6, 0, 0, 4, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0,15,15,15,15,15,15,15,15, 0, 0, 0, 0, 0, 0, 0, 0
		db 4, 4, 0, 0, 0, 0,10,10,10, 0, 0, 0, 4, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0,15,15,15,15,15,15,15,15,15,15, 0, 0, 0, 0, 0, 0, 0
		db 4, 4, 4, 0, 0, 0,10,10,10, 0, 0, 4, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0,15,15, 2, 2,15,15,15,15, 2, 2,15,15, 0, 0, 0, 0, 6, 0
		db 0, 4, 4, 4, 0, 0, 0, 0, 0, 0, 4, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,15,15,15, 2,15,15,15,15, 2,15,15,15, 0, 0, 0, 0, 0, 0
		db 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,15,15,15,15,15,15,15,15,15,15,15,15, 0, 0, 0, 0, 0, 0
		db 0, 0, 0, 0, 4, 4, 4, 4, 4, 0, 0, 0, 8, 8, 0, 0, 0, 0, 0, 0, 0, 0,15,15, 0, 0, 0, 0, 0, 0, 0, 0,15,15, 0, 0, 0, 0, 0, 0
		db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 8, 8, 8, 0, 0, 0, 0, 0, 0, 0,15, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 0
		db 6, 0, 0, 0, 0,10,10,10, 0, 0, 8, 8, 0, 0, 8, 8, 0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 8, 8, 8, 0, 0, 8, 8, 8, 0, 0, 0, 0, 8, 0
		db 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 8, 0, 0, 0, 0, 8, 8, 0, 8, 8, 0, 0, 8, 0, 0, 8, 0, 0, 8, 0, 8, 8, 8, 0, 0, 8, 8, 8, 8, 8
		db 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 6, 0, 0, 8, 8, 0, 8, 8, 0, 8, 8, 0, 8, 8, 0, 8, 8, 0, 8, 8, 0, 0, 8, 8, 8, 8, 8, 8
		db 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 8, 8, 0, 8, 0, 8, 8, 0, 0, 8, 8, 0, 8, 8, 8, 8, 0, 0, 0, 8, 8, 0, 0, 0, 8, 8, 0, 0
		db 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 8, 0, 8, 8, 8, 8, 0, 8, 8, 0, 8, 8, 8, 8, 8, 8, 0, 0, 0, 0, 0, 0, 8, 8, 0, 0, 8, 8, 0, 0
		db 0, 6, 0, 0, 0, 0, 6, 0, 0, 0, 8, 8, 8, 8, 8, 0, 0, 0, 8, 8, 8, 0, 0, 0, 8, 8, 0, 8, 8, 8, 0, 8, 8, 0, 0, 0, 8, 8, 0, 0
		db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 8, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 8, 8, 8, 0, 0, 8, 8, 0, 0, 0, 0, 8, 0, 0, 0
	endif
