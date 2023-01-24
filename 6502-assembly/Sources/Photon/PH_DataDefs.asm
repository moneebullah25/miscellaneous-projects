
;4 directions represented as accelerations	
Directions:				
	dw 0,-1		; Up
	dw 1,0		; Right
	dw 0,1		; Down
	dw -1,0		; Left


;Random number Lookup tables	
	Align 4
Randoms1:
	db $0A,$9F,$F0,$1B,$69,$3D,$E8,$52,$C6,$41,$B7,$74,$23,$AC,$8E,$D5
Randoms2:
	db $9C,$EE,$B5,$CA,$AF,$F0,$DB,$69,$3D,$58,$22,$06,$41,$17,$74,$83

	
;Default Settings for new level
UserRamBak:
 db 1						;Rotation
	ifdef ScreenWidth20
		dw 16				;Player X
		dw 16				;Player Y
	else
		dw 32
		dw 32
	endif
 dw 1						;Accel X
 dw 0						;Accel Y
	ifdef ScreenWidth20
		dw ScreenWidth-16	;Cpu X
		dw ScreenHeight-16	;Cpu Y
	else
		dw ScreenWidth-32
		dw ScreenHeight-32
	endif
 db 3						;Cpu Direction
 dw -1						;Cpu Accel X
 dw 0						;Cpu Accel Y
 db 1						;Cpu Turn
UserRamBakEnd:


;Title Message
	ifdef ScreenWidth20
Ttitle1:
	db "Battle of the Chibi",255
Ttitle2:
	db "Photonic hunters!",255
	else
Ttitle:
	db "Battle of the Chibi Photonic hunters!",255
	endif
TBestLevel:
	db "BestLevel:",255

;Game Over message
Tgameover:
	db "Game Over!",255
TYouSuck:
	ifdef ScreenWidth40
		db "Your Performance Sucks!",255
	else
		db "Your GamePlay Sucks!",255
	endif
TYouRock:
	db "New Best Performance!",255

	
;Obstruction objects (Cpacket format)
Object1:
	;   CYYYYYYY, DXXXXXXX  CD=Command Y=Y dest X=X dest
    db %00000011,%01111101	;Move
    db %00000000,%10000110	;  Line
    db %01111010,%10000000	;  Line
    db %00000000,%11111010	;  Line
    db %00000110,%10000000	;  Line
    db %01111010,%10000110	;  Line
    db %00000000,%01111010	;Move
    db %10000110,%10000110	;  Line + End

Object2:
    db %00000001,%01111101	;Move
    db %00000010,%10000000	;  Line
    db %00000000,%10000010	;  Line
    db %00000000,%00000010	;Move
    db %00000000,%10000010	;  Line
    db %01111110,%10000000	;  Line
    db %01111110,%00000000	;Move
    db %01111110,%10000000	;  Line
    db %00000000,%11111110	;  Line
    db %00000000,%01111110	;Move
    db %00000000,%11111110	;  Line
    db %10000010,%10000000	;  Line + End
	
	
	