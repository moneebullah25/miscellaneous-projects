DefineTiles:
SetPalette:
ScreenInit:
	rts
		
	
GetVDPScreenPos:	; BC=XYpos	
	lda #$80			;Screen base is $8000
	sta z_h		
	
	lda z_b				;Xpos
	ifdef ScreenWidth32
		clc
		adc #4
	endif
	sta z_l

	ldy z_c				;Ypos
	beq GetVDPScreenPos_YZero
GetVDPScreenPos_Addagain:	;Repeatedly add screen width (22) Y times 
	clc
	lda z_l
	adc #40			;40 bytes per line
	sta z_l
	lda z_h
	adc #0			;Add Carry
	sta z_h
	
	dey
	bne GetVDPScreenPos_Addagain
GetVDPScreenPos_YZero:
	rts