Player_ReadControlsDual:	;--21RLDU
	lda #255
	sta z_h				;Cursor buildup
	sta z_l				;Unused 
	
	lda #6
	ldx #%00100000		;Fire 2 (Enter)
	jsr TestCursorBit
	
	lda #9
	ldx #%00000100		;Fire 1 (Space)
	jsr TestCursorBit
	
	lda #4
	ldx #%10000000		;Right (Numpad 6)
	jsr TestCursorBit
	
	;lda #4
	ldx #%01000000		;Left (Numpad 4)
	jsr TestCursorBit
	
	lda #7
	;ldx #%01000000		;Down (Numpad 2)
	jsr TestCursorBit
	
	lda #3
	;ldx #%01000000		;Up	(Numpad 8)
	;jsr TestCursorBit
	;rts
	
TestCursorBit:
	pha
		sta $E810			;Select line
		txa
		and $E812			;test key
		clc					;Clear carry (Pressed)
		beq TestCursorBitB		
		sec					;Set carry (not pressed)
TestCursorBitB:	
		rol z_h				;shift the key into the buildup
	pla
	rts
	
	