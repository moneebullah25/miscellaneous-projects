
;Current player pos
PlayerX 	equ $60		;Position of next printed character
PlayerY 	equ PlayerX+1

;Last player pos (For clearing sprite)
PlayerX2 	equ PlayerX+2
PlayerY2 	equ PlayerX+3



z_Regs 		equ $20

z_HL equ z_Regs
z_L  equ z_Regs
z_H  equ z_Regs+1

z_BC equ z_Regs+2
z_C  equ z_Regs+2
z_B  equ z_Regs+3

z_DE equ z_Regs+4
z_E  equ z_Regs+4
z_D  equ z_Regs+5


	org $200-10		;Our program starts at $0200
	db $80,$08,$02,$00,$40,$0A,$42,$53,$39,$33
	
;ScreenInit	-	SUZY chip needs low byte setting first 
					;OR IT WILL WIPE THE HIGH BYTE!
	
	;Set screen ram pointer to $C000
	stz $FD94		;DISPADR	Display Address L (Visible)
	lda #$C0	
	sta $FD95		;DISPADR	Display Address H (Visible)
	
	;Do the palette
	stz $FDA0		;Palette Color 0 ----GGGG (Black)
	stz $FDB0		;Palette Color 0 BBBBRRRR
	
	;lda #%00000000	;Palette Color 1 ----GGGG (Purple)
	stz $FDA1
	lda #%01110111	;Palette Color 1 BBBBRRRR
	sta $FDB1
	
	lda #%00001111	;Palette Color 2 ----GGGG (Cyan)
	sta $FDA2
	lda #%11110000	;Palette Color 2 BBBBRRRR
	sta $FDB2
	
	lda #%00001111	;Palette Color 3 ----GGGG (White)
	sta $FDA3
	lda #%11111111	;Palette Color 3 BBBBRRRR
	sta $FDB3
	




	lda #3			;Start SX
	sta PlayerX
	lda #3			;Start SY
	sta PlayerY

	lda #0			;Fake No Keys on first run		
	sta z_h
	jmp StartDraw	;Force Draw of character first run
	
	
infloop:
	jsr Player_ReadControlsDual
	lda z_h
	beq infloop		;See if no keys are pressed
	pha
StartDraw:	
		ldx PlayerX		;Back up X
		stx PlayerX2
		
		ldy PlayerY		;Back up Y
		sty PlayerY2
		
		jsr BlankPlayer	;Remove old player sprite
		
		ldx PlayerX		;Back up X
		ldy PlayerY		;Back up Y
		
	pla
	sta z_h
	and #%10000000	;UDLR12IO  
	beq JoyNotUp	;Jump if UP not presesd
	dey				;Move Y Up the screen
JoyNotUp:
	lda z_h	
	and #%01000000	;UDLR12IO 
	beq JoyNotDown	;Jump if DOWN not presesd
	iny 			;Move Y Down the screen
JoyNotDown:
	lda z_h
	and #%00100000	;UDLR12IO  
	beq JoyNotLeft	;Move X Left 
	dex
JoyNotLeft:
	lda z_h
	and #%00010000	;UDLR12IO 
	beq JoyNotRight	;Move X Right
	inx
JoyNotRight:

	stx PlayerX		;Update X
	sty PlayerY		;Update Y
	
;X Boundary Check - if we go <0 we will end up back at 255
	cpx #80-3
	bcs PlayerReset
	
;Y Boundary Check - only need to check 1 byte
	cpy #102-7
	bcs PlayerReset
	
	jmp PlayerPosYOk ;Not Out of boundsS
	
PlayerReset:
	ldx PlayerX2	;Reset Xpos	
	stx PlayerX
	
	ldy PlayerY2	;Reset Ypos
	sty PlayerY
	
PlayerPosYOk:
	jsr DrawPlayer	;Draw Player Sprite
	
	ldx #255
	ldy #100
	jsr PauseXY		;Wait a bit!	
	jmp infloop
	
PauseXY:
	dex
	bne PauseXY
	dey 
	bne PauseXY
	rts
	
	
Player_ReadControlsDual:
	lda $FCB0		;JOYSTICK	Read Joystick and Switches	
	sta z_h			;UDLR12IO
	rts

bmpwidth equ 4		;Width in bytes
bmpheight equ 8		;Height in lines
			
BlankPlayer:
	lda #<BitmapBlank	;Bitmap source
	sta z_L
	lda #>BitmapBlank
	sta z_H
	jmp DrawSprite
DrawPlayer:
	lda #<BitmapSmiley	;Bitmap source
	sta z_L
	lda #>BitmapSmiley
	sta z_H
DrawSprite:
	jsr GetScreenPos
	ldx #0
BitmapNextLine:
	phy
		ldY #0
BitmapNextByte:
		lda (z_hl),Y		;Copy a byte from the source 
		sta (z_de),Y		;to the destination
			
		inY
		cpY #bmpwidth		;Repeat for next byte of line
		bne BitmapNextByte
		
		clc
		tya
		adc z_l				;ADD Y to Z_HL to move source 
		sta z_l
		lda z_h
		adc #0
		sta z_h
			
		clc
		lda z_e
		adc #$50			;ADD 50 to Z_DE to move Destination
		sta z_e
		lda z_d
		adc #0
		sta z_d
	ply
	inx 
	cpx #bmpheight			;Check if we've done all the lines
	bne BitmapNextLine		;Repeat until we have
	rts
	
BitmapSmiley:
    DB $00,$11,$11,$00     ;  0
    DB $01,$11,$11,$10     ;  1
    DB $11,$31,$13,$11     ;  2
    DB $11,$11,$11,$11     ;  3
    DB $11,$11,$11,$11     ;  4
    DB $11,$21,$12,$11     ;  5
    DB $01,$12,$21,$10     ;  6
    DB $00,$11,$11,$00     ;  7

BitmapBlank:
    ds 32,0

	

	;Y= $50 bytes per Yline = 00000000 01010000
	;Move Y into top byte 	= YYYYYYYY 00000000
	;Shift Right Twice      = 00YYYYYY YY000000
	;Shift Right Twice      = 0000YYYY YYYY0000
	
GetScreenPos:
	lda #$00		;Reset z_C
	sta z_c
	
	tya 			;Move Y into top byte 	= YYYYYYYY 00000000
	lsr
	ror z_c
	lsr 
	ror z_c			;Shift Right Twice      = 00YYYYYY YY000000
	
	sta z_d			;Store High byte in total	
	lda z_c			
	sta z_e			;Store Low byte in total
	
	lda z_d			;Shift Right Twice      = 0000YYYY YYYY0000
	lsr
	ror z_c
	lsr 
	ror z_c
	
	clc				;Add High byte to total
	adc z_d
	adc #$C0		;Screen base at &C0000
	sta z_d

	
	lda z_c			;Add Low byte to total
	adc z_e
	sta z_e
	
	lda z_d			;Add any carry to the high byte
	adc #0
	sta z_d
	
	clc				;Add the X pos 
	txa 
	adc z_e 
	sta z_e
	
	lda z_d			;Add any carry to the high byte
	adc #0
	sta z_d
	rts
	
	

