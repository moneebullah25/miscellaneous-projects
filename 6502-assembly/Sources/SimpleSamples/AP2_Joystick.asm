
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
z_B  equ z_Regs+2

z_DE equ z_Regs+4
z_E  equ z_Regs+4
z_D  equ z_Regs+5


	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
	ORG $0C00	;Program Start
	sei 		;Disable interrupts
	
	lda $C050 	;TXTCLR:   Display Graphics
	lda $C052 	;MIXCLR:   Display Full Screen
	lda $c057 	;HIRES:    Display HiRes Graphics
	lda $C055 	;TXTPAGE2: If 80STORE Off: Display Page 2
	
	lda #$40	;Clear $4000-$6000
	sta z_h
	lda #$00
	sta z_l
	
	ldx #$20	;Clear $2000 bytes
	ldy #$00
	
FillZeros:	
	sta (z_hl),y
	dey
	bne FillZeros
	inc z_h
	dex 
	bne FillZeros
	
	lda #4			;Start SX
	sta PlayerX
	lda #80			;Start SY
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
	and #%00000001	;---FRLDU
	beq JoyNotUp	;Jump if UP not presesd
	tya
	sec				;Move Y Up the screen
	sbc #8
	tay
JoyNotUp:
	lda z_h
	and #%00000010	;---FRLDU
	beq JoyNotDown	;Jump if DOWN not presesd
	tya
	clc	 			;Move Y Down the screen
	adc #8
	tay
JoyNotDown:
	lda z_h
	and #%00000100	;---FRLDU
	beq JoyNotLeft	;Move X Left 
	dex
JoyNotLeft:
	lda z_h
	and #%00001000	;---FRLDU
	beq JoyNotRight	;Move X Right
	inx
JoyNotRight:
	stx PlayerX		;Update X
	sty PlayerY		;Update Y
	
;X Boundary Check - if we go <0 we will end up back at 255
	cpx #40
	bcs PlayerReset
	
;Y Boundary Check - only need to check 1 byte
	cpy #192
	bcs PlayerReset
	
	jmp PlayerPosYOk ;Not Out of bounds
	
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
	
BlankPlayer:
	lda #<BitmapBlank	;Source Bitmap Data
	sta z_L
	lda #>BitmapBlank
	sta z_H
	jmp DrawSprite

DrawPlayer:
	lda #<BitmapSmiley	;Source Bitmap Data
	sta z_L
	lda #>BitmapSmiley
	sta z_H
	
DrawSprite:
	jsr GetScreenPos
	
	ldY #0
BitmapNextLine:
	ldx #0
	lda (z_hl),Y 	;Read byte from source
	sta (z_de,X) 	;Write to screen
			
	lda z_d			;move mempos down a line
	clc
	adc #$04		;add $0400 to the line number 
	sta z_d			;  (only works within an  8 line block)

	iny
	cpy #8
	bne BitmapNextLine;need a recalc every 8 lines
	rts
		
BitmapSmiley:
	;   C0123456
    DB %00011100     ;  0
    DB %00111110     ;  1
    DB %01101011     ;  2
    DB %01111111     ;  3
    DB %01111111     ;  4
    DB %01101011     ;  5
    DB %00110110     ;  6
    DB %00011100     ;  7

BitmapBlank:
	ds 8,0
	
	
	;Screen layout is split in 3 parts according to Y line
	;AABBBCCC - AA*$0028  BBB*$0080  CCC*$0400
GetScreenPos:
	lda #0
	sta z_e
	tya 				;--BBB---	;Multiply by $0080
	and #%00111000
	lsr
	lsr
	lsr					
	lsr					;Shift 1 bit right 
	ror z_e
	adc #$40			;Screen base
	sta z_d
	tya					;AA------		;multiply by $0028
	rol 				;Get 1st A from AA------ 
	bcc GetScreenPos_SecondThird
GetScreenPos_ThirdThird:
	lda z_e
	clc
	adc #$50			;3/3 = Add $0050 to address
	jmp GetScreenPos_ThirdDone
GetScreenPos_SecondThird:
	rol 				;Get 2nd A from AA------ 
	bcc GetScreenPos_FirstThird
	lda z_e
	clc
	adc #$28			;3/2 = Add $0028 to address
GetScreenPos_ThirdDone:	
	sta z_e
GetScreenPos_FirstThird:;1/3 = Add nothing to addreess
	tya 			;-----CCC	;Multiply by 4
	and #%00000111
	asl
	asl
	adc z_d
	sta z_d
	
	txa				;Process X
	clc
	adc z_e			;Add X to calculated address
	sta z_e
	rts
	

PauseXY:
	dex
	bne PauseXY
	dey 
	bne PauseXY
	rts	
	
;Apple Joysticks are annoying!
;they are analog... we have to strobe the port 
;then read from the X and Y ports, and count up until the top bit changes
;this is a 'timer'...using just 1 bit (the top one) 
;it effectively returns an 'analog' value from about 0-100
	
Player_ReadControlsDual:	;---FRLDU
	lda $C061			;Fire 1
	and %00000001		;Move in the fire button
	sta z_h
	
	lda $C070			;Strobe Joypads
	ldy #0
	ldx #0 
Joy_ReadAgain:
	pha
	pla					;delay
Joy_gotPDL1:			;Jump backhere when we get X
Joy_ChkPDl0:
	lda	$C064 			;<--SM ***   Y
JoySelfModAA_Plus2:
	bpl Joy_gotPDL0		;Have we got Y?
	nop
	iny	
	lda $C065			;<--SM ***   X
JoySelfModB_Plus2:
	bmi Joy_nogots		;Have we got X?
	bpl Joy_gotPDL1
Joy_nogots:
	inx
	jmp Joy_ChkPdl0
Joy_gotPDL0:			;We've Got Tpos - just waiting for X
	lda  $C065			;<--SM ***   X
JoySelfModBB_Plus2:
	bmi Joy_Nogots
	
	tya
	jsr JoyConvertAnalog;Convert Y
	txa
						;Convert X
	
JoyConvertAnalog:	;covert analog from 0-100 into L/R or U/D
	cmp #$66
	bcs Joy_Rbit
	cmp #$33
	bcc Joy_Lbit
	clc 
	bcc Joy_Cbit
Joy_Rbit:
	sec 
Joy_Cbit:
	rol z_h
	clc
	rol z_h
	rts
	
Joy_Lbit:
	clc
	rol z_h
	sec 
	rol z_h
	rts
	
	