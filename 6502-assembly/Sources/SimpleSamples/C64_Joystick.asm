;FourColor equ 1

	 
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


;Current player pos
PlayerX 	equ $60		;Position of next printed character
PlayerY 	equ PlayerX+1

;Last player pos (For clearing sprite)
PlayerX2 	equ PlayerX+2
PlayerY2 	equ PlayerX+3



;Init Routine
*=$0801
	db $0E,$08,$0A,$00,$9E,$20,$28,$32,$30,$36,$34,$29,$00,$00,$00  
*=$0810	;Start at $0810

	;	  LXMSHVVV - L=Cur Line X=extended BG M=mode 
				;(Txt/Bmp) S=screen on H=height V=Vert scroll
		lda #%00111011	;turn on graphics mode
        sta $D011
		
		;     ---MWHHH - M=Multicolor W=scr width H=horiz scroll
		ifdef FourColor
			lda #%11011000  ;1=Multicolor 4 coor 
		else
			lda #%11001000  ;0=standard 2 color 
		endif
	    sta $D016

		;     SSSSTTT- - T=Text/Bmp screen address S=Screen (color) address
		lda #%00011000  ;T=1 Screen at $2000 					
        sta $D018			;(Other bits have no function in bitmap mode)
		
		;	  ----CCCC
		lda #%00000000
		sta $D021		;Background color (only bits #0-#3).	
		sta $D020		;Border 
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	
	lda #$20		;Clear $4000-$6000
	sta z_h
	ldx #$20		;Clear $2000 bytes
	jsr FillZeros
	
	lda #$04		;Clear $0400-$0800
	sta z_h
	ldx #$04		;Clear $400 bytes
	jsr FillZeros
	
	
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
	cmp #%11111111
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
	bne JoyNotUp	;Jump if UP not presesd
	tya
	sec				;Move Y Up the screen
	sbc #8
	tay
JoyNotUp:
	lda z_h
	and #%00000010	;---FRLDU
	bne JoyNotDown	;Jump if DOWN not presesd
	tya
	clc	 			;Move Y Down the screen
	adc #8
	tay
JoyNotDown:
	lda z_h
	and #%00000100	;---FRLDU
	bne JoyNotLeft	;Move X Left 
	dex
JoyNotLeft:
	lda z_h
	and #%00001000	;---FRLDU
	bne JoyNotRight	;Move X Right
	inx
JoyNotRight:
	
	stx PlayerX		;Update X
	sty PlayerY		;Update Y
	
;X Boundary Check - if we go <0 we will end up back at 255
	cpx #40
	bcs PlayerReset
	
;Y Boundary Check - only need to check 1 byte
	cpy #200
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
	
FillZeros:
	lda #$00	;Byte to fill
	sta z_l		;Bottom Byte of Dest Address
	tay			;Bottom Byte of Byte Count
	
FillZerosB:	
	sta (z_hl),y
	dey
	bne FillZerosB
	inc z_h
	dex 
	bne FillZerosB	
	rts
	
	
	
	
BlankPlayer:
	lda #<BitmapBlank		;Source Bitmap Data
	sta z_L
	lda #>BitmapBlank
	sta z_H
	jmp DrawSprite
DrawPlayer:
	lda #<Bitmap		;Source Bitmap Data
	sta z_L
	lda #>Bitmap
	sta z_H

DrawSprite:
	txa
	pha
	tya
	pha
		stx z_b
		sty z_c
		jsr GetScreenPos ;Get screen pos from XY into Z_DE

BitmapNextLine:
		ldY #0			 ;Offset for bytes in this strip
BitmapNextByte:
		lda (z_hl),Y	 ;Load in a byte from source offset with Y
		sta (z_de),Y	 ;Store it in screen ram - offset with Y
		inY				 ;INC the offset
		cpY #8			 ;We draw 8 lines * bitmap width
		bne BitmapNextByte

	pla
	tay
	pla
	tax
	
;Fill Color Data
	stx z_b
	sty z_c
	jsr GetColMemPos	;Get color pos from XY into Z_DE

	ldy #0
	ifdef FourColor
		lda #$43		;Color
		sta (z_de),Y	;%22221111 Color 1,2
		lda #01
		sta (z_bc),Y	;%----3333 Color 3 (White)
	else
		lda #$40		;Color
		sta (z_de),Y	;%11110000 Color 1,2
	endif		
		
	rts
	
	
Bitmap:
	ifdef FourColor
        DB %00010100     ;  0
        DB %00010101     ;  1
        DB %01110101     ;  2
        DB %01010101     ;  3
        DB %01010101     ;  4
        DB %01100101     ;  5
        DB %00011001     ;  6
        DB %00010100     ;  7
	else
		DB %00111100     ;  0
        DB %01111110     ;  1
        DB %11011011     ;  2
        DB %11111111     ;  3
        DB %11111111     ;  4
        DB %11011011     ;  5
        DB %01100110     ;  6
        DB %00111100     ;  7
	endif	
	    
BitmapBlank:
	ds 8,0				;Empty Sprite
	


	
;Address= (X * 8) + (Top5BitsOfY * 40) + $2000
GetScreenPos:
	lda #0
	sta z_b
	sta z_d
	txa				;Multiple X by 8
	asl				;-------- XXXXXXXX
	rol z_d
	asl
	rol z_d
	asl
	rol z_d			;-----XXX XXXXX---
	sta z_e

;40 bytes per Yline =00000000 00101000
	tya
	and #%11111000	;00000000 YYYYYyyy
	asl
	rol z_b
	asl
	rol z_b
	asl				;00000000 00101000
	rol z_b			;00000YYY YYyyy000
	tax 
		adc z_e		;Add part to total L
		sta z_e
		lda z_b		;Add part to total H
		adc z_d
		sta z_d
	txa 
	asl
	rol z_b
	asl				;00000000 00101000
	rol z_b			;000YYYYY yyy00000
	
	adc z_e			;Add part to total L
	sta z_e
	lda z_b			;Add part to total H
	adc z_d
	adc #$20+0		;Screen Base $2000
	sta z_d
	rts
	
	
;Color Ram data at $D800 & $400
;Address = $0400+ Y Strip * 40 + Xpos

GetColMemPos:
	lda #0
	sta z_d
	txa
	sta z_e			;Xpos
;40 bytes per Yline =00000000 00101000
	tya				;Need to multiply by 40 (%00101000)
	and #%11111000	;One color per 8x8 square
	tay 
		clc
		adc z_e		;Add Ypos part to Xpos 
		sta z_e		;Save %00-01000 part
	tya 
	asl
	rol z_d	
	asl				;00000000 00101000
	rol z_d			;000YYYYY yyy00000
	clc
	adc z_e			;Add Ypos part to total
	sta z_e
	sta z_c
	
	lda z_d
	adc #$04+0		;Color Offset $0400
	sta z_d
	
	adc #$D8-4		;Color Offset $D800
	sta z_b
	rts				;z_DE = $0400-07FF byte  
				;z_BC = $D800-DBFF byte (for 4 color)


PauseXY:
	dex
	bne PauseXY
	dey 
	bne PauseXY
	rts


Player_ReadControlsDual:	
	;lda $DC00			;Read in Joystick 1
	lda $DC01			;Read in Joystick 2
	sta z_h			;---FRLDU
	rts
		
