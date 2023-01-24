
z_Regs equ $20

z_HL equ z_Regs
z_L  equ z_Regs
z_H  equ z_Regs+1

z_BC equ z_Regs+2
z_C  equ z_Regs+2
z_B  equ z_Regs+3

z_DE equ z_Regs+4
z_E  equ z_Regs+4
z_D  equ z_Regs+5
z_As  equ z_Regs+6


;Current player pos
PlayerX 	equ $60		
PlayerY 	equ PlayerX+1

;Last player pos (For clearing sprite)
PlayerX2 	equ PlayerX+2
PlayerY2 	equ PlayerX+3


* = $1001
		; BASIC program to boot the machine language code
		db $0b, $10, $0a, $00, $9e, $34, $31, $30, $39, $00, $00, $00
;start of code $100A

;Screen Init
	ldx #16					;We're going to copy 16 registers 
ScreenInitAgain:	
	dex
	lda VicScreenSettings,x	;Get A parameter
	sta $9000,X				;Store to the video registers at $9000
	txa
	bne ScreenInitAgain
	
	
	
	
	lda #$1E	;Clear $1E00-$2000
	sta z_h
	lda #$00
	sta z_l
	
	ldx #$02	;Clear $200 bytes
	tay			;#$00
	lda #1		;Tile 1=Blank
FillZerosB:	
	sta (z_hl),y
	dey
	bne FillZerosB
	inc z_h
	dex 
	bne FillZerosB
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
;Define Tiles
		
	lda #<Bitmap					;Source Bitmap Data
	sta z_L
	lda #>Bitmap
	sta z_H

	lda #<(BitmapEnd-Bitmap);Source Bitmap Data Length
	sta z_C
	lda #>(BitmapEnd-Bitmap)
	sta z_B
	
	lda #<$1C00		;Tile 0 in VIC Custom Characters
	sta z_E
	lda #>$1C00
	sta z_D
	
	jsr DefineTiles		;Define the tile patterns
	
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
	and #%00001000	;-FLDU--R
	bne JoyNotUp	;Jump if UP not presesd
	dey				;Move Y Up the screen
JoyNotUp:
	lda z_h
	and #%00010000	;-FLDU--R
	bne JoyNotDown	;Jump if DOWN not presesd
	iny 			;Move Y Down the screen
JoyNotDown:
	lda z_h
	and #%00100000	;-FLDU--R
	bne JoyNotLeft	;Move X Left 
	dex
JoyNotLeft:
	lda z_h
	and #%00000001	;-FLDU--R
	bne JoyNotRight	;Move X Right
	inx
JoyNotRight:

	stx PlayerX		;Update X
	sty PlayerY		;Update Y
	
;X Boundary Check - if we go <0 we will end up back at 255
	cpx #22
	bcs PlayerReset
	
;Y Boundary Check - only need to check 1 byte
	cpy #23
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
	lda #1		;Tile Num (blank sprite)
	jmp DrawSprite
DrawPlayer:
	lda #0		;Tile Num (Smiley)
DrawSprite:
	pha
		stx z_b
		sty z_c
		jsr GetVDPScreenPos	;Calculate Tilemap mempos
	pla
		
	sta (z_hl),y		;Transfer Tile to ram
	lda z_h
	clc
	adc #$78			;add Offset to Color Ram ($9600)
	sta z_h
	lda #4				;Color
	sta (z_hl),y		;Set Tile Color
	rts
	
Bitmap:	;Smiley
        DB %00111100     ;  0
        DB %01111110     ;  1
        DB %11011011     ;  2
        DB %11111111     ;  3
        DB %11111111     ;  4
        DB %11011011     ;  5
        DB %01100110     ;  6
        DB %00111100     ;  7
		
        DS 8,0			;Blank Sprite
BitmapEnd:

	
	
	
	

Player_ReadControlsDual:
	lda #%01111111
	sta $9122	;Set Data Direction of port B to READ (0=read)
	
;	lda #%11000011
;	sta $9113	;Set Data Direction of port A to READ (0=read)
	
	lda $9120	;Port B (R------- Switch)
	sta z_as
	
	lda $911F	;Port A (--FLDU-- Switches)
	rol z_as
	rol 
	sta z_h		;-FLDU--R
	
;	lda #255
;	sta $9122	;Reset port B (for Keyb col scan)
	rts
	
	
PauseXY:
	dex
	bne PauseXY
	dey 
	bne PauseXY
	rts

		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
;Address= $1E00 + (Ypos * 22) + Xpos	
	
GetVDPScreenPos:	; BC=XYpos	
	lda #$1e			;Screen base is $1E00
	sta z_h					;Colors at $9600 (add $7800 offset)
	
	lda z_b				;Xpos
	sta z_l

	ldy z_c				;Ypos
	beq GetVDPScreenPos_YZero
GetVDPScreenPos_Addagain:	;Repeatedly add screen width (22) Y times 
	clc
	lda z_l
	adc #22			;22 bytes per line
	sta z_l
	lda z_h
	adc #0			;Add Carry
	sta z_h
	
	dey
	bne GetVDPScreenPos_Addagain
GetVDPScreenPos_YZero:
	rts
	
;Copy Data to the Character Defs from ram/rom to char ram
DefineTiles:	
		ldy #0
DefineTiles2:
        lda (z_HL),Y		;Copy From z_HL
        sta (z_DE),Y		;To z_ DE
		iny					;Inc Byte
		BNE	DefineTiles_SkipInc1
		INC	z_H				;Inc H bytes
		INC	z_D
DefineTiles_SkipInc1:
		DEC z_C				;Dec Counter
		BNE DefineTiles2
		LDA z_B
		BEQ	DefineTiles_Done
		DEC z_B
		jmp DefineTiles2
DefineTiles_Done:
		rts
		

	;LLLL options
	   ; 0000   ROM   8000  32768
	   ; 0001         8400  33792
	   ; 0010         8800  34816
	   ; 0011         8C00  35840
	   ; 1000   RAM   0000  0000
	   ; 1001         xxxx
	   ; 1010         xxxx  unavail.
	   ; 1011         xxxx
	   ; 1100         1000  4096
	   ; 1101         1400  5120
	   ; 1110         1800  6144
	   ; 1111         1C00  7168		<---
	   
	   
	
VicScreenSettings:
	db $0C		;$9000 - horizontal centering
	db $26		;$9001 - vertical centering
	db $96		;$9002 - set # of columns / 
					;Bit7 = screen base bit ($16 for screen at $1000)
	db $AE		;$9003 - set # of rows
	db $7A		;$9004 - TV raster beam line
	db $FF		;$9005 - bits 0-3 start of character memory /  
					;bits 4-7 is rest of video address 
					;$(CF for screen at $1000)
	db $57		;$9006 - horizontal position of light pen
	db $EA		;$9007 - vertical position of light pen
	db $FF		;$9008 - Digitized value of paddle X
	db $FF		;$9009 - Digitized value of paddle Y
	db $00		;$900A - Frequency for oscillator 1 (low)
	db $00		;$900B - Frequency for oscillator 2 (medium)
	db $00		;$900C - Frequency for oscillator 3 (high)
	db $00		;$900D - Frequency of noise source
	db $00		;$900E - bit 0-3 sets volume of all sound / 
					;bits 4-7 are auxiliary color information
	db $00+8 	;$900F - Screen and border color register
	
	
	

