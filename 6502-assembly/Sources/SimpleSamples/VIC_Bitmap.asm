
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

;Draw Bitmap
	lda #3				;Start SX
	sta z_b
	lda #3				;Start SY
	sta z_c
	jsr GetVDPScreenPos
	
	lda #0				;Tile 0 (smiley)
	sta (z_hl),y		;Transfer Tile to ram
	lda z_h
	clc
	adc #$78			;add Offset to Color Ram ($9600)
	sta z_h
	lda #4				;Color
	sta (z_hl),y		;Set Tile Color
	
	jmp *

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
	
	
	

	
Bitmap:
        DB %00111100     ;  0
        DB %01111110     ;  1
        DB %11011011     ;  2
        DB %11111111     ;  3
        DB %11111111     ;  4
        DB %11011011     ;  5
        DB %01100110     ;  6
        DB %00111100     ;  7
BitmapEnd:

