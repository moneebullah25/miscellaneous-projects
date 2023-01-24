
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

	
	lda #4
	sta z_d				;Color
	
	lda #3				;Start SX
	sta z_b
	
	lda #3				;Start SY
	sta z_c
	
	ldx #6				;Width in tiles
	ldy #6				;Height in tiles
	lda #0				;TileStart
	jsr FillAreaWithTiles	;Draw the tiles to screen
	
	jmp *
	
GetVDPScreenPos:	; BC=XYpos	
	lda #$1e			;Screen base is $1E00
	sta z_h
	
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

FillAreaWithTiles:
	sta z_e			;Backup Tile number
	tya
	pha
						;Calculate screen ram location of tile
		jsr GetVDPScreenPos	
	pla
	tay
FillAreaWithTiles_Yagain:
	tya
	pha
	txa
	pha
		lda z_e			;Tilenum
		ldy #0
FillAreaWithTiles_Xagain:
		sta (z_hl),y	;Transfer Tile to ram
		pha
			lda z_h		;Back up z_H
			pha
				clc
				adc #$78	;Offset to Color Ram
				sta z_h
				lda z_d
				sta (z_hl),y ;Set Tile Color
			pla
			sta z_h		;Restore z_H
			iny
		pla
		clc
		adc #1			;Increase tile number
		dex				;Decrease X counter
		bne FillAreaWithTiles_Xagain
		sta z_e			;Back up Tilenum for next loop
		inc z_c
	pla
	tax
	pla
	tay 
	lda z_l
	adc #22			;Move Down (22 bytes per line)
	sta z_l
	lda z_h
	adc #0			;Add Carry
	sta z_h
	dey				;Decrease Y counter
	bne FillAreaWithTiles_Yagain
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
	
	
	

DefineTiles:	;Copy Data to the Character Defs from ram/rom to char ram
		ldy #0
DefineTiles2:
        lda (z_HL),Y
        sta (z_DE),Y
		iny
		BNE	LDIR_SkipInc1
		INC	z_H
		INC	z_D
LDIR_SkipInc1:
		DEC z_C
		BNE DefineTiles2
		LDA z_B
		BEQ	DefineTiles_Done
		DEC z_B
		jmp DefineTiles2
DefineTiles_Done:
		rts
		

	
Bitmap:
	incbin "\ResAll\Sprites\RawVIC.raw"
BitmapEnd:

