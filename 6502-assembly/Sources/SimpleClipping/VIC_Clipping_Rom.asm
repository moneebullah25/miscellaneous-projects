
VscreenMinX equ 84		;Top left of visible screen in logical co-ordinates
VscreenMinY equ 80

;VscreenWid equ 24		;Visible Screen Size in logical units
;VscreenHei equ 24

;LIMITATION.. The Virtual screen cannot be smaller than the sprite or 
;the crop will malfunction! (It can be the same size)

VscreenWid equ 88		;Visible Screen Size in logical units
VscreenHei equ 96

VscreenWidClip equ 0
VscreenHeiClip equ 0



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

z_ixl equ z_Regs+8
z_ixh equ z_Regs+9
z_ix equ z_Regs+8

z_iyl equ z_Regs+10
z_iyh equ z_Regs+11
z_iy  equ z_Regs+10



;Current player pos
PlayerX 	equ $60		
PlayerY 	equ PlayerX+1


spritehclip equ PlayerX+4



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

	
	jsr cls
	
	ldx #VscreenMinX			;Start SX
	stx PlayerX
	ldy #VscreenMinY+88			;Start SY
	sty PlayerY
	
	lda #0			;Fake No Keys on first run		
	sta z_d
	jmp StartDraw	;Force Draw of character first run
	
	
infloop:
	jsr Player_ReadControlsDual
	lda z_h
	cmp #%11111111
	beq infloop		;See if no keys are pressed
	pha
StartDraw:	
		ldx PlayerX		;Back up X
		ldy PlayerY		;Back up Y
		
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
	
	jsr DrawPlayer	;Draw Player Sprite
	
	ldx #255
	ldy #10
	jsr PauseXY		;Wait a bit!	
		
	jmp infloop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


	
BlankPlayer:
	lda #0
	sta z_iyh		;Color
	lda #36			;Tile Num (blank sprite)
	jmp DrawSprite
	
DrawPlayer:
	lda #4
	sta z_iyh		;Color
	lda #0			;Tile Num (Chibiko)
DrawSprite:
	sta z_iyl		;First Tile to show

	stx z_b			;Xpos
	sty z_c			;Ypos
	
	lda #24
	sta z_h			;Wid
	lda #24
	sta z_l			;Hei
	
ShowBitmap:		;Show Zero terminated 'bitmap' from z_HL to z_DE
				;Size (W,H) z_B,z_C 
	jsr DoCrop	;Crop the sprite BC=XY pos HL=WidthHeigh, IY=source data
	bcc DoDraw
		rts			;Nothing to draw
DoDraw:				
	jsr GetVDPScreenPos		;Calculate screen ram location of tile
	
FillAreaWithTiles_Yagain:
	ldx z_h	
		ldy #0
FillAreaWithTiles_Xagain:
		lda z_iyl		;Tilenum
		sta (z_de),y	;Transfer Tile to ram
		
		lda z_d			;Back up z_H
		pha
			clc
			adc #$78		;Offset to Color Ram
			sta z_d
			lda z_iyh
			sta (z_de),y 	;Set Tile Color
		pla
		sta z_d			;Restore z_H
		iny
		inc z_iyl
		dex				;Decrease X counter
		bne FillAreaWithTiles_Xagain
		inc z_c
		
		lda spritehclip
		beq NoSpriteClip ;Skip any unused Htiles
		clc
		adc z_iyl
		sta z_iyl
NoSpriteClip:
	lda z_e
	adc #22				;Move Down (22 bytes per line)
	sta z_e
	bcc FillAreaDok
	inc z_d
FillAreaDok:
	dec z_l				;Decrease Y counter
	bne FillAreaWithTiles_Yagain
	rts
	
	
		
Bitmap:
	incbin "\ResAll\Sprites\RawVIC.raw"
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
	
;Address= $1E00 + (Ypos * 22) + Xpos  (22=16+4+2)
	
GetVDPScreenPos:	; BC=XYpos	
	clc
	lda z_c				;Ypos
	asl
	sta z_e				;*2
	asl
	sta z_c				;*4
	adc z_e
	sta z_e
	
	lda #0
	sta z_d
	lda z_c
	asl
	rol z_d
	asl
	rol z_d
	adc z_e				;*16 
	sta z_e
	
	lda z_d
	adc #$1e
	sta z_d
	
	
	lda z_b				;Xpos
	adc z_e
	sta z_e
	bcc GetVDPScreenPos_YZero
	inc z_d

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
		

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Cls:
	lda #$1E		;$8000 screen base
	sta z_h
	lda #0
	sta z_l
	ldx #3			;$300 bytes total
	ldy #0
	
	lda #36			;Space
ClsAgain:
	sta (z_hl),y
	iny
	bne ClsAgain
	inc z_h
	dex 
	bne ClsAgain
	rts
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;x,y pos = bc / width+height = hl

docrop_alloffscreen:
	sec		;set carry = nothing to draw
	rts

;BC=X/Y pos HL=Width/Height, IY=source data
	
docrop:
	ldy #0				;Y=0 throughout this routine
	sty spritehclip
	sty z_d
	sty z_e				;e=top d=bottom crop

;crop top side
	lda z_c
	sec
	sbc #vscreenminy	;>minimum co-odinate
	bcs notcrop			;nc=nothing needs cropping
	jsr neg				;amount to remove
	clc
	adc #2
	and #%11111100		;Conv to Number of tiles
	cmp z_l			
	bcs docrop_alloffscreen	;all offscreen
	sta z_e				;amount to remove from top of source
	tya		;y=0		;draw from top of screen
notcrop:
	sta z_c				;draw ypos
	
;crop bottom hand side
	clc
	adc z_l				;add height
	sec
	sbc #vscreenhei-vscreenheiclip	;logical height of screen
	bcc nobcrop			;c=nothing needs cropping
	and #%11111100		;Conv to Number of tiles
	cmp z_l				;no pixels onscreen?
	bcs docrop_alloffscreen	;all offscreen
	sta z_d				;bottom crop
nobcrop:

;Calculate new height
	lda z_e			;units to remove from top
	clc				;units to remove from bottom
	adc z_d
	beq novclip		;nothing to remove?
	jsr neg
	clc
	adc z_l			;subtract from old height
	sta z_l			;new height


;remove lines from source bitmap (z_iy)
	lda z_e			;lines to remove from top
	lsr				;Convert to tiles
	lsr
	beq novclip		;any lines to remove from the top?
	tax
	
	lda z_h			;calc bytes per 2 lines 
	lsr				;(2 physical lines per logical y co-ord)
	lsr
	sta z_e
	
	lda z_iyl		
	clc
movedownaline:
	;remove lines from the top (start pos of source data)
	adc z_e			;Add E to L
	bcc movedownalineB
	inc z_iyh
	clc
movedownalineB:		
	dex
	bne movedownaline
	sta z_iyl

novclip:
	sty z_d			;DE=0
	sty z_e			;e=left d=right crop

;crop left hand side
	lda z_b
	sec
	sbc #vscreenminx 	;remove left virtual border
	bcs nolcrop			;nc=nothing needs cropping
	jsr neg
	cmp z_h				;no pixels onscreen?
	bcs docrop_alloffscreen	;all offscreen
	clc
	adc #3
	sta z_e				;left crop
	tya ;Y=0			;draw from left of screen
nolcrop:

;crop right hand side
	sta z_b				;draw xpos
	clc
	adc z_h				;add width
	sec
	sbc #vscreenwid-vscreenwidclip	;logical width of screen
	bcc norcrop			;c=nothing needs cropping
	cmp z_h				;no pixels onscreen?
	bcc  cropd
		jmp docrop_alloffscreen	;all offscreen
cropd:
	sta z_d				;right crop
norcrop:

;Calculate new width
	lda z_d				;units to remove from left
	clc
	adc z_e				;units to remove from right
	lsr 
	lsr
	beq nohclip			;nothing to crop?

	sta spritehclip	;number of horizontal bytes to skip
						;after each line
						
;remove bytes from source bitmap (z_iy)
	asl
	asl
	jsr neg
	clc
	adc z_h
	bne cropb
		jmp docrop_alloffscreen	;nothing to draw?
cropb:
	sta z_h				;width

	lda z_e				;amount to subtract from left
	lsr 
	lsr					;4 logical units per Tile 
	clc
	adc z_iyl
	sta z_iyl			;move across horizontal sprite.
	bcc nohclip
	inc z_iyh
nohclip:

;Convert to physical co-ords
	lsr z_b					;Quarter xpos (4 units per block)
	lsr z_b
	lsr z_h					;Quarter width (4 units per block) 
	lsr z_h

	lsr z_c					;Quarter Ypos
	lsr z_c
	lsr z_l					;Quarter Height
	lsr z_l

	clc 					;clear carry = crop ok
	rts


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	
		
neg:				;Negate a 
	eor #255
	clc
	adc #1
	rts
	
	
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
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
	
	

