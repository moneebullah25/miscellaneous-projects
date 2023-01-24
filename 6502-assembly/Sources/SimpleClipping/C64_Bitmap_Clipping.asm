;We're using 4x8 tiles on the C64.

VscreenMinX equ 48		;Top left of visible screen in logical co-ordinates
VscreenMinY equ 78

;LIMITATION.. The Virtual screen cannot be smaller than the sprite or 
;the crop will malfunction! (It can be the same size)

VscreenWid equ 160		;Visible Screen Size in logical units
VscreenHei equ 100

VscreenWidClip equ 0
VscreenHeiClip equ 0



;Current player pos
PlayerX 	equ $60		
PlayerY 	equ PlayerX+1

spritehclip equ PlayerX+4

	 
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

z_As equ z_Regs+6
z_Hs equ z_Regs+7

z_ixl equ z_Regs+8
z_ixh equ z_Regs+9
z_ix equ z_Regs+8

z_iyl equ z_Regs+10
z_iyh equ z_Regs+11
z_iy  equ z_Regs+10


;Init Routine
*=$0801
	db $0E,$08,$0A,$00,$9E,$20,$28,$32,$30,$36,$34,$29,$00,$00,$00  
*=$0810	;Start at $0810

	sei

	;	  LXMSHVVV - L=Cur Line X=extended BG M=mode 
				;(Txt/Bmp) S=screen on H=height V=Vert scroll
	lda #%00111011	;turn on graphics mode
	sta $D011

	
	;     ---MWHHH - M=Multicolor W=scr width H=horiz scroll
	lda #%11011000  ;1=Multicolor 4 coor 

	sta $D016

	;     SSSSTTT- - T=Text/Bmp screen address S=Screen (color) address
	lda #%00011000  ;T=1 Screen at $2000 
							;(Other bits have no function in bitmap mode)
	sta $D018		
	
	;	  ----CCCC
	lda #%00000000
	sta $D021		;Background color (only bits #0-#3).	
	sta $D020		;Border 

	jsr cls
	
	ldx #VscreenMinX			;Start SX
	stx PlayerX
	ldy #VscreenMinY			;Start SY
	sty PlayerY
	
	stx z_b
	sty z_c
	jsr DrawPlayer	;Draw Player Sprite
	
infloop:
	jsr Player_ReadControlsDual
	lda z_h
	cmp #%11111111
	beq infloop		;See if no keys are pressed
	pha		
		ldx PlayerX		;Back up X
		ldy PlayerY		;Back up Y
		stx z_b
		sty z_c
		jsr DrawPlayer	;Draw Player Sprite
			
		ldx PlayerX		;Back up X
		ldy PlayerY		;Back up Y
	pla
	sta z_h
	and #%00000001	;---FLRUD
	bne JoyNotUp	;Jump if UP not presesd
	dey
JoyNotUp:
	lda z_h
	and #%00000010	;---FRLDU
	bne JoyNotDown	;Jump if DOWN not presesd
	iny
JoyNotDown:
	lda z_h
	and #%00000100	;---FLRUD
	bne JoyNotLeft	;Move X Left 
	dex
JoyNotLeft:
	lda z_h
	and #%00001000	;---FLRUD
	bne JoyNotRight	;Move X Right
	inx
JoyNotRight:
	
	stx PlayerX		;Update X
	sty PlayerY		;Update Y
	
	stx z_b
	sty z_c
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
	;lda $DC00			;Read in Joystick 1
	lda $DC01			;Read in Joystick 2
	sta z_h			;%---FRLDU
	rts
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DrawPlayer:
	lda #<Bitmap	;Source Bitmap Data
	sta z_iyl
	lda #>Bitmap
	sta z_iyh

	lda #24			;Width in pairs of pixels (logical units)
	sta z_h
	lda #24			;Height in pairs of pixels (logical units)
	sta z_l

	jsr DoCrop	;Crop the sprite BC=XY pos HL=WidthHeigh, IY=source data
	bcc DoDraw
	rts
		
DoDraw:	
	lda z_h			;Bytes per line (Width)
	asl
	asl
	asl
	sta z_h	;8 vert lines separate each horizontal block
	
	jsr GetScreenPos;Get screen pos from XY into Z_DE
	ldx #0
	jmp SpriteFirstLine
	
SpriteNextLine:
	inc z_e				;Screen line ram down
	lda z_e
	and #%00000111		;Not within 8 pixe strip?
	bne NoRecalcNeededB
	lda z_e
	bne NotZero			
	inc z_d				;Update Top Byte
NotZero:		
	inc z_d				;Each strip is 40*8=$0140
	clc
	adc #$40-8			
	sta z_e
	bcc NoRecalcNeededB
	inc z_d
NoRecalcNeededB:

	lda spritehclip 	;Skip any offscren bytes to next line
	beq NoSpriteClip
	clc
	adc z_iyl
	sta z_iyl
	bcc NoSpriteClip
	inc z_iyh
NoSpriteClip:

SpriteFirstLine:
	clc
	ldy #0
SpriteNextByte:
	lda (z_iy,x)	;Sprite Source
	eor (z_de),y	;XOR Sprite
	sta (z_de),y	;Screen Destination
	
	inc z_iyl		;Update Sprite source +1 Horizontal byte
	bne DrawSpriteB
	inc z_iyh	
DrawSpriteB:		
	tya
	adc #8			;8 lines per block (+8 to move across)
	tay
	cmp z_h
	bne SpriteNextByte
	
	dec z_l
	bne SpriteNextLine	;Repeat for next line	
	rts		
	
	
	
	
;Address= (X * 8) + (Top5BitsOfY * 40) + $2000
GetScreenPos:
	lda #0
	sta z_d
	
	lda z_b			;Multiple X by 8
	asl				;-------- XXXXXXXX
	rol z_d
	asl
	rol z_d
	asl
	rol z_d			;-----XXX XXXXX---
	sta z_e

	lda #0
	sta z_b
	
	lda z_c
	and #%00000111
	adc z_e
	sta z_e
	
;40 bytes per Yline =00000000 00101000
	lda z_c
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
	adc #$20		;Screen Base $2000
	sta z_d
	rts
	
;Color Ram data at $D800 & $400
;Address = $0400+ Y Strip * 40 + Xpos

				;z_DE = $0400-07FF byte  
				;z_BC = $D800-DBFF byte (for 4 color)


cls:
	ldy #$20			;$5000 screen base
	ldx #$20			;$2000 bytes
	lda #0
	jsr clsPart
	
	ldy #$04			;$0400 Color ram
	ldx #$04			;$0400 Bytes
	lda #$43
	jsr clsPart
	ldy #$D8			;$D800 Color ram
	ldx #$04			;$0400 Bytes
	lda #$01
clsPart:	
	pha
		sty z_h
		lda #0
		sta z_l
		tay
	pla 
clsB:	
	sta (z_hl),y
	iny
	sta (z_hl),y
	iny
	sta (z_hl),y
	iny
	sta (z_hl),y
	iny
	sta (z_hl),y
	iny
	sta (z_hl),y
	iny
	sta (z_hl),y
	iny
	sta (z_hl),y
	iny
	bne clsB
	inc z_h
	dex 
	bne clsB
	rts				
				
		
		
Bitmap:
		incbin "\ResALL\Sprites\RawA52.RAW"
BitmapEnd:


	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

docrop_alloffscreen:
	sec		;set carry = nothing to draw
	rts
	
;BC=X/Y pos HL=Width/Height, IY=source data

docrop:
	ldy #0		;Y=0 throughout this routine
	sty spritehclip
	sty z_d
	sty z_e		;e=top d=bottom crop

;crop top side
	lda z_c
	sec
	sbc #vscreenminy	;>minimum co-odinate
	bcs notcrop			;nc=nothing needs cropping
	jsr neg				;amount to remove
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
	cmp z_l				;no pixels onscreen?
	bcs docrop_alloffscreen	;all offscreen
	sta z_d				;amount to remove from bottom 

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
	beq novclip		;any lines to remove from the top?
	tax
	
	lda z_h			;calc bytes per 2 lines 
	lsr				;(2 physical lines per logical y co-ord)
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
	jsr neg				;Amount to remove
	cmp z_h				;no pixels onscreen?
	bcs docrop_alloffscreen	;all offscreen
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
	bcs docrop_alloffscreen	;all offscreen
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

;remove lines from source bitmap (z_iy)
	asl
	asl
	jsr neg
	clc
	adc z_h

	bne cropb
		jmp docrop_alloffscreen	;nothing to draw?
cropb:
	sta z_h				;new width

	lda z_e				;amount to subtract from left
	lsr 
	lsr					;4 logical units per byte 
	clc
	adc z_iyl
	sta z_iyl			;move across in source bitmap.
	bcc nohclip
	inc z_iyh
nohclip:

;Convert to physical co-ords
	lsr z_b				;Quarter xpos (2 pixels per byte)
	lsr z_b
	lsr z_h				;Quarter width (2 pixels per byte) 
	lsr z_h

	asl z_c				;double ypos (2 lines per logical unit)
	asl z_l				;double height (2 lines per logical unit)

	clc 				;clear carry = crop ok
	rts


		
neg:				;Negate a 
	eor #255
	clc
	adc #1
	rts
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	