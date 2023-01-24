

VscreenMinX equ 48		;Top left of visible screen in logical co-ordinates
VscreenMinY equ 80

;VscreenWid equ 24		;Visible Screen Size in logical units
;VscreenHei equ 24

;LIMITATION.. The Virtual screen cannot be smaller than the sprite or 
;the crop will malfunction! (It can be the same size)

VscreenWid equ 80		;Visible Screen Size in logical units
VscreenHei equ 51

VscreenWidClip equ 0

VscreenHeiClip equ 0


;Screenbuffer at $D800-$F800 and $B800-$D800


;Current player pos
PlayerX 	equ $60		;Position of next printed character
PlayerY 	equ PlayerX+1

;Last player pos (For clearing sprite)
spritehclip 	equ PlayerX+2
ScreenBuffer	equ PlayerX+3


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


z_ixl equ z_Regs+8
z_ixh equ z_Regs+9
z_ix equ z_Regs+8

z_iyl equ z_Regs+10
z_iyh equ z_Regs+11
z_iy  equ z_Regs+10


	org $200-10		;Our program starts at $0200
	db $80,$08,$02,$00,$40,$0A,$42,$53,$39,$33
	
;ScreenInit	-	SUZY chip needs low byte setting first 
					;OR IT WILL WIPE THE HIGH BYTE!
	
	;Set screen ram pointer to $D800
	;Alt Screenbuffer at $B800
	lda #$D8	
	sta ScreenBuffer
	jsr FlipScreenBuffer
	
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
	




	lda #VscreenMinX	;Start SX
	sta PlayerX
	lda #VscreenMinY	;Start SY
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
		
		jsr cls	;Remove old player sprite
		
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
	
	stx z_b
	sty z_c
	
	jsr DrawPlayer	;Draw Player Sprite
	jsr FlipScreenBuffer
	
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

	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
	
DrawPlayer:
	lda #<Bitmap	;Source Bitmap Data
	sta z_iyl
	lda #>Bitmap
	sta z_iyh
	
	lda #24			;Width in pairs of pixels 
	sta z_h				;(logical units)
	lda #24		;Height in pairs of pixels (logical units)
	sta z_l

	jsr DoCrop			;Crop the sprite BC=XY pos HL=WidthHeigh, 
	bcc DoDraw				;IY=source data
	
		rts				;All Offscreen - nothing to draw
DoDraw:	
		
DrawSprite:
	jsr GetScreenPos	;Get screen pos from XY into Z_DE
	ldx z_l

BitmapNextLine:
		ldY z_h
		dey
BitmapNextByte:
		lda (z_iy),Y		;Copy a byte from the source 
		sta (z_de),Y		;to the destination
			
		dey					;Repeat for next byte of line
		bpl BitmapNextByte  ;repeat while >0

		
		clc
		lda z_h
		adc z_iyl			;ADD Width to Z_IY to move source 
		sta z_iyl
		bcc BitmapNext_NoIYHinc
		inc z_iyh
		clc
BitmapNext_NoIYHinc

		lda spritehclip		;Skip any horizontal cropped bytes
		beq NoSpriteClip
		adc z_iyl
		sta z_iyl
		bcc NoSpriteClip
		inc z_iyh
		clc
NoSpriteClip:		

		lda z_e
		adc #$50			;ADD 50 to Z_DE to move down 1 line
		sta z_e
		bcc BitmapNext_NoDinc
		inc z_d
BitmapNext_NoDinc
	 
	dex			;Check if we've done all the lines
	bne BitmapNextLine		;Repeat until we have
	rts
	

	
Bitmap:
	incbin "\ResALL\Sprites\RawMSX.RAW"
BitmapEnd:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
Cls:
	lda ScreenBuffer	;$B800/$D800 screen base
	sta z_h
	lda #0
	sta z_l
	ldx #$20			;$2000 bytes total
	tay
ClsAgain:
	sta (z_hl),y
	iny
	bne ClsAgain
	inc z_h
	dex 
	bne ClsAgain
	rts
	
FlipScreenBuffer:
	lda ScreenBuffer
	stz $FD94		;DISPADR	Display Address L (Visible)
	sta $FD95		;DISPADR	Display Address H (Visible)		
	
	eor #%01100000	;$D800 /$B800
	sta ScreenBuffer
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	

	;Y= $50 bytes per Yline = 00000000 01010000
	;Move Y into top byte 	= YYYYYYYY 00000000
	;Shift Right Twice      = 00YYYYYY YY000000
	;Shift Right Twice      = 0000YYYY YYYY0000
	
GetScreenPos:
	
	
	lda z_c 			;Move Y into top byte 	= YYYYYYYY 00000000
	
	stz z_c
	
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
	adc ScreenBuffer	;Screen base at &C0000
	sta z_d

	
	lda z_c			;Add Low byte to total
	adc z_e
	sta z_e
	
	lda z_d			;Add any carry to the high byte
	adc #0
	sta z_d
	
	clc				;Add the X pos 
	lda z_b
	adc z_e 
	sta z_e
	
	lda z_d			;Add any carry to the high byte
	adc #0
	sta z_d
	rts
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
;x,y pos = bc / width+height = hl

docrop_alloffscreen:
	sec					;set carry = nothing to draw
	rts
	
;BC=X/Y pos HL=Width/Height, IY=source data

docrop:
	ldy #0				;Y=0 throughout this routine
	sty spritehclip
	sty z_d
	sty z_e				;e=top d=bottom crop

;crop top side
	lda z_c				;Ypos
	sec
	sbc #vscreenminy	;>minimum co-odinate
	bcs notcrop			;nc=nothing needs cropping
	jsr neg
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
	lda z_e				;units to remove from top
	clc					;units to remove from bottom
	adc z_d
	beq novclip			;nothing to remove?
	
	jsr neg
	clc
	adc z_l				;subtract from old height
	sta z_l				;new height

	
	
;remove lines from source bitmap (z_iy)	
	lda z_e				;lines to remove from top
	beq novclip			;any lines to remove from the top?
	tax
	
	lda z_h				;calc bytes per 2 lines 
	asl					;(2 physical lines per logical y co-ord)
	sta z_e
	
	lda z_iyl
	clc
movedownaline:
	;remove lines from the top (start pos of source data)
	adc z_e				;Add E to L
	bcc movedownalineB
	inc z_iyh
	clc
movedownalineB:		
	dex
	bne movedownaline
	sta z_iyl	
novclip:



	sty z_d
	sty z_e				;e=left d=right crop

;crop left hand side
	lda z_b				;Xpos
	sec
	sbc #vscreenminx 	;remove left virtual border
	bcs nolcrop			;nc=nothing needs cropping
	jsr neg
	cmp z_h				;no pixels onscreen?
	bcs docrop_alloffscreen	;all offscreen
cropc:
	sta z_e				;left crop
	tya ;Y=0			;draw from left of screen
nolcrop:
	sta z_b				;draw xpos
	
	
	
;crop right hand side	
	clc
	adc z_h				;add width
	sec
	sbc #vscreenwid-vscreenwidclip	;logical width of screen
	bcc norcrop			;c=nothing needs cropping
	cmp z_h				;no pixels onscreen?
	bcs docrop_alloffscreen	;all offscreen
cropd:
	sta z_d				;right crop	
norcrop:


;Calculate new width
	lda z_d				;units to remove from left
	clc
	adc z_e				;units to remove from right
	beq nohclip			;nothing to crop?

	sta spritehclip	;number of horizontal bytes to skip
						;after each line

;Remove lines from source bitmap z_IY
	jsr neg
	clc
	adc z_h				;New Width
	bne cropb
		jmp docrop_alloffscreen	;nothing to draw?
cropb:
	sta z_h				;width

	lda z_e				;amount to subtract from left
	clc
	adc z_iyl
	sta z_iyl			;move across in source bitmap.
	bcc nohclip
	inc z_iyh
nohclip:


	asl z_c			;double ypos (2 lines per logical unit)
	asl z_l			;double height (2 lines per logical unit)

	clc				;clear carry = crop ok
	rts


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		
neg:				;Negate a 
	eor #255
	clc
	adc #1
	rts
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

