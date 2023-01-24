;DoubleWidth equ 1 ;For 80x25 pet


VscreenMinX equ 48		;Top left of visible screen in logical co-ordinates
VscreenMinY equ 80

;VscreenWid equ 24		;Visible Screen Size in logical units
;VscreenHei equ 24

;LIMITATION.. The Virtual screen cannot be smaller than the sprite or 
;the crop will malfunction! (It can be the same size)

VscreenWid equ 160		;Visible Screen Size in logical units
VscreenHei equ 100

VscreenWidClip equ 0
VscreenHeiClip equ 0

	 

;Current player pos
PlayerX 	equ $40		
PlayerY 	equ PlayerX+1
FrameBase	equ PlayerX+2
FrameNo		equ PlayerX+3
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


*=$0401
	db $0e,$04,$0a,$00,$9e,$20,$28, $31,$30,$34,$30,$29,$00,$00,$00
	
	
	SEI						;Stop interrupts
	jsr Cls					;Clear the screen
	
	
	;lda #12
	;sta $E84C	;Enable Graphics on later PETs (12=Gra 14=Lower)

		
	lda #0
	sta FrameNo
	sta FrameBase
	
	ldx #VscreenMinX			;Start SX
	stx PlayerX
	ldy #VscreenMinY			;Start SY
	sty PlayerY
	
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
		
		ldy PlayerY		;Back up Y
		
		stx z_b
		sty z_c
		jsr BlankPlayer	;Remove old player sprite
		
		ldx PlayerX		;Back up X
		ldy PlayerY		;Back up Y
		
	pla
	sta z_h
	and #%00000001	;---FRLDU
	bne JoyNotUp	;Jump if UP not presesd
	dey
	lda #4
	sta FrameBase	;Up/Down Frames
JoyNotUp:
	lda z_h
	and #%00000010	;---FRLDU
	bne JoyNotDown	;Jump if DOWN not presesd
	iny
	lda #4
	sta FrameBase	;Up/Down Frames
JoyNotDown:
	lda z_h
	and #%00000100	;---FRLDU
	bne JoyNotLeft	;Move X Left 
	dex
	lda #2
	sta FrameBase	;Left Frames
JoyNotLeft:
	lda z_h
	and #%00001000	;---FRLDU
	bne JoyNotRight	;Move X Right
	inx
	lda #0
	sta FrameBase	;Right Frames
JoyNotRight:
	
	stx PlayerX		;Update X
	sty PlayerY		;Update Y
	
	lda FrameNo
	eor #%00000001	;Loop 4 frames
	sta FrameNo
	
	stx z_b
	sty z_c
	jsr DrawPlayer	;Draw Player Sprite
	
	ldx #255
	ldy #20
PauseXY:
	dex
	bne PauseXY
	dey 
	bne PauseXY
	
	jmp infloop
	
	
	
	
	
	
	
BlankPlayer:		
	lda #<BlankBitmap
	sta z_iyl
	lda #>BlankBitmap
	sta z_iyh
	jmp DrawBoth
	
DrawPlayer:		
	lda FrameNo		;Frame of anim
	clc
	adc FrameBase	;Anim bank
	asl
	asl
	asl
	asl				;16 bytes per frame
	clc
	adc #<Bitmap	;Add address of bitmaps
	sta z_iyl
	
	lda #>Bitmap
	adc #0			;Add carry from low byte
	sta z_iyh
	
DrawBoth:	
	lda #16			
	sta z_h			;Wid
	lda #16
	sta z_l			;Hei
	
ShowBitmap:	;Show Zero terminated 'bitmap' from z_HL to z_DE
				;Size (W,H) z_B,z_C 
	jsr DoCrop	;Crop the sprite BC=XY pos HL=WidthHeight,
	bcc DoDraw		;IY=source data
		rts
DoDraw:				

	jsr GetVDPScreenPos		;Get Vram Address in z_DE
ShowNextLine:	
	ldx z_h					;Width
	ldy #0
ShowBitmapH:	
	lda (z_iy),y			;Transfer one char
	sta (z_de),y
	iny
	dex						
	bne ShowBitmapH			;next Char
	
;Next Dest line
	clc
	ifdef DoubleWidth
		lda #80				;Move Dest down 1 line
	else
		lda #40				;Move Dest down 1 line
	endif
	adc z_e
	sta z_e					;Update Low byte
	bcc DestDok
		inc z_d				;Update High byte with carry
		clc
DestDok:	

;Next Bitmap line	
		lda spritehclip		;Bytes to skip after each line
		beq NoSpriteClip
		clc
		adc z_iyl
		sta z_iyl
		bcc NoSpriteClip
		inc z_iyh
NoSpriteClip:
	
	tya						;Add bytes shown during last line
	clc
	adc z_iyl
	sta z_iyl
	bcc NextLine
		inc z_iyh
		clc
NextLine:	
	dec z_l
	bne ShowNextLine		;Next line
	rts	

	
;$8000 + Ypos *40 + Xpos		Ypos*40=32+8
	
GetVDPScreenPos:	; Get Screen address BC=XYpos in z_DE
	lda #0
	sta z_d
	
	lda z_c			
	asl
	asl
	asl
	sta z_e			;Ypos *8
	asl
	rol z_d
	asl
	rol z_d
	adc z_e
	sta z_e			;Ypos * 32 
	
	lda z_d
	adc #$80		;Screen base is $8000
	sta z_d
	
	clc
	lda z_e
	adc z_b
	sta z_e
	bcc GetVDPScreenPos_Done
		inc z_d
GetVDPScreenPos_Done:
	rts
	
	

	
Bitmap:			;4x4 - 4 frames
	incbin "\ResAll\Sprites\AnimPET.RAW"
BlankBitmap:
	db 32,32,32,32		;Empty 4x4
	db 32,32,32,32
	db 32,32,32,32
	db 32,32,32,32
	
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
	

Cls:
	lda #$80		;$8000 screen base
	sta z_h
	lda #0
	sta z_l
	ldx #4			;$400 bytes total
	ldy #0
	
	lda #32			;Space
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
	lda z_c				;Ypos
	sec
	sbc #vscreenminy	;>minimum co-odinate
	bcs notcrop			;nc=nothing needs cropping
	jsr neg
	clc
	adc #1
	and #%11111100		;We can only work in Single Chars
	cmp z_l			
	bcs docrop_alloffscreen	;all offscreen
	sta z_e				;amount to remove from top of source
	tya		 ;lda #0	;draw from top of screen
notcrop:
	sta z_c				;draw ypos

;crop bottom hand side
	clc
	adc z_l				;add height
	sec
	sbc #vscreenhei-vscreenheiclip	;logical height of screen
	bcc nobcrop			;c=nothing needs cropping
	and #%11111100
	cmp z_l				;no pixels onscreen?
	bcs docrop_alloffscreen	;all offscreen
	sta z_d				;bottom crop
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

	lda z_e				;lines to remove from top
	lsr
	lsr
	beq novclip			;any lines to remove from the top?
	tax
	
	lda z_h				;calc bytes per 2 lines
	lsr					;(2 physical lines per logical y co-ord)
	lsr
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
	sty z_e				;e=top d=bottom crop

;crop left hand side
	lda z_b
	sec
	sbc #vscreenminx 	;remove left virtual border
	bcs nolcrop			;nc=nothing needs cropping
	jsr neg
	cmp z_h				;no pixels onscreen?
	bcc cropc
	jmp docrop_alloffscreen	;all offscreen
cropc:
	clc
	adc #3
	sta z_e				;left crop
	tya	;y=0			;draw from left of screen
nolcrop:
	sta z_b				;draw xpos

;crop right hand side
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

	lda z_d				;units to remove from left
	clc
	adc z_e				;units to remove from right
	lsr 
	lsr
	beq nohclip			;nothing to crop?

	sta spritehclip	;number of horizontal bytes to skip
						;after each line
	asl
	asl					;Tile Count
	jsr neg
	clc
	adc z_h
	bne cropb
		jmp docrop_alloffscreen	;nothing to draw?
cropb:
	sta z_h				;New width

	lda z_e				;amount to subtract from left
	lsr
	lsr
	clc
	adc z_iyl
	sta z_iyl			;move across horizontal sprite.
	bcc nohclip
		inc z_iyh
nohclip:

hclipdone:
	lsr z_b				;Quarter xpos (4 units per block)
	lsr z_b
	lsr z_h				;Quarter width (4 units per block) 
	lsr z_h

	lsr z_c				;Quarter Ypos
	lsr z_c

	lsr z_l				;Quarter Height
	lsr z_l

	clc 				;clear carry = crop ok
	rts


	
		
neg:				;Negate a 
	eor #255
	clc
	adc #1
	rts
	
	
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	