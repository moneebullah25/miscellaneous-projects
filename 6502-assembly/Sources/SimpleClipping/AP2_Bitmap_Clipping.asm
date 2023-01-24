FourColor equ 1


VscreenMinX equ 48		;Top left of visible screen in logical co-ordinates
VscreenMinY equ 80

;VscreenWid equ 24		;Visible Screen Size in logical units
;VscreenHei equ 24

;LIMITATION.. The Virtual screen cannot be smaller than the sprite or 
;the crop will malfunction! (It can be the same size)

VscreenWid equ 160		;Visible Screen Size in logical units
VscreenHei equ 96

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


	ifdef BuildA80		;Atari 800 settings
GTIA equ $D000			;GTIA address
PIA  equ $D300			;PIA address
	org $A000     	  	;Start of cartridge area
	
	else				;Atari 5200 settings
GTIA  equ $C000			;GTIA address
POKEY equ $E800			;POKEY address
	org $4000       	;Start of cartridge area	
	endif
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
	ORG $0C00	;Program Start
	sei 		;Disable interrupts
	
	lda $C050 	;TXTCLR:   Display Graphics
	lda $C052 	;MIXCLR:   Display Full Screen
	lda $c057 	;HIRES:    Display HiRes Graphics
	lda $C055 	;TXTPAGE2: If 80STORE Off: Display Page 2
	

	
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
	and #%00001111
	cmp #%00001111
	beq infloop		;See if no keys are pressed
	pha
StartDraw:	
	
	
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
	ldy #10
	jsr PauseXY		;Wait a bit!	
		
	jmp infloop


PauseXY:
	dex
	bne PauseXY
	dey 
	bne PauseXY
	rts


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
DrawPlayer:
	lda #<Bitmap		;Source Bitmap Data
	sta z_iyl
	lda #>Bitmap
	sta z_iyh

	lda #32				;Width in pairs of pixels 
	sta z_h				;(logical units)
	
	lda #24				;Height in pairs of pixels 
	sta z_l				;(logical units)

	jsr DoCrop			;Crop the sprite BC=XY pos HL=WidthHeigh, 
	bcc DoDraw				;IY=source data
	
		rts				;All Offscreen - nothing to draw
DoDraw:	
	jsr GetScreenPos	;Get screen pos from XY into Z_DE
	jmp SpriteFirstLine


	;Screen layout is split in 3 parts according to Y line
	;AABBBCCC - AA*$0028  BBB*$0080  CCC*$0400		
SpriteNextLine:	
	tya			;INC source byte
	clc
	adc z_iyl
	sta z_iyl
	bcc NextLine
		inc z_iyh
		clc
NextLine:				;X=High Byte    Y=Low Byte
	lda z_d				
	adc #%00000100		;CCC*$0400
	tax					
		and #%00011100
		bne NextLineDone
	txa
	adc #-%00100000		;Fix top byte for CCC overflow
	tax
	lda z_e
	clc
	adc #%10000000		;bbB*$0080
	tay
	bcc NextLineDoneY
	inx
	txa
	and #%00000011		;BBb*$0080
	bne NextLineDoneY
	txa
	sbc #%00000100
	tax
	tya
		clc
		adc #$28		;AA*$0028
	tay
NextLineDoneY:	
	sty z_e
NextLineDone:	
	stx z_d
	
NoRecalcNeededB:
		lda spritehclip
		beq NoSpriteClip
		clc
		adc z_iyl		;Remove any unused bytes
		sta z_iyl			;of our sprite from left/right
		bcc NoSpriteClip
		inc z_iyh
NoSpriteClip:

SpriteFirstLine:
	ldy #0
	ldx z_h				;Width (Bytes)
SpriteNextByte:
	lda (z_iy),y		;Source Data
	eor (z_de),y		;XOR Sprite
	sta (z_de),y		;Screen Destination
	iny
	dex
	bne SpriteNextByte
	dec z_l
	bne SpriteNextLine		;Repeat for next line
	rts						;Finished
	
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	

	;Screen layout is split in 3 parts according to Y line
	;AABBBCCC - AA*$0028  BBB*$0080  CCC*$0400
GetScreenPos:
	lda #0
	sta z_e
	lda z_c 				;--BBB---	;Multiply by $0080
	and #%00111000
	lsr
	lsr
	lsr					
	lsr					;Shift 1 bit right 
	ror z_e
	adc #$40			;Screen base
	sta z_d
	lda z_c					;AA------		;multiply by $0028
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
	lda z_c 			;-----CCC	;Multiply by 4
	and #%00000111
	asl
	asl
	adc z_d
	sta z_d
	
	lda z_b				;Process X
	clc
	adc z_e			;Add X to calculated address
	sta z_e
	rts
	
	


cls:
	ldy #$40			;$4000 screen base
	ldx #$20			;$2000 bytes
	lda #0
	
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
	incbin "\ResAll\Sprites\RawAP2.RAW"
	incbin "\ResAll\Sprites\RawAP2_4col.RAW"
BitmapEnd:


	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
	
	lda z_h		;calc bytes per 2 lines (2 physical lines per logical y co-ord)
	lsr
	sta z_e
	
;4 pixels per byte on cpc (2 in virtual co-ords)
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
	lda z_d				;units to remove from left
	clc
	adc z_e				;units to remove from right
	lsr 
	lsr
	beq nohclip			;nothing to crop?

	sta spritehclip	;number of horizontal bytes to skip
						;after each line

	asl
	asl
	jsr neg
	clc
	adc z_h				;New Width
	bne cropb
		jmp docrop_alloffscreen	;nothing to draw?
cropb:
	sta z_h				;width

	lda z_e				;amount to subtract from left
	lsr 
	lsr					;4 logical units per byte 
	clc
	adc z_iyl
	sta z_iyl			;move across in source bitmap.
	bcc nohclip
	inc z_iyh
nohclip:


	lsr z_b			;Quarter xpos (2 pixels per byte)
	lsr z_b
	lsr z_h			;Quarter width (2 pixels per byte) 
	lsr z_h

	asl z_c			;double ypos (2 lines per logical unit)
	asl z_l			;double height (2 lines per logical unit)

	clc				 ;clear carry = crop ok
	rts


		
neg:				;Negate a 
	eor #255
	clc
	adc #1
	rts
	
	
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
;Apple Joysticks are annoying!
;they are analog... we have to strobe the port 
;then read from the X and Y ports, and count up until the top bit changes
;this is a 'timer'...using just 1 bit (the top one) 
;it effectively returns an 'analog' value from about 0-100
	
Player_ReadControlsDual:	;---FRLDU
	lda $C061			;Fire 1
	and #%10000000
	rol	;Move in the fire button
	rol z_h
	
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
	jsr JoyConvertAnalog;Convert Y
	lda z_h
	eor #%11111111
	sta z_h
	rts
	
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
	
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	


		
	