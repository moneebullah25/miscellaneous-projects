; *** ALTERNATE sprite routine - allows single vline movement

VscreenMinX equ 64		;Top left of visible screen in logical co-ordinates
VscreenMinY equ 80


;LIMITATION.. The Virtual screen cannot be smaller than the sprite or 
;the crop will malfunction! (It can be the same size)

VscreenWid equ 128		;Visible Screen Size in logical units
VscreenHei equ 96

	ifndef UseStackMisuse
VscreenWidClip equ 0
	else
	
VscreenWidClip equ 2	;alter right boundary due to working in words
	endif	
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


RunLocation equ $0200

	ORG RunLocation  ;Actually our code runs at &3000 - but we shift it to here
BBCFirstByte:
	SEI			;Stop interrupts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
;Stop the sound chip making a noise!

	;&43 = Data Dir Reg A
	;&40 = I/O Reg B &40
	;&41 = I/O Reg A &41
	
	lda 255		;Set all bits to write
	sta $FE43 ; Data direction port
	
	;	  1CCOVVVV = CC=channel O=operation (1=volume) V=Value (Volume 15=off)
	lda #%10011111	;Turn off channel 0
	sta $FE41
		
	    ; ----BAAA   =A=address (0=sound chip, 3=Keyboard) B=new setting for address AAA
	lda #%00001000		;Send data to Sound Chip
	sta $FE40			
	lda #%00000000		;Stop sending data to sound chip
	sta $FE40
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Transfer program from load address ($3000) 
;To Run address $0200 (out the way of screen)
	
	lda #$30	;Source H $3000
	sta z_h
	
	lda #>(BBCLastByte-BBCFirstByte+256)
	sta z_b		;Byte count H
	
	lda #>RunLocation
	sta z_d		;Destination H $0200
	
	ldy #0		;Low byte of address
	sty z_l
	sty z_e

BBCLDIR:		
    lda (z_HL),Y
    sta (z_DE),Y
	iny
	BNE	BBCLDIR_SkipInc1
	INC	z_H ;Inc Ybytes of address
	INC	z_D
	DEC z_B
	BEQ	BBCLDIR_Done
BBCLDIR_SkipInc1:
	sec	;Relative jump (JR)
	bcs BBCLDIR	;this program code is relocated
BBCLDIR_Done:
;Jump to the new address in copied code ($0200)
	jmp start 
start:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;ScreenInit


	lda #$D8		;Mode 1
	sta $FE20		;Video ULA Control	
SendULA:
	ldx #0
NextULAreg	
	lda ULAConfig,X
	sta $FE21		;ULA Load in color config
	
	stx $FE00		;CRTC Reg Select
	lda CRTCConfig,X
	sta $FE01		;CRTC Reg Data
	
	inx
	cpx #16
	bne NextULAreg	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	
	
	
	
	lda #$41	;Clear $4180-$8080
	sta z_h
	lda #$80
	sta z_l
	
	ldx #$3F	;Clear $3F00 bytes
	ldy #$00
	lda #0
FillZeros:	
	sta (z_hl),y
	dey
	bne FillZeros
	inc z_h
	dex 
	bne FillZeros
	
	
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
	beq JoyNotUp	;Jump if UP not presesd
	dey
JoyNotUp:
	lda z_h
	and #%00000010	;---FRLDU
	beq JoyNotDown	;Jump if DOWN not presesd
	iny
JoyNotDown:
	lda z_h
	and #%00000100	;---FLRUD
	beq JoyNotLeft	;Move X Left 
	dex
JoyNotLeft:
	lda z_h
	and #%00001000	;---FLRUD
	beq JoyNotRight	;Move X Right
	inx
JoyNotRight:
	
	stx PlayerX		;Update X
	sty PlayerY		;Update Y
	
	
	stx z_b
	sty z_c
	;jsr cls
	
	jsr DrawPlayer	;Draw Player Sprite
	
	ldx #255
	ldy #20
	jsr PauseXY		;Wait a bit!	
	
	jmp infloop
	
PauseXY:
	dex
	bne PauseXY
	dey 
	bne PauseXY
	rts


	
	
	
cls:
	lda #$50			;$5000 screen base
	sta z_h
	lda #0
	sta z_l
	tay
	ldx #$30			;$3000 bytes
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
	
	
DrawPlayer:
	lda #<Bitmap	;Source Bitmap Data
	sta z_iyl
	lda #>Bitmap
	sta z_iyh

	lda #24			;Width in pairs of pixels (logical units)
	sta z_h
	lda #24			;Height in pairs of pixels (logical units)
	sta z_l
		
	jsr DoCrop		;Crop the sprite BC=XY pos 
	bcc DoDraw			;HL=WidthHeigh, IY=source data
	rts
		
		
;Crop the sprite BC=X/Y pos HL=Width/Height, IY=source data
DoDraw:	
	lda z_h	;Bytes per line (Width)
	asl
	asl
	asl
	sta z_h	;8 vert lines separate each horizontal block

	jsr GetScreenPos;Get screen pos from XY into Z_DE
	ldx #0
	jmp SpriteFirstLine
	
SpriteNextLine:			;This runs after the first line
	inc z_e				;Screen line ram down
	lda z_e
	and #%00000111		;not within 8 pixel strip
	bne NoRecalcNeededB
	sec
	lda z_e
	sbc #8				;Correct low part
	sta z_e

	inc z_d				;add 2 to high part
	inc z_d
NoRecalcNeededB:

	lda spritehclip	;Skip any offscren bytes to next line
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
	cmp z_h			;Width
	bne SpriteNextByte
	
	dec z_l
	bne SpriteNextLine	;Repeat for next line
	rts	
	
	
Bitmap:
		incbin "\ResALL\Sprites\SpriteCPC.raw"
BitmapEnd:
	

GetNextLine:
	inc z_e			;Screen line ram down
	lda z_e
	and #%00000111
	bne NoRecalcNeeded
	sec
	lda z_e
	sbc #8
	sta z_e

	inc z_d
	inc z_d
NoRecalcNeeded:
	rts

	

;BBC type is odd - the first 8 screen bytes go DOWN... 
;the 9ths goes back to the top 
;Effectively we're filling in 8x8 character blocks in a zigzag pattern

GetScreenPos:
		lda #0
		sta z_d
		
		lda z_b			;Xpos
		asl
		rol z_d		;2
		asl 
		rol z_d		;4
		asl 
		rol z_d		;8		;8 bytes per X line
		sta z_e
		
		;We have to work in 8 pixel tall strips on the BBC
		lda z_c			;Ypos
		and #%11111000	;Multiply Y strip num by $02				
		lsr			;$04 00
		lsr			;$02 00		
		
		adc #$50	;Screen Offset $5000
		adc z_d		;Add to D
		sta z_d
		
		
		lda z_c
		and #%00000111	;Add bottom bits of Y
		adc z_e
		sta z_e
	rts

	


Player_ReadControlsDual:;----LRUD
	lda #$F0					;Set port to read (For fire button)
	STA $FE43				;SN76489 - Data Direction
	sta z_h
	
	;lda #%00000000			;Get Channel 0 - Joy 1 LR
	jsr Player_ReadControlsGetData
	lda #%00000001			;Get Channel 1 - Joy 1 UD
	jsr Player_ReadControlsGetData
	rts
	
	;See page 429 of the 'BBC Microcomputer Advanced user Guide' 
Player_ReadControlsGetData:	;We need to convert analog to digital
	sta $FEC0						;Select channel
Player_ReadControlsDualWait:
	lda $FEC0						;Get Data
	and #%10000000
	bne Player_ReadControlsDualWait	;0= data ready
	
	lda $FEC1						;8 bit analog data
	cmp #255-32
	bcs Player_ReadControlsDualHigh
	cmp #32				
	bcc Player_ReadControlsDualLow 	;Centered
	clc
	bcc Player_ReadControlsDualB	;efective branch always
	
Player_ReadControlsDualLow:		;R/D
	sec
Player_ReadControlsDualB:
	rol z_h
	clc
	rol z_h
	rts
Player_ReadControlsDualHigh:	;U/L
	clc
	rol z_h
	sec
	rol z_h
	rts

	
	
	
	
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



docrop_alloffscreen
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
	bcs docrop_alloffscreen	;all offscreen?
	sta z_e				;amount to remove from top of source
	tya	;y=0			;draw from top of screen
notcrop:
	sta z_c				;draw ypos
	
;crop bottom side
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
	
	;lda z_h			;calc bytes per 2 lines 
	;sta z_e			;(2 physical lines per logical y co-ord)
	
	lda z_iyl
	clc
movedownaline:
	;remove lines from the top (start pos of source data)
	adc z_h ;z_e			;Add E to L
	bcc movedownalineB
	inc z_iyh
	clc
movedownalineB:		
	dex
	bne movedownaline
	sta z_iyl	
novclip:

	sty z_d		;Y=0
	sty z_e		;e=left d=right crop

;crop left hand side
	lda z_b
	sec
	sbc #vscreenminx 		;remove left virtual border
	bcs nolcrop				;nc=nothing needs cropping
	jsr neg					;Amount to remove
	cmp z_h					;no pixels onscreen?
	bcs docrop_alloffscreen
	clc
	adc #1
	sta z_e					;left crop
	tya	;y=0				;draw from left of screen
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
	beq nohclip			;nothing to crop?
	sta spritehclip	;number of horizontal bytes to skip
							;after each line
	
;remove lines from source bitmap (z_iy)
	asl
	jsr neg
	clc
	adc z_h				;Is whole width of sprite removed?
	bne cropb
		jmp docrop_alloffscreen
cropb:
	sta z_h				;new width

	lda z_e				;amount to subtract from left
	lsr 				;2 logical units per byte (4px)
	clc
	adc z_iyl
	sta z_iyl			;move across in source bitmap.
	bcc nohclip
	inc z_iyh
nohclip:

;Convert to physical co-ords
	lsr z_b				;halve xpos (4 pixels per byte)
	lsr z_h				;halve width (4 pixels per byte) 
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

	
CRTCConfig:
	db $7F		;0 - Horizontal total
	db $40		;1 - Horizontal displayed characters
	db $5A		;2 - Horizontal sync position
	db $28		;3 - Horizontal sync width/Vertical sync time
	db $26			;4 - Vertical total
	db $00			;5 - Vertical total adjust
	db 24			;6 - Vertical displayed characters (25)
	db 31			;7 - Vertical sync position
	db $01			;8 - Interlace/Display delay/Cursor delay
	db $07			;9 - Scan lines per character
	db %00110000	;10 - Cursor start line and blink type
	db $0			;11 - Cursor end line
	db $0A		;12 - Screen start address H (Address /8)
	db $00		;13 - Screen start address L 

	
ULAConfig:	
Palette0:	;Colours
;		SC  SC		-	S=Screen C=Color
	db $07,$17	;0
	db $47,$57	;0
Palette1:
	db $22,$32		;1
	db $62,$72		;1
Palette2:
	db $81,$91			;2
	db $C1,$D1			;2
Palette3:
	db $A0,$B0				;3
	db $E0,$F0				;3
	
;EOR True   Color
;7  (0) 	black
;6  (1) 	red
;5  (2) 	green
;4  (3) 	yellow (green—red)
;3  (4) 	blue
;2  (5) 	magenta (red—blue)
;1  (6) 	cyan (green—blue)
;0  (7) 	white

BBCLastByte: db 0