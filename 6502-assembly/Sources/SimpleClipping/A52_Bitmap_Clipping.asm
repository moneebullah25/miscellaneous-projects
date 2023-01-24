FourColor equ 1


VscreenMinX equ 48		;Top left of visible screen in logical co-ordinates
VscreenMinY equ 80


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
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	INIT

ProgramStart:        
	sei             ;Disable interrupts
	
    ldx #$00		;Zero GTIA area and Zero Page
    txa
ClearLoop    
    sta $00,x   	;Clear zero page
    sta GTIA,x      ;Clear GTIA
    dex
    bne ClearLoop
		
	lda #<DisplayList
	sta $D402 		;DLISTL - Display list lo
	lda #>DisplayList
	sta $D403 		;DLISTH - Display list hi
	
	lda #%00100010   	
	sta $D400 		;DMACTL - DMA Control (screen on)

	ifdef FourColor
		lda #$98      	;Set color PF1 (foreground) (CYAN)
		sta GTIA+ $17 	;COLPF1 equ 
		
		lda #$0F       	;Set color PF2 (background) (White)
		sta GTIA+ $18	;COLPF2 

		lda #$68        ;Set color PF0 (Purple)
		sta GTIA+ $16
		
		lda #$00       ;Set color PF0 (Black)
		sta GTIA+ $1A
	else
		lda #$0F
		sta GTIA+ $17 	;COLPF1 equ 		
		
		lda #$00        ;2 color mode only uses the brightness of color1
		sta  GTIA+ $16
		sta  GTIA+ $18
		sta  GTIA+ $1A	
	endif

	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	Your Code Goes Here		

		
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

	lda #24				;Width in pairs of pixels (logical units)
	sta z_h
	lda #24				;Height in pairs of pixels (logical units)
	sta z_l

	jsr DoCrop			;Crop the sprite BC=XY pos HL=WidthHeigh,
							;IY=source data
	bcc DoDraw
		rts				;All Offscreen - nothing to draw
DoDraw:	
		
	
DrawSprite:
	jsr GetScreenPos	;Get screen pos from XY into Z_DE
	jmp SpriteFirstLine
	
SpriteNextLine:
	clc
	lda z_e
	adc #$28			;Move down a line (40 Bytes / $0028)
	sta z_e
	bcc SpriteNextLineDone
		inc z_d
SpriteNextLineDone:

	tya					
	clc
	adc z_iyl			;INC source byte by amount copied
	sta z_iyl	
	bcc NoRecalcNeededB
		inc z_iyh		
NoRecalcNeededB:

	lda spritehclip
	beq NoSpriteClip
	clc
	adc z_iyl			;Remove any unused bytes
	sta z_iyl				;of our sprite from left/right
	bcc NoSpriteClip
	inc z_iyh
NoSpriteClip:

SpriteFirstLine:
	ldy #0
	ldx z_h
SpriteNextByte:
	lda (z_iy),y		;Source Data
	eor (z_de),y		;XOR Sprite
	sta (z_de),y		;Screen Destination
	iny
	dex
	bne SpriteNextByte
	dec z_l
	bne SpriteNextLine	;Repeat for next line
	rts					;Finished
	
	
		;40 bytes per line = * %00000000 00101000
	;We shift our Ypos 3 to the right, add it, then another 2, eg
	
	;%00000000 00101000	=40 (32+8)
	;%YYYYYYYY 00000000
	;%000YYYYY YYY00000	= Y*32
	;%00000YYY YYYYY000 = Y*8
GetScreenPos:
	lda z_b
	sta z_e			;Store X pos in E
	
	lda z_c		;Get Ypos - store in B
	sta z_b			;%YYYYYYYY 00000000		
	
	lda #0
	lsr z_b			;Shift 3 Bits
	ror 			;%0YYYYYYY Y0000000
	lsr z_b
	ror 			;%00YYYYYY YY000000
	lsr z_b
	ror 			;%000YYYYY YYY00000 = Y*32
	tax	
		clc
		adc z_e		;Update Low Byte
		sta z_e
		
		lda #0 		;Update High Byte
		adc z_b
		sta z_d
	txa
	lsr z_b			;Shift 2 bits
	ror 			;%0000YYYY YYYY0000
	lsr z_b
	ror 			;%00000YYY YYYYY000 = Y*8 
		
	adc z_e			;Add to Update Low Byte
	sta z_e
	
	
	lda z_b			;Add to Update High Byte
	adc z_d
	sta z_d	

	clc
	lda z_e
	adc #$60
	sta z_e
	lda z_d			;Add Screen Base ($2060)
	adc #$20
	sta z_d		
	rts
	
	
;Color Ram data at $D800 & $400
;Address = $0400+ Y Strip * 40 + Xpos

				;z_DE = $0400-07FF byte  
				;z_BC = $D800-DBFF byte (for 4 color)


cls:
	rts
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

;remove bytes from source bitmap (z_iy)
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

;	Atari 800 Joystick Routine

	ifdef BuildA80	
Player_ReadControlsDual:
		lda PIA+$0	;22221111 - RLDU in player controls
		and #%00001111	;Bottom Nibble is Player 1 Joystick
		sta z_h
	rts
	
	else
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	Atari 5200 - Atari 5200 doesn't have PIA 

Player_ReadControlsDual:
	lda pokey+0			;$E800 - POT0 - game paddle 0
	jsr Player_ReadControlsProcessAnalog
	
	lda pokey+1			;$E801 - POT1 - game paddle 1
	jsr Player_ReadControlsProcessAnalog

	lda z_h
	and #%00001111	;Bottom Nibble is Player 1 Joystick
	sta z_h
	rts
	
	;Convert Analog to Digital
Player_ReadControlsProcessAnalog:
	cmp #255-64
	bcs Player_ReadControlsProcessHigh
	cmp #64
	bcc Player_ReadControlsProcessLow
	sec
	bcs Player_ReadControlsProcessB

Player_ReadControlsProcessHigh:		;B/R
	clc
Player_ReadControlsProcessB:
	rol z_h
	sec
	rol z_h
	rts
Player_ReadControlsProcessLow:		;T/L
	sec
	rol z_h
	clc
	rol z_h
	rts
	endif
		


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	Display List
  
    org $b000
	ifdef FourColor	
Smode Equ $0E	;E=4 color.... F=2 color
	else
Smode Equ $0F	;E=4 color.... F=2 color
	endif

DisplayList:				;Display list data
	db $70,$70,$70;$70 7= 8 blank lines 0= blank lines

		db $40+Smode,$60,$20	;Strange start ($2060) to safely step over the boundary
		
		db	         Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		db	   Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		db	   Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		db	   Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		db	   Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		
		db	   Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		db	   Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		db	   Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		db	   Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		db	   Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		
		db $40+Smode,$00,$30	;Have to manually step over the 4k boundary ($3000)
		db	         Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		db	   Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		db	   Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		db	   Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		db	   Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		
		db	   Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		db	   Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		db	   Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		db	   Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		db	   Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		
	db $41					;Loop
	dw DisplayList
		

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	Rom Header
        org $bffd
        db $FF         ;Disable Atari Logo
        dw ProgramStart;program Start
		
		
		
	