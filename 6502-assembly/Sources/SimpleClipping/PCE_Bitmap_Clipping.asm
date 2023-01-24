
VscreenMinX equ 64		;Top left of visible screen in logical co-ordinates
VscreenMinY equ 72

;VscreenWid equ 24		;Visible Screen Size in logical units
;VscreenHei equ 24

;LIMITATION.. The Virtual screen cannot be smaller than the sprite or 
;the crop will malfunction! (It can be the same size)

VscreenWid equ 128		;Visible Screen Size in logical units
VscreenHei equ 112

VscreenWidClip equ 0
VscreenHeiClip equ 0

	 
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


;Current player pos
PlayerX 	equ $60		;Position of next printed character
PlayerY 	equ PlayerX+1

spritehclip equ PlayerX+4

	org $e000		;bank $0	
ProgramStart:

	sei				;Disable interrupts
	csh				;Highspeed Mode
	cld				;Clear Decimal mode
	
	lda #$f8		;map in RAM
	tam #%00000010	;TAM1 (2000-3FFF)

	lda #$ff		;map in I/O (#$ff)
	tam #%00000001	;TAM0 (0000-1FFF)
	tax				
	txs				;Init stack pointer
		
		;      T12 - TIQ, IRQ1, IRQ2
	lda #%00000111
	sta $1402		;IRQ mask... 1=Off
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		

;	ScreenInit
	st0 #5				;RegSelect 5
		 ;BSXXIIII	Backgroundon Spriteon eXtendedsync Interruptenable
	st1 #%10000000		;Background ON, Sprites On
	st2 #0
	
	st0 #9			
		; 0BBB0000
	st1 #%00000000		;BACKGROUND Tilemap size (32x32)
	st2 #0
	
;Reset Background scroll registers
	st0 #7				;Background X-scroll (------XX XXXXXXXX)
	st1 #0
	st2 #0
	
	st0 #8				;Background Y-scroll (-------Y YYYYYYYY)
	st1 #248			;Move Byte pos 0 to top left of screen 
	st2 #0				
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
;	Init Palette

;Background Color
	stz $0402			;Palette address L
	stz $0403			;Palette address H
	stz $0404			;GGRRRBBB	
	stz $0405			;-------G
;Color 1
	lda #1		
	sta $0402			;Palette address L
	stz $0403			;Palette address H
	lda #%00011011			
	sta $0404			;GGRRRBBB
	lda #%00000000
	sta $0405			;-------G
;Color 2
	lda #2		
	sta $0402			;Palette address L
	stz $0403			;Palette address H
	lda #%11000111			
	sta $0404			;GGRRRBBB
	lda #%00000001
	sta $0405			;-------G
;Color 3
	lda #3
	sta $0402			;Palette address L
	stz $0403			;Palette address H
	lda #%11111111			
	sta $0404			;GGRRRBBB
	lda #%00000001
	sta $0405			;-------G
	
	jsr cls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		

	
	lda #<Bitmap		;Source Bitmap Data
	sta z_L
	lda #>Bitmap
	sta z_H
	
	lda #<(BitmapEnd-Bitmap);Source Bitmap Data Length
	sta z_C
	lda #>(BitmapEnd-Bitmap)
	sta z_B
	
	lda #<$1800			;Tile 384 (256+128 - 32 bytes per tile)
	sta z_E
	lda #>$1800
	sta z_D
	jsr DefineTiles		;Define the tile patterns

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
	cmp #255
	beq infloop		;See if no keys are pressed

StartDraw:	
	jsr cls			;Remove Sprite
	
	ldx PlayerX		;Back up X
	ldy PlayerY		;Back up Y
	
	lda z_h
	and #%00000001	;RSBALDRU 
	bne JoyNotUp	;Jump if UP not presesd
	dey				;Move Y Up the screen
JoyNotUp:
	lda z_h
	and #%00000100	;RSBALDRU 
	bne JoyNotDown	;Jump if DOWN not presesd
	iny 			;Move Y Down the screen
JoyNotDown:
	lda z_h
	and #%00001000	;RSBALDRU 
	bne JoyNotLeft	;Move X Left 
	dex
JoyNotLeft:
	lda z_h
	and #%00000010	;RSBALDRU 
	bne JoyNotRight	;Move X Right
	inx
JoyNotRight:

	stx PlayerX		;Update X
	sty PlayerY		;Update Y
	
PlayerPosYOk:
	jsr DrawPlayer	;Draw Player Sprite
	
	ldx #255
	ldy #200
	jsr PauseXY		;Wait a bit!
	jmp infloop
	
	
	
DrawPlayer:
	lda #1
	sta z_iyh		;Tile 256+
	lda #128		;Tile Num
	sta z_iyl
	
;Remember! We don't use the first 256 tiles, 
;as some of this memory is used by the tilemap!
	
	stx z_b			;Xpos
	sty z_c			;Ypos
	
	lda #24			;Width in tiles
	sta z_h
	lda #24			;Height in tiles
	sta z_l
	
FillAreaWithTiles:	; z_b = SX... z_c = SY... X=Width...
						; Y= Height... A=start tile

	jsr DoCrop		;Crop the sprite BC=XY pos HL=WidthHeight,
	bcc DoDraw		 ;IY=source data
		rts
DoDraw:				
	
FillAreaWithTiles_Yagain:
		jsr GetVDPScreenPos	;Recalculate memory position
		ldx z_h
		st0 #2				;Set Write Register
FillAreaWithTiles_Xagain:	;Save the TileNum to Vram
		lda z_iyl
		sta $0102			;L Byte
		lda z_iyh
		sta $0103			;H Byte - Tile 256+
		inc z_iyl			;Increase Tile Number
		dex 
		bne FillAreaWithTiles_Xagain
		
		lda spritehclip		;Skip tiles before next line
		beq NoSpriteClip
		clc
		adc z_iyl
		sta z_iyl
		bcc NoSpriteClip
		inc z_iyh
NoSpriteClip:

		inc z_c				;Inc Ypos
	dec z_l					;Decrease Y count
	bne FillAreaWithTiles_Yagain
	rts
	


GetVDPScreenPos:	; BC=XYpos	
		st0 #0			;Select Vram Write
		lda z_c
		;and #%00000111	;Multiply Ypos by 32 - low byte
		asl
		asl
		asl
		asl
		asl
		clc
		adc z_b			;Add Xpos
		sta $0102		;Send to Data-L
		
		lda z_c
		and #%11111000	;Multiply Ypos by 32 - high byte
		lsr
		lsr
		lsr
		sta $0103		;Send to Data-H
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		
	

	
	
Bitmap:
	incbin "\ResALL\Sprites\RawPCE.RAW"
BitmapEnd:

PauseXY:
	dex
	bne PauseXY
	dey 
	bne PauseXY
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		
	

	

	;R:      3210
	;W:		   CS			C=Clear S=Select key/dir
	
	;Reset the Multitap... following reads will read in 
		;from joysticks 1-5
		
Player_ReadControlsDual:
	ldx #%00000001			;Reset Multitap 1
	jsr JoypadSendCommand
	ldx #%00000011			;Reset Multitap 2
	jsr JoypadSendCommand

	ldx #%00000001				
	jsr JoypadSendCommand	;----LDRU (Left/Down/Right/Up)
	jsr JoypadShiftFourBits
	dex
	jsr JoypadSendCommand	;---RSBA (Run/Start/B/A)
	jsr JoypadShiftFourBits
	rts

JoypadShiftFourBits:		;Shift RSBA in to z_as
	ldy #4
JoypadShiftFourBitsB:
	ror
	ror z_h
	dey
	bne JoypadShiftFourBitsB
	rts
	
JoypadSendCommand:
	stx $1000				;Set option from X
	PHA 					;Delay
	PLA 
	NOP 
	NOP
	lda $1000				;Load result
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		
	

prepareVram:		;z_HL=VRAM address to select

	st0 #0				;Select Memory Write Reg
	lda z_e
	sta $0102 			;st1 - L address
	lda z_d
	sta $0103 			;st2 - H Address
	rts

	;BC=Bytes
	;DE=Destination Ram
	;HL=Source Bytes
DefineTiles:							
	jsr prepareVram			;Select Ram address
	st0 #2					;Select Data reg
	ldx z_C					;B=High byte of count - X=Low byte
	ldy #0	
DefineTilesAgain:
		lda (z_HL),Y		;Load a byte
		sta $0102			;Store Low byte
		iny
		lda (z_HL),Y		;Load a byte
		sta $0103			;Store High Byte
		iny
		bne DefineTilesAgainYok
		inc z_h				;INC High byte Y=low byte
DefineTilesAgainYok:		
		txa					;Is Low Byte Zero?
		bne DefineTilesDecBC_C
		lda z_B				;Are We done
		beq DefineTilesAgainDone
		DEC z_B				;DEC high byte (X is low byte)
DefineTilesDecBC_C:	
		DEx					;Subtract 2 
		DEX					;Since we did 2 bytes
		jmp DefineTilesAgain
DefineTilesAgainDone:
	rts
	
	
cls:	
	st0 #0			;VDP reg 0 (address)
	st1 #$00		;L - Start of tilemap $0000
	st2 #$00		;H
	
	st0 #2			;Select VDP Reg2 (data)	
	
	ldx #4
	cly			;1024 tiles total (32x32= $400)
ClsAgain:	
	st1 #0			;Fill the entire area with our "Space tile"
	st2 #%00000001		;(tile 256)
	dey
	bne ClsAgain
	dex 
	bne ClsAgain
	rts
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;x,y pos = bc / width+height = hl

docrop_alloffscreen:
	sec		;set carry = nothing to draw
	rts
	
docrop:
	ldy #0				;Y=0 throughout this routine
	sty spritehclip
	sty z_d
	sty z_e				;e=top d=bottom crop

;crop top side
	lda z_c
	sec
	sbc #vscreenminy
	bcs notcrop			;nc=nothing needs cropping
	jsr neg
	inc
	and #%11111100
	cmp z_l			
	bcs docrop_alloffscreen	;all offscreen
	sta z_e				;top crop
	tya ;lda #0				;draw from top of screen
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
	lsr
	lsr
	beq novclip	;any lines to remove from the top?
	tax
	
	lda z_h		;calc bytes per strip
	lsr
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

	;ldy #0		;Y=0 throughout this routine
	sty z_d
	sty z_e		;e=top d=bottom crop

;crop left hand side
	lda z_b
	sec
	sbc #vscreenminx 		;remove left virtual border
	bcs nolcrop	;nc=nothing needs cropping
	jsr neg
	cmp z_h	;no pixels onscreen?
	bcs docrop_alloffscreen	;all offscreen
	clc
	adc #3
	sta z_e				;left crop
	tya	;y=0			;draw from top of screen
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

;Calculate new width
	lda z_d				;units to remove from left
	clc
	adc z_e				;units to remove from right
	lsr 
	lsr
	beq nohclip			;nothing to crop?

	sta spritehclip	;number of horizontal bytes to skip
						;after each line
	asl
	asl					;Convert to a sprite count
	jsr neg
	clc
	adc z_h
	bne cropb
		jmp docrop_alloffscreen	;nothing to draw?
cropb:
	sta z_h				;New width

	lda z_e				;amount to subtract from left
	lsr 
	lsr					;Sprites to skip 
	clc
	adc z_iyl
	sta z_iyl			;move across horizontal sprite.
	bcc nohclip
	inc z_iyh
nohclip:

	lsr z_b				;Quarter xpos (4 units per block)
	lsr z_b
	lsr z_h				;Quarter width (4 units per block) 
	lsr z_h

	lsr z_c				;Quarter Ypos
	lsr z_c

	lsr z_l				;Quarter Height
	lsr z_l

	clc					 ;clear carry = crop ok
	rts

		
neg:				;Negate a 
	eor #255
	clc
	adc #1
	rts
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	

	
	org $fffe
	dw ProgramStart			;Reset Vector 
	
