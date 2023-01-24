;Compile With Option: "Vasm NES"

VscreenMinX equ 64		;Top left of visible screen in logical co-ordinates
VscreenMinY equ 72-4

;VscreenWid equ 24		;Visible Screen Size in logical units
;VscreenHei equ 24

;LIMITATION.. The Virtual screen cannot be smaller than the sprite or 
;the crop will malfunction! (It can be the same size)

VscreenWid equ 128		;Visible Screen Size in logical units
VscreenHei equ 112+8

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


	org $BFF0

	db "NES",$1a		;ID
	db $01				;Rom pages (16k each)
	db $0				;CHR-ROM pages
	db %01000010		;mmmmFTBM		mmmm = mapper no bottom 4 bits , Four screen vram layout, Trainer at &7000, Battery ram at &6000, Mirror (0=horiz, 1=vert)
	db %00000000		;mmmm--PV 		mapper (top 4 bits...  Pc10 arcade, Vs unisystem )
	db 0				;Ram pages
	db 0,0,0,0,0,0,0
						;We selected Mapper 4 - it has 8k VRAM , 8K Sram and 128k rom
	
vblanked 	equ $7F		;Zero page address of Vblank count
VDP_CT equ z_Regs+18	;Position of next Tile in buffer
VDPBuffer equ $200		;Tilemap Buffer


;Current player pos
PlayerX 	equ $60		;Position of next printed character
PlayerY 	equ PlayerX+1

spritehclip equ PlayerX+4

SpriteBuffer equ VDPBuffer+$100	;SpriteBuffer for DMA

HspriteNum equ PlayerX+5		;Count of H-sprites used


ProgramStart:
	sei					;Interrupts off
	cld					;Clear Decimal flag
	
	ldx #$ff			;Set up stack
	txs
	
;Palette
	lda #$3F		;Select Palette ram &3F00
	sta $2006		;PPUADDR H
	lda #4*4
	sta $2006		;PPUADDR L
	
	ldx #4
PaletteAgain
	lda Palette-1,x 
	sta $2007		;PPUDATA
	dex 
	bne PaletteAgain

	

	lda #%10000000	;Turn on extra ram at $6000-%7FFF
	sta $A001
	
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	

	
	lda #<Bitmap			;Source Bitmap Data
	sta z_L
	lda #>Bitmap
	sta z_H

	lda #<(BitmapEnd-Bitmap);Source Bitmap Data Length
	sta z_C
	lda #>(BitmapEnd-Bitmap)
	sta z_B
	
	lda #<$0800				;Tile 128 (16 bytes per tile)
	sta z_E
	lda #>$0800
	sta z_D
	
	jsr DefineTiles			;Define the tile patterns
	
	
	
	lda #%00011010 			;Left Black Strip
	;lda #%00011110 			;No Left Black Strip
	sta $2001				;PPUMASK
	
		
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
	beq infloop		;See if no keys are pressed

StartDraw:	

	
	ldx PlayerX		;Back up X
	ldy PlayerY		;Back up Y
	
	lda z_h
	and #%00010000	;RLDUSsBA 
	beq JoyNotUp	;Jump if UP not presesd
	dey				;Move Y Up the screen
JoyNotUp:
	lda z_h
	and #%00100000	;RLDUSsBA
	beq JoyNotDown	;Jump if DOWN not presesd
	iny 			;Move Y Down the screen
JoyNotDown:
	lda z_h
	and #%01000000	;RLDUSsBA 
	beq JoyNotLeft	;Move X Left 
	dex
JoyNotLeft:
	lda z_h
	and #%10000000	;RLDUSsBA
	beq JoyNotRight	;Move X Right
	inx
JoyNotRight:

	stx PlayerX		;Update X
	sty PlayerY		;Update Y
	
	lda #0
	sta HspriteNum
	
PlayerPosYOk:
	jsr DrawPlayer			;Draw Player Sprite
	
	jsr ClearUnsusedSprites	;Zero unneeded sprites
	
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

ClearUnsusedSprites:
	lda HspriteNum		;Convert sprite to byte
	asl
	asl
	tay					;*4
	
	lda #0
ClearUnsusedSpritesB
	sta SpriteBuffer,y		;Clear remaining bytes
	iny
	bne ClearUnsusedSpritesB ;At end of sprite 64?
	rts
	

Palette:  
; 	Color   3   2   1  0
		db $30,$21,$14,$0D

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
Bitmap:
	incbin "\ResALL\Sprites\RawNES.RAW"
	db 1,2,3,4,5,6,7,8
	db 1,2,3,4,5,6,7,8
	db 1,2,3,4,5,6,7,8
	db 1,2,3,4,5,6,7,8		;Junk data to check we're not over drawing
BitmapEnd:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
waitframe:
	pha
		lda #$00
		sta vblanked		;Zero Vblanked
waitloop:
		lda vblanked		;Wait for the interrupt to change it
		beq waitloop
	pla
	rts
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
GetVDPScreenPos:			; BC=XYpos
	lda z_h
	pha
	lda z_l
	pha		
		lda z_c
		ifdef ScrWid256		;256x192
			clc
			adc #2
		endif
		and #%00000111		;Ypos * 32 tiles per line
		clc
		ror
		ror
		ror
		ror
		ora z_b				;Add Xpos
		sta z_l				;Store in L byte
		lda z_c
		ifdef ScrWid256	;256x192
			clc
			adc #2
		endif
		and #%11111000		;Other bits of Ypos for H byte
		clc
		ror
		ror
		ror
		clc
		adc #$20			;$2000 ofset for base of tilemap
		sta z_h
			
		jsr GetVdpBufferCT	;Get the VDP buffer pos
		lda z_h
		sta VDPBuffer,Y		;Store the address to the buffer
		iny
		lda z_l
		sta VDPBuffer,Y
		iny					;We still need to write a data byte!
	pla
	sta z_l
	pla 
	sta z_h
	rts
	
GetVdpBufferCT:	
	ldy VDP_CT
	cpy #32*3			;See if buffer is full
	bcc VdpNotBusy		
	jsr waitframe		;Buffer is full, so wait for Vblank
	ldy VDP_CT
VdpNotBusy:	
	lda #0
	sta VDP_CT			;Halt the queue
	rts
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
	;BC=Bytes
	;DE=Destination Ram
	;HL=Source Bytes		
DefineTiles:				;Send Data to tile definitions
	ldx z_C					;B=High byte of count - X=Low byte
	ldy #0
	jsr NesDisableScreen
	jsr prepareVram			;Calculate destination address
		
DefineTilesAgain	
	lda (z_HL),Y		
	sta $2007 				;PPUDATA - Write data to data-port
	iny
	bne DefineTilesAgainYok
	
	inc z_h					;INC High byte Y=low byte
DefineTilesAgainYok:		
	txa						;Is Low Byte Zero
	bne DefineTilesDecBC_C
	lda z_B					;Is High Byte Zero (Are We done?)
	beq DefineTilesAgainDone
	DEC z_B					;DEC Count High byte (X is low byte)
DefineTilesDecBC_C:	
	DEx						;Decrease Count Low Byte
	jmp DefineTilesAgain
DefineTilesAgainDone:
	jmp NesEnableScreen
	
	
	


Player_ReadControlsDual:
	txa
	pha
		;Strobe joysticks to reset them
		ldx #$01		;Send a 1 to joysticks (strobe reset)
		stx $4016		;JOYPAD1 port
		
		dex 			;Send a 0 to joysticks (read data)
		stx $4016		;JOYPAD1 port

		ldx #8			;Read in 8 bits from each joystick
Player_ReadControlsDualloop:
		lda $4016		;JOYPAD1
		lsr 	   		; bit0 -> Carry
		ror z_h  		;Add carry to Joy1 data
		dex 
		bne Player_ReadControlsDualloop
	pla 
	tax
	rts
	
	
DrawPlayer:	
	stx z_b				;XYpos of sprite in logical units
	sty z_c
	
	lda #24				;Width in tiles
	sta z_h
	lda #24				;Height in tiles
	sta z_l
		
	lda #128			;TileStart
	sta z_iyl			;Backup tilenum	
FillAreaWithTiles:
	jsr DoCrop			;Crop the sprite BC=XY pos 
	bcc DoDraw				;HL=WidthHeigh, IY=source data
		rts
DoDraw:				
	lda HspriteNum		;Next Hsprite Num
	asl
	asl
	tay
		
FillAreaWithTiles_Yagain:
	ldx z_h				;Width
	lda z_b				;Xpos
	pha
FillAreaWithTiles_Xagain:
		lda z_c
		sta SpriteBuffer,y	;Y
		iny
		
		lda z_iyl
		sta SpriteBuffer,y	;Pattern
		iny
		inc z_iyl
		
		lda #0
		sta SpriteBuffer,y	;Settings
		iny
		
		lda z_b
		sta SpriteBuffer,y	;X
		iny
		
		lda z_b				;Xpos+8
		clc
		adc #8
		sta z_b
		dex 
		bne FillAreaWithTiles_Xagain
	pla
	sta z_b
	
	lda z_c				;Ypos +8
	clc
	adc #8
	sta z_c

	lda spritehclip
	beq NoSpriteClip	;Skip tiles as needed
	clc
	adc z_iyl
	sta z_iyl
	bcc NoSpriteClip
	inc z_iyh
NoSpriteClip:
	dec z_l
	bne FillAreaWithTiles_Yagain
	
	tya
	lsr
	lsr
	sta HspriteNum			;Next H-sprite num
	rts

	
prepareVram:				;Select a destination address

		lda z_d				;MSB - DEST ADDR
		sta $2006			;PPUADDR
		lda z_e				;LSB - Dest ADDR
		sta $2006			;PPUADDR
	rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	


NesDisableScreen:			;Turn OFF the screen
;(Sprite enable/back enable/Sprite leftstrip / backleftstrip)
	lda #%00000000	
	sta $2001				;PPUMASK
	;lda #$00				;NMI disable (Vblank)
	sta $2000				;PPUCTRL - VPHB SINN
	rts	

		
NesEnableScreen:			;Turn ON the screen
;(Sprite enable/back enable/Sprite leftstrip / backleftstrip)
	lda #%00011010 	
	sta $2001				;PPUMASK
	
	lda #$80				;NMI enable (Vblank)
	sta $2000				;PPUCTRL - VPHB SINN
	rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	


ResetScroll:	
	lda #0					;Scroll X
	sta $2005				;PPUSCROLL
	lda #0-8				;Scroll y
	sta $2005				;PPUSCROLL
	rts	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
Nmihandler:						;Vblank Interrupt handler
	php
	pha
	tya
	pha
		lda #SpriteBuffer/256	;Data to copy to sprites
		sta $4014 				;Start Spirte DMA transfer to OAM
		
		ldy #0
CustomNmihandlerAgain:	
		cpy VDP_CT		;See if there are any bytes left to write
		bcs CustomNmihandlerDone
		
		lda VDPBuffer,y
		iny	
		sta $2006	;PPUADDR 	Destination address - H
		
		lda VDPBuffer,y
		iny
		sta $2006	;PPUADDR 	Destination address - L
		
		lda VDPBuffer,y
		iny
		sta $2007 	;PPUDATA
		
		jmp CustomNmihandlerAgain;Process more bytes

CustomNmihandlerDone:		;Reset Scroll
		lda #0				;Scroll X
		sta VDP_CT
		sta $2005
		lda #0-8			;Scroll y
		sta $2005

		inc vblanked		;Alter Vblank Zero page entry
	pla
	tay
	pla
	plp
irqhandler:	;Do nothing
	rti						;Return from interrupt handler



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
	lda z_c				;Ypos
	sec
	sbc #vscreenminy	;>minimum co-odinate
	bcs notcrop			;nc=nothing needs cropping
	jsr neg
	cmp z_l			
	bcs docrop_alloffscreen	;all offscreen
	sta z_e				;top crop
	eor #%00000011
	and #%00000011		;We need to draw this sprite 
	clc						;partially offscreen
	adc #1	
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
	sec
	sbc #1	
	and #%11111100		;Convert to a tile count
	beq docrop_alloffscreen
	sta z_l				;new height


;remove lines from source bitmap (z_iy)	
	lda z_e				;lines to remove from top?
	beq novclip			;any lines to remove from the top?
	clc
	adc #4				;We draw one extra line to allow
	lsr 					; half offscreen sprites 
	lsr
	beq novclip			;any lines to remove from the top?
	tax
	
	lda z_h				;calc bytes per 2 lines 
	lsr						;(2 physical lines per logical y)
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
	;ldy #0		;Y=0 throughout this routine
	sty spritehclip
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
	adc #3				;round up for crop
	sta z_e				;left crop
	and #%00000011		;We need to draw this sprite 
	eor #%00000011			;offset for the removed sprites
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

	asl z_b				;Double Xpos (Pixels)
	asl z_c				;Double Ypos (Pixels)
	
	lsr z_h				;Quarter width (Tiles)
	lsr z_h

	lsr z_l				;Quarter Height (Tiles)
	lsr z_l

	clc					;clear carry = crop ok
	rts

		
neg:				;Negate a 	
	eor #255
	clc
	adc #1
	rts
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
;Cartridge Footer	
	org $FFFA
	dw nmihandler			;FFFA - Interrupt handler
	dw ProgramStart			;FFFC - Entry point
	dw irqhandler			;FFFE - IRQ Handler
	
	
	
	
	
	
	