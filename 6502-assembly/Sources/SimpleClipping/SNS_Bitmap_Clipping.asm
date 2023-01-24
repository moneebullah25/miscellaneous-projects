
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


SnesScreenBuffer equ $0200 	;Tilemap buffer at $0200+ ($800 bytes)
SnesSpriteBuffer equ $0A00 	;Sprite buffer at $0A00+ ($220 bytes)

;Current player pos
PlayerX 	equ $60		;Position of next printed character
PlayerY 	equ PlayerX+1

spritehclip equ PlayerX+4


	org $8000		;Start of ROM
	SEI				;Stop interrupts
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		
;ScreenInit

		 ;aaaabbbb -aaa=base addr for BG2 bbb=base addr for BG1
	lda #%00010001
	sta $210B 		;BG1 & BG2 VRAM location register [BG12NBA]                    
	
	;     xxxxxxss 	- xxx=address… ss=SC size  00=32x32 01=64x32 10=32x64 11=64x64
	stz $2107		;BG1SC - BG1 Tilemap VRAM location
	
	; abcdefff - abcd=tile sizes e=pri fff=mode def
	lda #%00001001
	sta $2105		;BGMODE - Screen mode register
	
	;	  x000bbbb - x=screen disable (1=disable) bbbb=brightness (15=max)
	lda #%10000000	;Screen off
	sta $2100		;INIDISP - Screen display register

	
;PaletteDefs	

;Background (Color 0)
	stz $2121		;CGADD - Colour selection  (0=Back)
		 ;gggrrrrr 
	stz $2122		;CGDATA - Colour data register
		 ;?bbbbbgg 
	stz $2122		;CGDATA

;Color 3
	lda #1		;Color 3
	sta $2121		;CGADD - Colour selection  (15=Font)
		 ;gggrrrrr 
	lda #%00001111	
	sta $2122		;CGDATA - Colour data register
		 ;?bbbbbgg 
	lda #%00111100
	sta $2122		;CGDATA

;Color 3
	lda #2		;Color 3
	sta $2121		;CGADD - Colour selection  (15=Font)
		 ;gggrrrrr 
	lda #%11100000	
	sta $2122		;CGDATA - Colour data register
		 ;?bbbbbgg 
	lda #%01111111
	sta $2122		;CGDATA
	
;Color 3
	lda #3		;Color 3
	sta $2121		;CGADD - Colour selection  (15=Font)
		 ;gggrrrrr 
	lda #%11111111	
	sta $2122		;CGDATA - Colour data register
		 ;?bbbbbgg 
	lda #%01111111
	sta $2122		;CGDATA

;TileDefs	
	;	  i000abcd - I 0=inc on $2118 or $2139 0=$2119 or $213A… abcd=move size
	stz $2115 		;VMAIN - Video port control (Inc on write to $2118)
		
;Clear Tilemap buffer
	lda #<SnesScreenBuffer
	sta z_l
	lda #>SnesScreenBuffer
	sta z_h
	
	ldx #8
	sta z_b
	
	jsr clear				;Zero Range
	
;Clear Sprite Buffer
	lda #>SnesSpriteBuffer
	sta z_h
	lda #<SnesSpriteBuffer
	sta z_l
	
	ldx #$03
	jsr clear				;Zero Range

;Set Scroll position
	stz $210D  		;BG1HOFS BG1 horizontal scroll   
	stz $210D  		;BG1HOFS
	
	lda #-1
	sta $210E  		;BG1VOFS BG1 vertical scroll 
	stz $210E  		;BG1VOFS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
	lda #<Bitmap		;Source Bitmap Data
	sta z_L
	lda #>Bitmap
	sta z_H

	lda #<(BitmapEnd-Bitmap);Source Bitmap Data Length
	sta z_C
	lda #>(BitmapEnd-Bitmap)
	sta z_B

	lda #<$1000			;Snes patterns start at $1000
	sta z_E				;each adddress holds 1 word...  
	lda #>$1000			;so each 32 byte tile takes 16 addreses,
	sta z_D				;and tile 128 is at $1800
	jsr DefineTiles		;Define the tile patterns
	
;Turn on the screen	+ Sprites
		; ---S4321 - S=sprites 4-1=enable Bgx
	lda #%00000001	;Turn on BG1 + SPRITES 
	sta $212C 		;Main screen designation [TM]    

	;	  x000bbbb - x=screen disable (1=disable) bbbb=brightness (15=max)
	lda #%00001111	;Screen on
	sta $2100		;INIDISP - Screen display register
	
	lda #%10000000					;Turn on interrupts
	sta $4200

	
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
	;cmp #%11111111
	beq infloop		;See if no keys are pressed

StartDraw:	
	pha
		ldx PlayerX		;Back up X
		ldy PlayerY		;Back up Y
	
		lda #%00000000		;Turn off interrupts
		sta $4200

		jsr BlankPlayer	;Remove old player sprite
	
		ldx PlayerX		;Back up X
		ldy PlayerY		;Back up Y
	
	pla
	sta z_h
	
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

	stx PlayerX
	sty PlayerY
	
PlayerPosYOk:
	jsr DrawPlayer	;Draw Player Sprite
	
	
	lda #%10000000	;Turn on interrupts (allow tile update)
	sta $4200

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
	txa
	pha
		;Strobe joysticks to reset them
		ldx #$01				;Send a 1 to joysticks (strobe reset)
		stx $4016				;JOYPAD1 port
		
		dex 					;Send a 0 to joysticks (read data)
		stx $4016				;JOYPAD1 port

		ldx #8					;Read in 8 bits from each joystick
Player_ReadControlsDualloop:
		lda $4016				;JOYPAD1
		lsr 	   				; bit0 -> Carry
		ror z_h  				;Add carry to Joy1 data
		dex 
		bne Player_ReadControlsDualloop
	pla 
	tax
	rts
  
Bitmap:
	ds 32
	incbin "\ResALL\Sprites\RawSNS.RAW"
BitmapEnd:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
clear:
	ldy #0
	tya
ClsAgain:
	sta (z_hl),y
	iny
	bne ClsAgain
	inc z_h
	dex 
	bne ClsAgain
	rts
	
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

GetVDPScreenPos:	; BC=XYpos	
	lda z_c
	sta z_h			;32 tiles per Y line - 2 bytes per tile
	lda #0
	clc
	ror z_h
	ror 
	ror z_h
	ror 
	
	adc z_b 		;2 Bytes per X tile
	adc z_b 	
	sta z_l
	bcc GetVDPScreenPos_NoH
	inc z_h
GetVDPScreenPos_NoH:

	lda z_h
	adc #>SnesScreenBuffer	;Add Tile Cache Base
	sta z_h						;Must be byte aligned $??00
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	
	;lda #3	;SX
	;sta z_b
	;lda #3	;SY
	;sta z_c
	
	;ldx #6	;WID
	;ldy #6	;HEI
	
	;lda #0	;TileStart
	
DrawPlayer:	
	stx z_b
	sty z_c
	
	lda #1			;TileStart
	sta z_iyl

	lda #24			;Width in tiles
	sta z_h
	lda #24			;Height in tiles
	sta z_l
	
	jsr DoCrop	;Crop the sprite BC=XY pos HL=WidthHeight,
	bcc DoDraw		; IY=source data
		rts
DoDraw:		
		
	ldy z_l				;Height
	ldx z_h				;Width
	
FillAreaWithTiles_Yagain:
	jsr GetVDPScreenPos		;Get Cache Mem pos in HL
	phx
	phy
		ldy #0
FillAreaWithTiles_Xagain:
		lda z_iyl		
		sta (z_hl),Y 		;$2118 VMDATAL - tttttttt
		iny
		lda #0
		sta (z_hl),Y 		;$2119 VMDATAH - VHLPPPtt
		iny

		inc z_iyl			;Next Pattern
		dex 
		bne FillAreaWithTiles_Xagain	;Repeat X 
		inc z_c
		
		lda spritehclip		;Skip unneeded patterns
		beq NoSpriteClip
		clc
		adc z_iyl
		sta z_iyl
		bcc NoSpriteClip
		inc z_iyh
NoSpriteClip:
	ply
	plx
	dey
	bne FillAreaWithTiles_Yagain		;Repeat Y
	rts
	
	

BlankPlayer:	
	stx z_b
	sty z_c
	
	lda #24					;Width in tiles
	sta z_h
	lda #24					;Height in tiles
	sta z_l
	
	;Size (W,H) z_B,z_C 
	jsr DoCrop	;Crop the sprite BC=XY pos HL=WidthHeigh, IY=source data
	bcc DoBlank
		rts
DoBlank:				
	ldy z_l
	ldx z_h
	
BlankAreaWithTiles_Yagain:
	jsr GetVDPScreenPos
	
	phx
	phy
		ldy #0
BlankAreaWithTiles_Xagain:
		lda #0
		sta (z_hl),Y 	;$2118 VMDATAL
		iny
		sta (z_hl),Y 	;$2119 VMDATAH Video port data   
		iny
		dex 
		bne BlankAreaWithTiles_Xagain
		inc z_c
	ply
	plx
	dey
	bne BlankAreaWithTiles_Yagain	
	rts	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
	
	;BC=Bytes
	;DE=Destination Ram
	;HL=Source Bytes	
	
DefineTiles:
	jsr prepareVram	;Get VRAM address
	
	ldx z_C					;B=High byte of count - X=Low byte
	ldy #0
DefineTilesAgain	
	jsr WaitVblank
	lda (z_HL),Y	
	sta $2119		;VMDATAH - Write first byte to VRAM
	iny	
	
	jsr WaitVblank	
	lda (z_HL),Y	
	sta $2118		;VMDATAL - were set to Autoinc address 
	iny
	bne DefineTilesAgainYok
	
	inc z_h				;INC High byte Y=low byte
DefineTilesAgainYok:		
	txa				;Is Low Byte Zero
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
	
	
prepareVram:			
	jsr WaitVblank
	lda z_e
	sta $2116		;VMADDL - Destination address in VRAM L
	lda z_d
	sta $2117		;VMADDH - Destination address in VRAM H
	rts
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
WaitVblank:
		lda $4212 			;HVBJOY - Status 	
			; xy00000a		- x=vblank state y=hblank state a=joypad ready
		and #%10000000
		beq WaitVblank		;Wait until we get nonzero - this means we're in VBLANK
	rts	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
CustomNmihandler:		;Vblank interrupt handler
	php
	pha
;Copy Sprites (OAM) Via DMA
		stz $2102
		stz $2103           ; Point to start of OAM

		lda #0				;Write mode 000=one byte address repeating 
		sta $4300
		lda #04
		sta $4301			;Destination $21xx= $2104
		
		lda #<SnesSpriteBuffer
		sta $4302			;Source (24 bit - Little endian)
		lda #>SnesSpriteBuffer
		sta $4303
		lda #0				;bits 16-23
		sta $4304
			
		lda #<$220
		sta $4305	
		lda #>$220			;No of bytes (24 bit - Little endian
		sta $4306
		lda #0
		sta $4307
		
		lda #%00000001		
		sta $420B			;enable DMA 0 (bit0=1)	
	
;Copy Tilemap Via DMA
		lda #0
		sta $2116		;MemL -Video port address [VMADDL/VMADDH]                            
		sta $2117		;MemH
		
		lda #128
		sta $2115			;Inc address on write to $2119
			 
		lda #%00000001		;Write mode 001=two bytes alternate
		sta $4300
		
		lda #$18
		sta $4301			;Destination $21xx= 2118
		
		lda #<SnesScreenBuffer
		sta $4302			;Source (24 bit - Little endian)
		lda #>SnesScreenBuffer
		sta $4303
		lda #0				;bits 16-23
		sta $4304
		
		lda #<(32*32*2)
		sta $4305			;No of bytes (24 bit - Little endian
		lda #>(32*32*2)			;(only 1st 16 bits used?)
		sta $4306
		lda #0
		sta $4307
		
		lda #0
		sta $420C			;Disable H-DMA transfer 
		lda #%00000001		
		sta $420B			;enable DMA 0 (bit0=1)
		
;Dma's finished 
		lda #0
		sta $2115			;Inc address on write to $2118
	pla
	plp
	rti

	
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

;crop bottom hand side
	sta z_c				;draw ypos
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
	sty spritehclip
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

	clc ;clear carry = crop ok
	rts

		
neg:				;Negate a 
	eor #255
	clc
	adc #1
	rts
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
;	Footer
	
	org $FFC0
     ; "123456789012345678901"
	db "www.ChibiAkumas.com  "	; Program title (21 byte Ascii string)

	db $20		;Rom mode/speed (bits 7-4 = speed, bits 3-0 = map mode)
	db $00		;Rom type (bits 7-4 = co-processor, bits 3-0 = type)
	db $01 		;Rom size in banks (1bank=32k)
	db $00 		;Ram size (0=none)
	db $00		;Country/video refresh (ntsc 60hz, pal 50hz) (0=j 1=us/eu)
	db $00		;Developer id code
	db $00		;Rom version number
	db "cc"		;Complement check
	db "cs" 	;Checksum

;65816 mode vectors
	dw $0000 	;Reserved
	dw $0000 	;Reserved
	dw $0000 	;Cop vector   (cop opcode)
	dw $0000 	;Brk vector   (brk opcode)
	dw $0000 	;Abort vector (unused)
	dw CustomNmihandler	;Vblank interrupt handler
	dw $0000 	;Reset vector (unused)
	dw $0000 	;Irq vector   (h/v-timer/external interrupt)

;6502 mode vectors
	dw $0000 	;Reserved
	dw $0000	;Reserved
	dw $0000 	;Cop vector   (cop opcode)
	dw $0000 	;Brk vector   (unused)
	dw $0000 	;Abort vector (unused)
	dw CustomNmihandler	;Vblank interrupt handler
	dw $8000 	;Reset vector (cpu is always in 6502 mode on reset)
	dw $0000 	;Irq/brk vector
	
	
	