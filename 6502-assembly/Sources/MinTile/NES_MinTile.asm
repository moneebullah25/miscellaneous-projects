
ScreenBase equ $0000+VDPBuffer


	include "\SrcAll\BasicMacros.asm"
		
TileSmoothXmove equ 1	;move in blocks <8 pixels
;TileSmoothYmove equ 1	;This would just waste cpu power

VscreenMinX equ 64		;Top left of visible screen in logical co-ordinates
VscreenMinY equ 80

;VscreenWid equ 24		;Visible Screen Size in logical units
;VscreenHei equ 24

;LIMITATION.. The Virtual screen cannot be smaller than the sprite or 
;the crop will malfunction! (It can be the same size)

VscreenWid equ 128		;Visible Screen Size in logical units
VscreenHei equ 96

	
VscreenWidClip equ 2	;alter right boundary due to working in words
VscreenHeiClip equ 0


vblanked 	equ $7F			;Zero page address of Vblank count



VDP_L equ z_Regs+18			;Position of next Tile in buffer L
VDP_H equ z_Regs+19			;Position of next Tile in buffer H
VDP_Update equ z_Regs+20	;VDP Update Flag

VDPBuffer equ $200				;$400 byte Tilemap Buffer
SpriteBuffer equ VDPBuffer+$400	;$100 byte Sprite data (64 sprites)


VDPNextSprite equ z_Regs+21		;Last used H-Sprite

TileCache equ $6200				;Tilemap Cache


offset equ TileCache-2
offset2 equ TileCache-1
TileClear equ TileCache-3
spritehclip equ TileCache-4
striproutine equ TileCache-5
	 
ChibikoDef equ $60C0
ChibicloneDef equ $60D0


TestSprite equ $0000
TestChibiko equ $0011

;Current player pos
;PlayerX 	equ $60		
;PlayerY 	equ PlayerX+1



z_Regs 		equ $40


	org $BFF0

	db "NES",$1a		;ID
	db $01				;Rom pages (16k each)
	db $0				;CHR-ROM pages
	db %01000010		;mmmmFTBM		mmmm = mapper no bottom 4 bits , Four screen vram layout, Trainer at &7000, Battery ram at &6000, Mirror (0=horiz, 1=vert)
	db %00000000		;mmmm--PV 		mapper (top 4 bits...  Pc10 arcade, Vs unisystem )
	db 0				;Ram pages
	db 0,0,0,0,0,0,0
						;We selected Mapper 4 - it has 8k VRAM , 8K Sram and 128k rom
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		
;ScreenInit


ProgramStart:
	sei					;Interrupts off
	cld					;Clear Decimal flag
	ldx #$ff			;Set up stack
	txs
	
;Palette
	lda #$3F		;Select Palette ram &3F00
	sta $2006		;PPUADDR H
	lda #0
	sta $2006		;PPUADDR L
	
	ldx #4*8
PaletteAgain
	lda Palette-1,x 
	sta $2007		;PPUDATA
	dex 
	bne PaletteAgain
	
	
;Define patterns
	loadpair z_hl,Bitmap		;Source Bitmap Data
	loadpair z_bc,(BitmapEnd-Bitmap);Bitmap Data Length
	loadpair z_de,0				;Tile 0 (16 bytes per tile)
	jsr DefineTiles				;Define the tile patterns

	
;Turn ON the screen
;(Sprite enable/back enable/Sprite leftstrip / backleftstrip)
	;lda #%00011110 	
	;sta $2001		;PPUMASK
	
	;lda #$80		;NMI enable (Vblank)
	;sta $2000		;PPUCTRL - VPHB SINN

	lda #%10000000	;Turn on extra ram at $6000-%7FFF
	sta $A001
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				
;Clear Game Data
	loadpair z_hl,$6000	;Start
	loadpair z_bc,$1000		;Bytes
	jsr cldir0				;Zero Range

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
	lda #1
	sta VDPBuffer+4+32
	
	lda #$40 
	sta VDP_L
	lda #$02
	sta VDP_H
	 	
	lda #24
	sta z_ixl
	loadpair z_hl,TileMap2
	loadpair z_de,TileCache
FillYAgain:
	loadpair z_bc,32
	jsr ldir
	loadpair z_bc,4
	jsr addhl_bc
	
	dec z_ixl
	bne FillYAgain

	
	loadpair z_hl,xChibicloneDef
	loadpair z_de,ChibicloneDef
	loadpair z_bc,16
	jsr ldir
	
	loadpair z_hl,xChibikoDef
	loadpair z_de,ChibikoDef
	loadpair z_bc,16
	jsr ldir
	
	
	jsr NesEnableScreen
	
	
	loadpair z_hl,TestSprite
	loadpair z_de,TileCache
	jsr cls
	

	loadpair z_ix,ChibikoDef
	jsr DrawSpriteAlways	;Draw Player Sprite

	
	
;                                      

;                                      	ld bc,&6060
	loadpair z_bc,$6060
infloop:
	pushpairsafe z_bc
		jsr readjoystick
	pullpairsafe z_bc
startdraw:
	pha
		bit lookupbits+4
		bne joynotfire
		inc offset

joynotfire:
		pushpair z_bc

			lda offset
			sta z_c
			lda offset+1
			sta z_b
			cmp z_c
			bne scrollchange
				jmp noscrollchange
scrollchange
			pushpair z_bc
				lda #$24
				sta z_iyh

				lda #$20
				sta z_iyl

				lda #32
				sta z_ixh
				
				lda #24
				sta z_ixl

				loadpair z_de,tilecache

				lda z_b
				pha
					loadpair z_hl,tilemap2
					lda z_c
					and #%00000011
					sta z_c

					lda #0
					sta z_b
					jsr addhl_bc
				pla
				and #%00000011
				sta z_c
				pushpair z_hl
					loadpair z_hl,tilemap2
					;lda #0
					;sta z_b
					jsr addhl_bc
				pullpair z_bc
				jsr changescroll
			pullpair z_bc
		
			lda z_c
			sta offset
			sta offset+1
		
			loadpair z_ix,chibikodef
			ldy #spr_flags
			
			lda (z_ix),y
			clc
			adc #1
			sta (z_ix),y
			
			;loadpair z_hl,$4180		;Screen Offset $4180
			;loadpair z_bc,(80*200)	;Screen Offset $4180
			;jsr cldir0				;Clear screen bytes
	
			jsr flagspriteforrefresh
noscrollchange:
	
		loadpair z_ix,chibikodef
		pullpair z_bc
	pla
	sta z_d

	and #%00000001
	bne joynotup
	dec z_c
	jsr flagspriteforrefresh
joynotup:
	lda z_d
	and #%00000010
	bne joynotdown
	inc z_c
	jsr flagspriteforrefresh
joynotdown:
	lda z_d
	and #%00000100
	bne joynotleft
	dec z_b
	jsr flagspriteforrefresh
joynotleft:
	lda z_d
	and #%00001000
	bne joynotright
	inc z_b
	jsr flagspriteforrefresh
joynotright:

joydone:

	pushpair z_bc
		loadpair z_ix,chibikodef
		ldy #spr_xpos
		lda z_b
		sta (z_ix),y
		
		ldy #spr_ypos
		lda z_c
		sta (z_ix),y
		pushpair z_ix
			jsr removesprite
		pullpair z_ix
		jsr zerospriteincache
		
		loadpair z_ix,chibiclonedef
		jsr flagspriteforrefresh
		pushpair z_ix
			jsr removesprite
		pullpair z_ix
		ldy #spr_xpos
		
		lda (z_ix),y
		clc
		adc #1
		sta (z_ix),y
		jsr zerospriteincache
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


		
		
		lda #$02
		sta tileclear
		loadpair z_hl,testsprite
		loadpair z_de,tilecache
		jsr cls
	
	
		lda #0
		sta VDP_Update

		
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		lda #0
		sta VDPNextSprite			;Reset Hardware sprite count
		
		sta tileclear
		
		loadpair z_ix,chibiclonedef
		jsr drawspritealways		;Draw the sprites
		
		loadpair z_ix,chibikodef
		jsr drawspritealways		;Draw the sprites
		
		lda #0
		ldy VDPNextSprite			;Hardware sprite count
ClearMoreSprites:
		sta SpriteBuffer,y			;Clear unused hardware sprite
		iny
		bne ClearMoreSprites		;Repeat for 256 bytes
		
		
	pullpair z_bc
	jmp infloop
	
	
	
Bitmap:
	ds 16
	incbin "\ResAll\Yquest\NES_YQuest.RAW"

	incbin "\ResALL\MinTile\Chibiko2TilesNES.RAW"
BitmapEnd
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
GetScreenPos:
	lda z_b				;Xpos in logical units (pairs of pixels)
	lsr 
	lsr 
	sta z_l				;Xpos in tiles
	
	lda z_c				;Ypos in logical units (pairs of pixels)
	lsr
	lsr
	sta z_h				;Ypos in tiles
	
	lda #0		
	lsr z_h
	ror 
	lsr z_h
	ror 
	lsr z_h				;32 tiles per Y line
	ror 
	
	adc z_l 			;Add X line
	sta z_l
	
	lda #>VDPBuffer		;Vram Cache base
	adc z_h
	sta z_h
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DoStrip:
	ldx #0
NextTile:
	lda (z_bcs,x)	;BC=Tilemap data
	beq EmptyTile	;EmptyTile
	tay
	
	lda TileClear
	beq NoClear
	txa
	sta (z_bcs,x)
NoClear:
	
DrawTileMore:	
	tya					;Tilenumber
	clc
	adc z_es			;Pattern base
	sta (z_hl,x)	  	;VMDATAL - were set to Autoinc address
	
EmptyTile:
	inc z_l				;INC VRAM dest
	bne IncVramHok
	inc z_h		
IncVramHok:

	INC z_Cs			;INC tilemap source
	BNE	IncTilemapBok
	INC	z_Bs
IncTilemapBok:

	dec z_iyl
	bne NextTile		;Tilecount
	rts
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
DoStripRev:
	lda z_b				;Xpos for strip
	sta z_d
	
	ldx #0
	ldy VDPNextSprite
NextTileRev:
	
	lda (z_bcs,x)		;BC=Tilemap data
	beq EmptyTileRev	;EmptyTile
	sta z_hs
	
	lda TileClear
	beq NoClearRev
	txa
	sta (z_bcs,x)
NoClearRev:
	
	lda z_c				;Ypos
	asl					;Convert to pixels
	clc
	adc #8				;Top 8 lines offscreen
	sta SpriteBuffer,y	;%YYYYYYYY
	iny

	lda z_hs			;Tile Number
	clc
	adc z_es			;Tile offset
	sta SpriteBuffer,y	;%TTTTTTTT
	iny
	
	lda #%01000000		;Xflip
	sta SpriteBuffer,y	;%VHB---PP  
	iny						;Vflip  Hflip  BG priority  Palette
	
	lda z_d				;Xpos
	asl					;Convert to pixels
	sta SpriteBuffer,y	;XXXXXXXX
	iny
		
;Blank Tile in tilemap sprite pos
	txa	;=0
	sta (z_hl,x)	  ;VMDATAL - were set to Autoinc address
	
EmptyTileRev:	
	lda z_d
	clc
	adc #4				;Move across one tile (sprite)
	sta z_d
	
	inc z_l				;Move across one tile (Tilemap)
	bne CustomTileDoneBRev
	inc z_h
CustomTileDoneBRev:

	lda z_Cs			;Move to next source tile
	bne CustomTileDoneCRev
	DEC	z_Bs
CustomTileDoneCRev:
	DEC z_Cs

	dec z_iyl			;Repeat until strip is done
	bne NextTileRev

	lda z_c
	clc
	adc #4				;Move down one tile (sprite)
	sta z_c
	
	sty VDPNextSprite
	rts
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
PrintChar:
	rts	


TestSpriteList:
Sprite_1:
  db 0,1,2,3,4
  db 5,6,7,8,9
  db 10,11,12,13,14
  db 15,16,17,18,19
  db 20,21,22,23,24
  db 25,26,27,28,29
  db 0,30,31,32,0
  db 0,33,34,35,0
  
Tilemap2
	db 2,2,2,2,3,3,3,3,2,2,2,2,3,3,3,3,2,2,2,2,3,3,3,3,2,2,2,2,3,3,3,3 ,1,2,1,1
	db 2,2,2,2,3,3,3,3,2,2,2,2,3,3,3,3,2,2,2,2,3,3,3,3,2,2,2,2,3,3,3,3 ,2,1,1,1
	db 2,2,2,2,3,3,3,3,2,2,2,2,3,3,3,3,2,2,2,2,3,3,3,3,2,2,2,2,3,3,3,3 ,1,2,1,1
	db 2,2,2,2,3,3,3,3,2,2,2,2,3,3,3,3,2,2,2,2,3,3,3,3,2,2,2,2,3,3,3,3 ,2,1,1,1
	db 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1 ,1,2,1,1
	db 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1 ,2,1,1,1
	db 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1 ,1,2,1,1
	db 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1 ,2,1,1,1
	db 1,1,1,1,4,4,4,4,1,1,1,1,4,4,4,4,1,1,1,1,4,4,4,4,1,1,1,1,4,4,4,4 ,1,2,1,1
	db 4,4,4,4,1,1,1,1,4,4,4,4,1,1,1,1,4,4,4,4,1,1,1,1,4,4,4,4,1,1,1,1 ,2,1,1,1
	db 1,1,1,1,4,4,4,4,1,1,1,1,4,4,4,4,1,1,1,1,4,4,4,4,1,1,1,1,4,4,4,4 ,1,2,1,1
	db 4,4,4,4,1,1,1,1,4,4,4,4,1,1,1,1,4,4,4,4,1,1,1,1,4,4,4,4,1,1,1,1 ,2,1,1,1
	db 2,2,2,2,3,3,3,3,2,2,2,2,3,3,3,3,2,2,2,2,3,3,3,3,2,2,2,2,3,3,3,3 ,1,2,1,1
	db 2,2,2,2,3,3,3,3,2,2,2,2,3,3,3,3,2,2,2,2,3,3,3,3,2,2,2,2,3,3,3,3 ,2,1,1,1
	db 2,2,2,2,3,3,3,3,2,2,2,2,3,3,3,3,2,2,2,2,3,3,3,3,2,2,2,2,3,3,3,3 ,1,2,1,1
	db 2,2,2,2,3,3,3,3,2,2,2,2,3,3,3,3,2,2,2,2,3,3,3,3,2,2,2,2,3,3,3,3 ,2,1,1,1
	db 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1 ,1,2,1,1
	db 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1 ,2,1,1,1
	db 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1 ,1,2,1,1
	db 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1 ,2,1,1,1
	db 1,1,1,1,4,4,4,4,1,1,1,1,4,4,4,4,1,1,1,1,4,4,4,4,1,1,1,1,4,4,4,4 ,1,2,1,1
	db 4,4,4,4,1,1,1,1,4,4,4,4,1,1,1,1,4,4,4,4,1,1,1,1,4,4,4,4,1,1,1,1 ,2,1,1,1
	db 1,1,1,1,4,4,4,4,1,1,1,1,4,4,4,4,1,1,1,1,4,4,4,4,1,1,1,1,4,4,4,4 ,1,2,1,1
	db 4,4,4,4,1,1,1,1,4,4,4,4,1,1,1,1,4,4,4,4,1,1,1,1,4,4,4,4,1,1,1,1 ,2,1,1,1



readjoystick:
Player_ReadControlsDual:
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
  
	lda $4017				;JOYPAD2
	lsr 	   				; bit0 -> Carry
	ror z_l  				;Add carry to Joy2 data
  
	dex 
	bne Player_ReadControlsDualloop
  
	lda z_h
	jsr Player_ReadControlsCorrectOrder
	sta z_h 
	
	rts
    
	;Convert: Right Left Down Up Start Select B A
	;To:	  Start Select B A Right Left Down Up 
Player_ReadControlsCorrectOrder:
	eor #255				;Flip bits so unpressed=1
	jsr SwapNibbles
	rts
	
    
; $4016/$4017 - 1=Pressed / 0=NotPressed

; Read  1 - B
; Read  2 - Y
; Read  3 - Select
; Read  4 - Start
; Read  5 - Up
; Read  6 - Down
; Read  7 - Left
; Read  8 - Right
; Read  9 - A
; Read 10 - X
; Read 11 - L
; Read 12 - R
; Read 13 - Zero
; Read 14 - Zero
; Read 15 - Zero
; Read 16 - Zero

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Nmihandler:
	pha			;PHP/PLP happens automatically
	tya
	pha
		ldy #0
CustomNmihandlerAgain:	
		lda VDP_Update
		beq CustomNmihandlerAgainB
			jmp  CustomNmihandlerDone
CustomNmihandlerAgainB:			
		lda VDP_H
		clc
		adc #($20-$02)			;NameTable at  $2000 in VRAM
								;    Cache at  %0200 in RAM
		sta $2006	;PPUADDR 	Destination address - H
		
		lda VDP_L
		sta $2006	;PPUADDR 	Destination address - L
SendAgain:		
		lda (VDP_L),y
		sta $2007 	;PPUDATA
		iny
		lda (VDP_L),y
		sta $2007 	;PPUDATA
		iny
		lda (VDP_L),y
		sta $2007 	;PPUDATA
		iny
		lda (VDP_L),y
		sta $2007 	;PPUDATA
		iny
		lda (VDP_L),y
		sta $2007 	;PPUDATA
		iny
		lda (VDP_L),y
		sta $2007 	;PPUDATA
		iny
		lda (VDP_L),y
		sta $2007 	;PPUDATA
		iny
		lda (VDP_L),y
		sta $2007 	;PPUDATA
		iny
		lda (VDP_L),y
		sta $2007 	;PPUDATA
		iny
		lda (VDP_L),y
		sta $2007 	;PPUDATA
		iny
		lda (VDP_L),y
		sta $2007 	;PPUDATA
		iny
		lda (VDP_L),y
		sta $2007 	;PPUDATA
		iny		
		lda (VDP_L),y
		sta $2007 	;PPUDATA
		iny
		lda (VDP_L),y
		sta $2007 	;PPUDATA
		iny
		lda (VDP_L),y
		sta $2007 	;PPUDATA
		iny
		lda (VDP_L),y
		sta $2007 	;PPUDATA
		iny		
		cpy #$B0
		bcc SendAgain 
		
		
		lda #0				;Scroll X
		sta $2005
		lda #0-8			;Scroll y
		sta $2005
		inc vblanked		;Alter Vblank Zero page entry
		
		clc					;Update start VRAM address for next time
		tya
		adc VDP_L
		sta VDP_L
		
		lda VDP_H
		adc #0
		cmp #$05
		bcc VDP_NoOverFlow
		lda #$40			;skip First 2 lines (offscreen)
		sta VDP_L		
		
		lda #$02			;VDPBuffer is at address $0200
		sta VDP_Update
VDP_NoOverFlow:				
		sta VDP_H

		
CustomNmihandlerDone:		
		
		lda #SpriteBuffer/256	;Data to copy to sprites
		sta $4014 				;Start Spirte DMA transfer to OAM
		
	pla
	tay
	pla
irqhandler:	;Do nothing
	rti						;Return from interrupt handler



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	


prepareVram:				;Select a destination address
	lda z_d					;MSB - DEST ADDR
	sta $2006				;PPUADDR
	lda z_e					;LSB - Dest ADDR
	sta $2006				;PPUADDR
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

waitframe:
	pha
		lda #$00
		sta vblanked		;Zero Vblanked
waitloop:
		lda vblanked		;Wait for the interrupt to change it
		beq waitloop
	pla
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
;BC=Bytes
;DE=Destination Ram
;HL=Source Bytes
	
DefineTiles:				;Send Data to tile definitions
	jsr NesDisableScreen
	jsr prepareVram			;Calculate destination address
	
	ldx z_C					;B=High byte of count - X=Low byte
	ldy #0	
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
	rts

NesDisableScreen:			;Turn OFF the screen
;(Sprite enable/back enable/Sprite leftstrip / backleftstrip)
	lda #%00000000	
	sta $2001				;PPUMASK
	;lda #$00				;NMI disable (Vblank)
	sta $2000				;PPUCTRL - VPHB SINN
	rts	
	
		
NesEnableScreen:			;Turn ON the screenY
;(Sprite enable/back enable/Sprite leftstrip / backleftstrip)
	lda #%00011110 	
	sta $2001				;PPUMASK
	lda #$80				;NMI enable (Vblank)
	sta $2000				;PPUCTRL - VPHB SINN
	rts
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	

	
	include "/srcALL/V1_MinimalTile.asm"
	include "\SrcAll\BasicFunctions.asm"
	
	
xChibicloneDef:
	dw TestSpriteList	;Tilemap
	dw TestChibiko		;Pattern Data
	db 20,32		;Width,Height
	db 64,128		;X,Y
	db 1,1			;RefreshTile,Sprite
	db 64,128		;X,Y
	db 0,0			;Flags
xChibikoDef:
	dw TestSpriteList	;Tilemap
	dw TestChibiko		;Pattern Data
	db 20,32		;Width,Height
	db 96,96		;X,Y
	db 1,1			;RefreshTile,Sprite
	db 64,128		;X,Y
	db 0,0			;Flags

	
	
Palette:
; 	Color   3   2   1  0
		db $30,$2B,$13,$0D	;Sprites 0
		db $30,$2B,$13,$0D	;Sprites 0
		db $30,$2B,$13,$0D	;Sprites 0
		db $30,$2B,$13,$0D	;Sprites 0
		
		db $30,$2B,$13,$0D	;Tiles 3
		db $30,$2B,$13,$0D	;Tiles 2
		db $30,$2B,$13,$0D	;Tiles 1
		db $30,$2B,$13,$0D	;Tiles 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
;	Footer
	org $FFFA
	dw nmihandler		;FFFA - Interrupt handler
	dw ProgramStart		;FFFC - Entry point
	dw irqhandler		;FFFE - IRQ Handler
	
	