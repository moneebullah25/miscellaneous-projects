
ScreenBase equ $0080+SnesScreenBuffer


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



FlipLut equ 0


SnesScreenBuffer equ $200 	;Tilemap buffer at $0200+ 
							;(32x32x2 = $800 bytes total)


TileCache equ $1300


offset equ TileCache-2
offset2 equ TileCache-1
TileClear equ TileCache-3
spritehclip equ TileCache-4
striproutine equ TileCache-5
	 
ChibikoDef equ $12C0
ChibicloneDef equ $12D0


TestSprite equ $0000
TestChibiko equ $0011

;Current player pos
;PlayerX 	equ $60		
;PlayerY 	equ PlayerX+1



z_Regs 		equ $20


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
	ldx #16
	ldy #0
	loadpair z_hl,Palette
	stz $2121		;CGADD - Colour selection  
PaletteAgain:
		 ;gggrrrrr 
	lda (z_hl),y
	sta $2122		;CGDATA - Colour data register
		 ;?bbbbbgg 
	iny
	lda (z_hl),y
	sta $2122		;CGDATA
	iny
	dex
	bne PaletteAgain
			
	

;Clear Buffer
	loadpair z_hl,SnesScreenBuffer
	loadpair z_bc,$800		;;32x32x2= $800
	jsr cldir0				;Zero Range	
	
;Set Scroll position
	stz $210D  		;BG1HOFS BG1 horizontal scroll   
	stz $210D  		;BG1HOFS
	
	lda #-1
	sta $210E  		;BG1VOFS BG1 vertical scroll 
	stz $210E  		;BG1VOFS

;Clear Screen 		
	stz $2116		;MemL -Video port address [VMADDL/VMADDH]                            
	stz $2117		;MemH

	
	
	lda #<Bitmap		;Source Bitmap Data
	sta z_L
	lda #>Bitmap
	sta z_H

	lda #<(BitmapEnd-Bitmap) ;Source Bitmap Data Length
	sta z_C
	lda #>(BitmapEnd-Bitmap)
	sta z_B

	lda #<$1000			;Snes patterns start at $1000
	sta z_E				;each adddress holds 1 word...  
	lda #>$1000			;so each 32 byte tile takes 16 addreses,
	sta z_D				;and tile 128 is at $1800
	jsr DefineTiles		;Define the tile patterns
	
	
	
;Turn on the screen	
		; ---S4321 - S=sprites 4-1=enable Bgx
	lda #%00000001	;Turn on BG1
	sta $212C 		;Main screen designation [TM]    
	
	;	  x000bbbb - x=screen disable (1=disable) bbbb=brightness (15=max)
	lda #%00001111	;Screen on
	sta $2100		;INIDISP - Screen display register
	
	
	lda #%10000000					;Turn on interrupts
	sta $4200

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
	
	 	
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
	
	
	
	
	
	
	loadpair z_hl,TestSprite
	loadpair z_de,TileCache
	jsr cls
	
	;jmp *

	loadpair z_ix,ChibikoDef
	jsr DrawSpriteAlways	;Draw Player Sprite

	stz offset
	
	
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
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		lda #0
		sta tileclear
		loadpair z_ix,chibiclonedef
		jsr drawsprite
		loadpair z_ix,chibikodef
		jsr drawspritealways
	pullpair z_bc
	jmp infloop
	
	
	
Bitmap:
	ds 32
	incbin "\ResAll\Yquest\SNS_YQuest.RAW"

	incbin "\ResALL\MinTile\Chibiko2TilesSNS.RAW"
BitmapEnd
	
	
			
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


GetScreenPos:
	lda z_b
	and #%11111100			;4 Logical Units per Tile
	lsr 					;2 Bytes per tile
	sta z_b
	
	lda z_c
	lsr
	lsr
	clc
	adc #2					;Skip top 2 lines
	sta z_h					;32 tiles per Y line
							; 2 bytes per tile
	lda #0		
	lsr z_h			;128
	ror 
	lsr z_h			;64
	ror 
	
	adc z_b 				;Add X line
	sta z_l
	
	lda #>SnesScreenBuffer	;Add screen buffer base
	adc z_h
	sta z_h
	rts
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DoStrip:
	ldx #0
NextTile:
	lda (z_bcs,x)			;BCs=Tilemap data
	beq CustomTileDone		;EmptyTile?
	sta z_hs
	
	lda TileClear
	beq NoClear				;Are we clearing tiles?
	txa
	sta (z_bcs,x)			;Yes! Zero that tile!
NoClear:
	tay				;=0

DrawTileMore:	
	lda z_hs
	clc
	adc z_es				;DEs=Base Pattern number
	
	sta (z_hl),y	 ;%TTTTTTTT
	iny

	lda z_ds
	adc #0			;%VHLPPPTT T=tile number V=vflip H=hflip 
	sta (z_hl),y 	;L=layer (in front of sprites) P=palette 
	
CustomTileDone:
	inc z_l
	inc z_l			;2 Bytes per tile - Move to next Vram tile 
	bne CustomTileDoneB
	inc z_h
	
CustomTileDoneB:
	INC z_Cs			;Move to next Tilemap Tile
	BNE	CustomTileDoneC
	INC	z_Bs
CustomTileDoneC:

	dec z_iyl			;Tiles on this line
	bne NextTile
	rts
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
DoStripRev:
	ldx #0
NextTileRev:
	lda (z_bcs,x)			;BCs=Tilemap data
	beq CustomTileDoneRev	;EmptyTile?
	sta z_hs
	
	lda TileClear
	beq NoClearRev			;Are we clearing tiles?
	txa
	sta (z_bcs,x)			;Yes! Zero that tile!
NoClearRev:
	tay				;=0

DrawTileMoreRev:	
	lda z_hs
	clc
	adc z_es				;DEs=Base Pattern number
	
	sta (z_hl),y	 ;%TTTTTTTT
	iny

	lda z_ds		;XFLIP
	adc #%01000000	;%VHLPPPTT T=tile number V=vflip H=hflip 
	sta (z_hl),y 	;L=layer (in front of sprites) P=palette 
	
CustomTileDoneRev:
	inc z_l
	inc z_l
	bne CustomTileDoneBRev
	inc z_h
CustomTileDoneBRev:
		
	lda z_Bs			;Move to next Tilemap Tile
	BNE	CustomTileDoneCRev
	DEC z_Bs
CustomTileDoneCRev:		
	DEC	z_Cs
	
	dec z_iyl			;Tiles on this line
	bne NextTileRev
	rts
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
CustomNmihandler:		;Vblank interrupt handler
	pha
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
		
		lda #0
		sta $2115			;Inc address on write to $2118
	pla
	rti

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	


	;BC=Bytes
	;DE=Destination Ram
	;HL=Source Bytes	
	
DefineTiles:
	jsr prepareVram	;Get VRAM address
	
	ldx z_C			;B=High byte of count - X=Low byte
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
	
	inc z_h			;INC High byte Y=low byte
DefineTilesAgainYok:		
	txa				;Is Low Byte Zero
	bne DefineTilesDecBC_C
	lda z_B			;Are We done
	beq DefineTilesAgainDone
	DEC z_B			;DEC high byte (X is low byte)
DefineTilesDecBC_C:	
	DEx				;Subtract 2 
	DEX				;Since we did 2 bytes
	jmp DefineTilesAgain
DefineTilesAgainDone:
	rts
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	

prepareVram:			
	jsr WaitVblank
	lda z_e
	sta $2116		;VMADDL - Destination address in VRAM L
	lda z_d
	sta $2117		;VMADDH - Destination address in VRAM H
	rts
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
WaitVblank:
	lda $4212 		;HVBJOY - Status 	
		; xy00000a	- x=vblank state y=hblank state a=joypad ready
	and #%10000000
	beq WaitVblank	;Wait until we get nonzero - this means we're in VBLANK
	rts	
	


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		
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
	dw %0000000000000000; ;0  %-BBBBBGGGGGRRRRR
	dw %0000011111100010; ;1  %-BBBBBGGGGGRRRRR
	dw %0010110101101011; ;2  %-BBBBBGGGGGRRRRR
	dw %0101001010010100; ;3  %-BBBBBGGGGGRRRRR
	dw %0111111111111111; ;4  %-BBBBBGGGGGRRRRR
	dw %0011001000000101; ;5  %-BBBBBGGGGGRRRRR
	dw %0001101101000110; ;6  %-BBBBBGGGGGRRRRR
	dw %0001100011011100; ;7  %-BBBBBGGGGGRRRRR
	dw %0011000111011100; ;8  %-BBBBBGGGGGRRRRR
	dw %0010101010111100; ;9  %-BBBBBGGGGGRRRRR
	dw %0010011111111111; ;10  %-BBBBBGGGGGRRRRR
	dw %0101000010110100; ;11  %-BBBBBGGGGGRRRRR
	dw %0111110000011111; ;12  %-BBBBBGGGGGRRRRR
	dw %0110100011000000; ;13  %-BBBBBGGGGGRRRRR
	dw %0101100110000111; ;14  %-BBBBBGGGGGRRRRR
	dw %0111111101000000; ;15  %-BBBBBGGGGGRRRRR



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
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
	
	
	
	