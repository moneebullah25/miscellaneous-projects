;FourColor equ 1

;Current player pos
PlayerX 	equ $60	
PlayerY 	equ PlayerX+1

;Last player pos (For clearing sprite)
PlayerX2 	equ PlayerX+2
PlayerY2 	equ PlayerX+3


TileSmoothXmove equ 1	;move in blocks <8 pixels
TileSmoothYmove equ 1	;This would just waste cpu power

VscreenMinX equ 64		;Top left of visible screen in logical co-ordinates
VscreenMinY equ 80

;VscreenWid equ 24		;Visible Screen Size in logical units
;VscreenHei equ 24

;LIMITATION.. The Virtual screen cannot be smaller than the sprite or 
;the crop will malfunction! (It can be the same size)

VscreenWid equ 128		;Visible Screen Size in logical units
VscreenHei equ 96

	
VscreenWidClip equ 2	;alter right boundary due to working in words
VscreenHeiClip equ 3



TileCache equ $0400

FlipLUT equ $0200		;Lookup table fo x-flip

offset equ TileCache-2
offset2 equ TileCache-1
TileClear equ TileCache-3
spritehclip equ TileCache-4
striproutine equ TileCache-5
	 
ChibikoDef equ $0300
ChibicloneDef equ $0310

z_Regs 		equ $20

ScreenBase equ $2064


	include "\SrcAll\BasicMacros.asm"
	


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

;Build the X-flip LUT

	ldy #0					;Y offset & source byte
	loadpair z_hl,FlipLUT 
FillLutAgain:
	tya
	ifdef FourColor
		and #%11000000	;A
		clc
		rol
		rol
		rol
		sta z_c
		tya 
		and #%00110000	;B
		ror
		ror
		ora z_c
		sta z_c
		tya 
		and #%00001100	;C
		rol
		rol
		ora z_c
		sta z_c
		tya 
		and #%00000011	;D
		ror
		ror
		ror
		ora z_c
	else				;2 color mode
		ror
		rol z_c
		ror
		rol z_c
		ror
		rol z_c
		ror
		rol z_c
		ror
		rol z_c
		ror
		rol z_c
		ror
		rol z_c
		ror
		rol z_c
		lda z_c
	endif
	sta (z_hl),y		;Write the calculate byte
	iny
	bne FillLutAgain	;Repeat for 0-255
	
	
	
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
	
	loadpair z_ix,ChibikoDef
	jsr DrawSpriteAlways	;Draw Player Sprite

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
		
	
	;We Return ---FRLDU in z_h for Player 0, and z_L for Player 1
	ifdef BuildA80	
ReadJoystick:
		lda PIA+$0	;22221111 - RLDU in player controls
		and #%00001111	;Bottom Nibble is Player 1 Joystick
		ora #%11100000
		sta z_h

		lda GTIA+$10	;$D010 - TRIG0 - joystick trigger 0
		clc
		rol
		rol
		rol
		rol
		ora z_h			;Joystick 1 Done
		sta z_h			
		
	rts
	endif
	
	
	ifdef BuildA52		;Atari 5200 doesn't have PIA 
ReadJoystick:
	lda GTIA+$10		;$C010 - TRIG0 - joystick trigger 0
	sta z_as
	
	lda pokey+0			;$E800 - POT0 - game paddle 0
	jsr Player_ReadControlsProcessAnalog
	
	lda pokey+1			;$E801 - POT1 - game paddle 1
	jsr Player_ReadControlsProcessAnalog
	
	lda #%11100000
	ora z_as
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
;		rol z_h
;		sec
;		rol z_h
;		rts
Player_ReadControlsProcessHigh:		;B/R
	clc
Player_ReadControlsProcessB:
	rol z_as
	sec
	rol z_as
	rts
Player_ReadControlsProcessLow:		;T/L
	sec
	rol z_as
	clc
	rol z_as
	rts
	endif
PrintChar:
	rts		

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


	
	
	;40 bytes per line = * %00000000 00101000
	;We shift our Ypos 3 to the right, add it, then another 2, eg
	
	;%00000000 00101000	=40 (32+8)
	;%YYYYYYYY 00000000
	;%000YYYYY YYY00000	= Y*32
	;%00000YYY YYYYY000 = Y*8
	
;(tiles are 4x8 in 4 color mode)
;(tiles are 8x8 in 2 color mode)
	
GetScreenPos:	
	lsr z_b
	lsr z_b			;4 LU per H-tile 
	
	;asl z_c		;1 LU = 2lines
	lda #0
	;lsr z_c		;Shift 3 Bits
	;ror 			;%0YYYYYYY Y0000000
	lsr z_c
	ror 			;%00YYYYYY YY000000
	lsr z_c
	ror 			;%000YYYYY YYY00000 = Y*32
	tax	
		clc
		adc z_b		;Update Low Byte
		sta z_l
		
		lda #0 		;Update High Byte
		adc z_c
		sta z_h
	txa
	lsr z_c			;Shift 2 bits
	ror 			;%0000YYYY YYYY0000
	lsr z_c
	ror 			;%00000YYY YYYYY000 = Y*8 
		
	adc z_l			;Add to Update Low Byte
	sta z_l
		
	lda z_c			;Add to Update High Byte
	adc z_h
	sta z_h	

	clc
	lda z_l
	adc #$60+4
	sta z_l
	lda z_h			;Add Screen Base ($2060)
	adc #$20
	sta z_h		
	rts	
	
	


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


DoStrip:
	ldx #0
NextTile:
DrawTile:
	lda (z_bcs,x)		;BC=Tilemap data
	beq EmptyTile		;EmptyTile
	tay
	
	lda TileClear
	beq NoClear			;0=Dont zero tilemap
	txa
	sta (z_bcs,x)		;Store 0 in tilemap
	
NoClear:
	stx z_hs			;=0
	tya 				;Tilenum
	
	asl 
	rol z_hs			;*2
	asl 
	rol z_hs			;*4
	asl 
	rol z_hs			;*8

	adc z_es
	sta z_ls
	
	lda z_hs
	adc z_ds
	sta z_hs			;HLs=Bitmap Source
	
	ldy #0
	
	
	lda (z_HLs,x)		;Get a source byte
	sta (z_HL),y		;Store to VRAM
	ldy #40*1			;Down a line 	+40
	inc z_Ls			;Inc source data
	
	lda (z_HLs,x)		;Get a source byte
	sta (z_HL),y		;Store to VRAM
	ldy #40*2			;Down a line	+80
	inc z_Ls			;Inc source data
	
	lda (z_HLs,x)		;Get a source byte
	sta (z_HL),y		;Store to VRAM
	ldy #40*3			;Down a line	+120
	inc z_Ls			;Inc source data
	
	lda (z_HLs,x)		;Get a source byte
	sta (z_HL),y		;Store to VRAM
	ldy #40*4			;Down a line	+160
	inc z_Ls			;Inc source data
	
	lda (z_HLs,x)		;Get a source byte
	sta (z_HL),y		;Store to VRAM
	ldy #40*5			;Down a line	+200
	inc z_Ls			;Inc source data
	
	lda (z_HLs,x)		;Get a source byte
	sta (z_HL),y		;Store to VRAM
	ldy #40*6			;Down a line	+240
	inc z_Ls			;Inc source data
	
	lda (z_HLs,x)		;Get a source byte
	sta (z_HL),y		;Store to VRAM
	ldy #24
	inc z_H				;Down a line +280
	inc z_Ls			;Inc source data
	
	lda (z_HLs,x)		;Get a source byte
	sta (z_HL),y		;Store to VRAM
	
	dec z_H				;Reset Vram dest
EmptyTile:

	INC z_Cs			;Inc source tilemap
	BNE	EmptyTileC
	INC	z_Bs
EmptyTileC:

	inc z_l				;Across one tile in VRAM
	bne EmptyTileL
	inc z_h
EmptyTileL:

	dec z_iyl			;Strip width count
	beq TileDone2
		jmp	NextTile
TileDone2:
	rts


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




DoStripRev:
	lda #>FlipLUT	;Get top byte of LUT address
	sta z_b
	ldx #0
NextTileRev:
DrawTileRev:
	lda (z_bcs,x)	;BC=Tilemap data
	bne NotEmptyTileR
	jmp EmptyTileR	;EmptyTile
NotEmptyTileR:
	tay
	
	lda TileClear

	beq NoClearRev
	txa
	sta (z_bcs,x)
	
NoClearRev:
	stx z_hs			;=0
	tya 				;Tilenum
	
	asl 
	rol z_hs			;*2
	asl 
	rol z_hs			;*4
	asl 
	rol z_hs			;*8

	adc z_es
	sta z_ls
	
	lda z_hs
	adc z_ds
	sta z_hs			;HLs=Bitmap Source
	
	ldy #0
	
	
	lda (z_HLs,x)		;Get a source byte
	sta z_c				;Set LUT source
	lda (z_bc,x)		;Get flipped byte
	sta (z_HL),y		;Store to VRAM
	ldy #40*1			;Down a line 	+40
	inc z_Ls			;Inc source data
	
	lda (z_HLs,x)		;Get a source byte
	sta z_c				;Set LUT source
	lda (z_bc,x)		;Get flipped byte
	sta (z_HL),y		;Store to VRAM
	ldy #40*2			;Down a line 	+80
	inc z_Ls			;Inc source data
	
	lda (z_HLs,x)		;Get a source byte
	sta z_c				;Set LUT source
	lda (z_bc,x)		;Get flipped byte
	sta (z_HL),y		;Store to VRAM
	ldy #40*3			;Down a line 	+120
	inc z_Ls			;Inc source data
	
	lda (z_HLs,x)		;Get a source byte
	sta z_c				;Set LUT source
	lda (z_bc,x)		;Get flipped byte
	sta (z_HL),y		;Store to VRAM
	ldy #40*4			;Down a line 	+160
	inc z_Ls			;Inc source data
	
	lda (z_HLs,x)		;Get a source byte
	sta z_c				;Set LUT source
	lda (z_bc,x)		;Get flipped byte
	sta (z_HL),y		;Store to VRAM
	ldy #40*5			;Down a line 	+200
	inc z_Ls			;Inc source data
	
	lda (z_HLs,x)		;Get a source byte
	sta z_c				;Set LUT source
	lda (z_bc,x)		;Get flipped byte
	sta (z_HL),y		;Store to VRAM
	ldy #40*6			;Down a line 	+240
	inc z_Ls			;Inc source data
	
	
	lda (z_HLs,x)		;Get a source byte
	sta z_c				;Set LUT source
	lda (z_bc,x)		;Get flipped byte
	sta (z_HL),y		;Store to VRAM
		
	inc z_h				;Down a line 	+240
	ldy #24
	
	inc z_Ls			;Inc source data
	
	
	lda (z_HLs,x)		;Get a source byte
	sta z_c				;Set LUT source
	lda (z_bc,x)		;Get flipped byte
	sta (z_HL),y		;Store to VRAM
	
	dec z_h				;Reset Vram dest
	
EmptyTileR:
	lda z_Cs			;Inc source tilemap
	bne EmptyTileRC
	dec	z_Bs
EmptyTileRC:
	dec z_Cs
	
	inc z_l				;Across one tile in VRAM
	bne EmptyTileRL
	inc z_h
EmptyTileRL:
	dec z_iyl
	beq TileDone2Rev
		jmp	NextTileRev	;Strip width count
TileDone2Rev:
	rts
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


	
	align 4		;Align sprites so whole tile is in a single L byte
TestSprite:
	ds 8
		incbin "\ResAll\Yquest\C64_YQuest.RAW"
TestChibiko:
	incbin "\ResALL\MinTile\Chibiko2_C64.raw"


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
	db 1,1			;Flags

	
	
	


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
		
		
	
	include "/srcALL/V1_MinimalTile.asm"
	include "\SrcAll\BasicFunctions.asm"
		
		
		
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
		
		
		