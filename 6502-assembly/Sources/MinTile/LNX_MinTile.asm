
;Current player pos
PlayerX 	equ $80	
PlayerY 	equ PlayerX+1

;Last player pos (For clearing sprite)
PlayerX2 	equ PlayerX+2
PlayerY2 	equ PlayerX+3


TileSmoothXmove equ 1	;move in blocks <8 pixels
TileSmoothYmove equ 1	;This would just waste cpu power

VscreenMinX equ 76		;Top left of visible screen in logical co-ordinates
VscreenMinY equ 94

;VscreenWid equ 24		;Visible Screen Size in logical units
;VscreenHei equ 24

;LIMITATION.. The Virtual screen cannot be smaller than the sprite or 
;the crop will malfunction! (It can be the same size)

VscreenWid equ 104		;Visible Screen Size in logical units
VscreenHei equ 68

	
VscreenWidClip equ 2	;alter right boundary due to working in words
VscreenHeiClip equ 0



TileCache equ $E200


FlipLUT equ $E800		;256 byte Lookup table for Xflip bytes


offset equ TileCache-2
offset2 equ TileCache-1
TileClear equ TileCache-3
spritehclip equ TileCache-4
striproutine equ TileCache-5
	 
ChibikoDef equ $b300
ChibicloneDef equ $b310

z_Regs 		equ $20

ScreenBase equ $C000+1


	include "\SrcAll\BasicMacros.asm"
	
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	INIT

	org $200-10		;Our program starts at $0200
	db $80,$08,$02,$00,$40,$0A,$42,$53,$39,$33
	
;ScreenInit	-	SUZY chip needs low byte setting first 
					;OR IT WILL WIPE THE HIGH BYTE!

	;Set screen ram pointer to $C000
	stz $FD94		;DISPADR	Display Address L (Visible)
	lda #$C0	
	sta $FD95		;DISPADR	Display Address H (Visible)
	
	sei
	
;Do the palette
	ldx #0
	ldy #0
	loadpair z_hl,Palette
	stz $2121		;CGADD - Colour selection  
PaletteAgain:
		 ;gggrrrrr 
	lda (z_hl),y
	sta $FDB0,x		;CGDATA - Colour data register
		 ;?bbbbbgg 
	iny
	lda (z_hl),y
	sta $FDA0,x		;CGDATA
	iny
	inx
	cpx #16
	bne PaletteAgain

	
	loadpair z_hl,(TileCache-8)
	loadpair z_bc,$400
	jsr cldir0
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Build the X-Flip LUT

	ldy #0
	loadpair z_hl,FlipLUT
 FillLutAgain:
	tya
	jsr SwapNibbles			;%AAAABBBB = %BBBBAAAA
	sta (z_hl),y
	iny
	bne FillLutAgain


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
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
		
	;Vram= $C000 + YposInLines * 80 + Xpos in bytes
	
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


GetScreenPos:
	lda z_c 		;Move Y into top byte 	= YYYYYYYY 00000000
	
	;and #%11111100		;Limit to whole tile movement
	;sta z_c 			;Limit to whole tile movement
	
	lsr				
	clc
	adc z_c			;+1/2 (6 lines per tile (4 Logical Units)
		
	stz z_c			;z_C=Lbyte A=High byte
	
	lsr				;Ypos *80 = (80=64+16)
	ror z_c
	lsr 
	ror z_c			;Shift Right Twice      = 00YYYYYY YY000000 
						;Y*64
	sta z_h			;Store High byte in total
	lda z_c			
	sta z_l			;Store Low byte in total
	
	lda z_h			;Shift Right Twice      = 0000YYYY YYYY0000
	lsr					;Y*16
	ror z_c
	lsr 
	ror z_c
	
	clc			;Add High byte to total
	adc z_h
	adc #$C0	;Screen base at &C0000
	sta z_h
	
	lda z_c			;Add Low byte to total
	adc z_l
	sta z_l
	
	bcc GetScreenPos_NoH
	inc z_h			;Add any carry to the high byte
GetScreenPos_NoH:	
	
	clc				;Add the X pos 
	lda z_b
	;and #%11111100		;Limit to whole tile movement
	lsr				
	sta z_b
	lsr
	clc
	adc z_b			;+1/2 (3 bytes per tile (4 Logical Units))
	adc #1			;Center screen (add 1 byte)
		
	adc z_l 
	sta z_l
	
	bcc GetScreenPos_NoH2
	inc z_h			;Add any carry to the high byte
GetScreenPos_NoH2:	
	rts
	
	


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DoStrip:
	ldx #0
NextTile:
	lda (z_bcs,x)	;BC=Tilemap data
	
	beq BlankTile	;EmptyTile
	tay
		lda TileClear
		beq NoClear
		txa				;X=0
		sta (z_bcs,x)
NoClear:			

	tya			;Tilenum
	stx z_b		;Tiles are 6x6 pixels = 3*6 bytes 
					;(18 bytes= 16+2)
	asl 		;*2
	rol z_b
	sta z_ls
	
	lda z_b
	sta z_hs
	
	lda z_ls
	asl			;*4
	rol z_b
	asl			;*8
	rol z_b
	asl			;*16
	rol z_b
		
	adc z_ls
	sta z_ls
			
	lda z_b
	adc z_hs
	sta z_hs	;HL=Bitmap Source

	lda z_ls
	clc
	adc z_es	;Add DE pattern base
	sta z_ls
	
	lda z_hs
	adc z_ds
	sta z_hs
	
	
	lda z_h
	pha
	lda z_l
	pha
		ldx #6			;Lines
DrawTileMore:		
		ldy #0
		lda (z_HLs),y		;Get pattern byte
		sta (z_HL),y		;Write to screen
		iny					;Move to next byte
		
		lda (z_HLs),y		;Get pattern byte
		sta (z_HL),y		;Write to screen
		iny					;Move to next byte
		
		lda (z_HLs),y		;Get pattern byte
		sta (z_HL),y		;Write to screen
		iny					;Move to next byte
		
		tya
		clc
		adc z_Ls			;Update source pattern address
		sta z_Ls
		bcc DrawTileMoreC
		inc z_Hs
DrawTileMoreC:
		
		lda z_L					
		clc
		adc #80				;Add 1 line to VRAM dest
		sta z_L
		bcc DrawTileMoreB
		inc z_H			
DrawTileMoreB:				
		dex					;Repeat 6 times
		bne DrawTileMore
	pla
	sta z_l
	pla 
	sta z_h
	
BlankTile:
	INC z_Cs			;Move to next tile in tilemap
	BNE	BlankTileNoB
	INC	z_Bs
BlankTileNoB:

	lda z_l
	clc
	adc #3				;Move across 1 Tile
	sta z_l
	bcc BlankTileNoH
	inc z_h
BlankTileNoH:

	dec z_iyl			;Repeat for width of strip
	beq TileDone2
		jmp	NextTile
TileDone2:
	rts


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


DoStripRev:
	ldx #0
NextTileRev:
		lda (z_bcs,x)	;BC=Tilemap data
		
		beq BlankTileRev	;EmptyTile
		tay
			lda TileClear

			beq NoClearRev
			txa
			sta (z_bcs,x)
NoClearRev:			

		tya			;Tilenum 
		stx z_b		;Tiles are 6x6 pixels = 3*6 bytes
						;(18 bytes= 16+2)
		asl 		;*2
		rol z_b
		sta z_ls
		
		lda z_b
		sta z_hs
		
		lda z_ls
		asl			;*4
		rol z_b
		asl			;*8
		rol z_b
		asl			;*16
		rol z_b
		
		adc z_ls
		sta z_ls
				
		lda z_b
		adc z_hs
		sta z_hs	;HL=Bitmap Source

		lda z_ls
		clc
		adc z_es	;Add DE pattern base
		sta z_ls
		
		lda z_hs
		adc z_ds
		sta z_hs
		
		lda z_h
		pha
		lda z_l
		pha
			lda #>FlipLUT	;X-Flip LUT
			sta z_b
		
			lda #6		;6 lines
DrawTileMoreRev:		
			ldy #2		;3 bytes per line
			pha
DrawTileMoreRevB:					
				lda (z_HLs,x) 	;Get pattern byte
				
				inc z_Ls		;Next Pattern Byte
				bne DrawTileMoreCRev
				inc z_Hs
DrawTileMoreCRev:				
				
				sta z_c			;Set LUT Entry 
				lda (z_bc,x)	;Read Xflipped from LUT
				sta (z_HL),y	;Write Xfliped byte
				
				dey				;Move to previous VRAM byte
				bpl DrawTileMoreRevB ;<0?
				
				lda z_L
				clc
				adc #80			;Down a line
				sta z_L
				bcc DrawTileMoreNoHRev
				inc z_H			
DrawTileMoreNoHRev:			

			pla
			sec
			sbc #1	;Decrease Line count
			bne DrawTileMoreRev
		pla
		sta z_l
		pla 
		sta z_h
		
BlankTileRev:
		lda z_Cs			;Move to last tile in tilemap
		bne	BlankTileRevNoB
		dec	z_Bs
BlankTileRevNoB:		
		dec z_Cs

		lda z_l
		clc
		adc #3				;Move across one tile
		sta z_l
		bcc BlankTileRevNoH
		inc z_h
BlankTileRevNoH:
		dec z_iyl			;Repeat for width of strip
		beq TileDone2Rev
			jmp	NextTileRev
TileDone2Rev:
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PrintChar:
	rts		

	
ReadJoystick:
	lda #$FF
	sta z_h			;Player 1 
	;sta z_l			;Player 2 - Only one pad so disabled
	
	eor $FCB0		;JOYSTICK	Read Joystick and Switches	UDLR12IO
			;(I= Inside Fire, O=Outside Fire, 1=Option 1, 2=Option 2)
			
	;FCB1 = Pause button on bit 0
			
	jsr SwapNibbles	; swap UDLR12IO to 12IOUDLR	
	tay				;Back up for later

	ldx #4
JoystickNextBitsB:	;shift 12IOUDLR into ---RLDU
	ror
	rol z_h			;Flip the order of the bits
	dex
	bne JoystickNextBitsB
	
	tya				;Get back backup 12IOUDLR
	ora #%00001111	;Get Fires 12IO---
	and z_h			;Or in ---RLDU
	sta z_h
	rts

;We Return ---FRLDU in z_h for Player 0, and z_L for Player 1
		
		
	align 4
TestSprite:
	ds 3*6
		incbin "\ResAll\MinTile\TileTestLNX.raw"
TestChibiko:
	incbin "\ResALL\MinTile\Chibiko2_LNX.raw"


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
	db 76,80		;X,Y
	db 1,1			;RefreshTile,Sprite
	db 64,128		;X,Y
	db 1,1			;Flags

	
	
Palette:
	dw $0000; ;0  -GBR
	dw $00F8; ;1  -GBR
	dw $0FF0; ;2  -GBR
	dw $0FFF; ;3  -GBR
	dw $0080; ;4  -GBR
	dw $0088; ;5  -GBR
	dw $0880; ;6  -GBR
	dw $0CCC; ;7  -GBR
	dw $0888; ;8  -GBR
	dw $000F; ;9  -GBR
	dw $0F00; ;10  -GBR
	dw $0F0F; ;11  -GBR
	dw $00F0; ;12  -GBR
	dw $00FF; ;13  -GBR
	dw $0FF0; ;14  -GBR
	dw $0FFF; ;15  -GBR

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
		
		