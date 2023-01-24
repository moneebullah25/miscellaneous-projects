
ScreenBase equ $0000+64


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

TileCache equ $2300


offset equ TileCache-2
offset2 equ TileCache-1
TileClear equ TileCache-3
spritehclip equ TileCache-4
striproutine equ TileCache-5
ZeroTileCacheRevM equ  TileCache-6
ChibikoDef equ $22C0
ChibicloneDef equ $22D0


TestSprite equ $0100
TestChibiko equ $0111

;Current player pos
;PlayerX 	equ $60		
;PlayerY 	equ PlayerX+1



z_Regs 		equ $20



	setdp $2000			;Define the direct page as #$2000

	org $4000		;bank $0	
ProgramStart:
	sei				;Disable interrupts
	csh				;Highspeed Mode
	cld				;Clear Decimal mode
	
	
	
	
	
	    ;      T12 - TIQ, IRQ1, IRQ2
	lda #%00000111
	sta $1402		;IRQ mask... 1=Off
	
	lda #$f8		;map in RAM
	tam #%00000010	;TAM1 (2000-3FFF)

	lda #$ff		;map in I/O (#$ff)
	tam #%00000001	;TAM0 (0000-1FFF)
	tax				
	txs				;Init stack pointer
		
	;Page in the banks of our cartridge
	lda #$00		;map in ROM
	tam #%00000100	;TAM1 (4000-5FFF)
	lda #$01		;map in ROM
	tam #%00001000	;TAM1 (6000-7FFF)
	lda #$02		;map in ROM
	tam #%00010000	;TAM1 (8000-9FFF)
	lda #$03		;map in ROM
	tam #%00100000	;TAM1 (A000-BFFF)
	lda #$04		;map in ROM
	tam #%01000000	;TAM1 (C000-DFFF)
	
	
	jmp Restart		;Jump to $4000
Restart:		
	;Page in last bank (We were running here before)
	lda #$05		;map in ROM
	tam #%10000000	;TAM1 (E000-FFFF)	
	

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
	
;Screen Shape	
	st0 #10			;Horizontal  Sync  Register (HSR)
	st1 #$02
	st2 #$02
	
	st0 #11			;Horizontal Display Register (HDR)
	st1 #$1F
	st2 #$03
	
	st0 #12			;Vertical Sync Register  (VPR)
	st1 #$02
	st2 #$0F
	
	st0 #13			;Vertical Display Register (VDR)
	st1 #$EF
	st2 #$00
	
	st0 #14			;Vertical Display End Position Register (VCR)
	st1 #$03
	st2 #$00
	
;Reset Background scroll registers
	st0 #7				;Background X-scroll (------XX XXXXXXXX)
	st1 #0
	st2 #0
	
	st0 #8				;Background Y-scroll (-------Y YYYYYYYY)
	st1 #248			;Move Byte pos 0 to top left of screen 
	st2 #0				
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
;	Init Palette	
	ldx #0
	ldy #0
	loadpair z_hl,Palette
	stz $0402			;Palette address L
	stz $0403			;Palette address H
PaletteAgain:
	lda (z_hl),y
	sta $0404		;GGRRRBBB
	iny
	lda (z_hl),y
	sta $0405		;-------G
	iny
	inx
	cpx #16
	bne PaletteAgain

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
;	Cls

	st0 #0			;VDP reg 0 (address)
	st1 #$00		;L - Start of tilemap $0000
	st2 #$00		;H
	
	st0 #2			;Select VDP Reg2 (data)	
	
	ldx #4
	ldy #0			;1024 tiles total (32x32)
ClsAgain:	
	st1 #0			;Fill the entire area with our "Space tile"
	st2 #%00000001		;(tile 256)
	dey
	bne ClsAgain
	dex 
	bne ClsAgain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		


;Send the tiles to VRAM (Normal + X-flip)
	
	lda #<Bitmap			;Source Bitmap Data
	sta z_L
	lda #>Bitmap
	sta z_H
	
	lda #<(BitmapEnd-Bitmap) ;Source Bitmap Data Length
	sta z_C
	lda #>(BitmapEnd-Bitmap)
	sta z_B
	
	lda #<$1000				;Tile 256
	sta z_E
	lda #>$1000
	sta z_D
	jsr DefineTilesCombo 	;Define the tile patterns
	
	
	
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
	incbin "\ResAll\Yquest\PCE_YQuest.RAW"

	incbin "\ResALL\MinTile\Chibiko2_PCE.RAW"
BitmapEnd
	
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
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

GetScreenPos:
	lda z_b			;Xpos in logical units (Pairs of pixels)
	lsr
	lsr
	sta z_b			;Xpos in tiles

	st0 #0			;Select Vram Write
	lda z_c
	clc
	adc #8			;2 lines down
	sta z_c		
	
	and #%00011100	;Multiply Ypos by 32 - Low byte
	asl
	asl
	asl
	clc
	adc z_b			;Add Xpos
	sta z_l		;Send to Data-L
	
	lda z_c
	and #%11100000	;Multiply Ypos by 32 - High Byte
	lsr
	lsr
	lsr
	lsr
	lsr
	sta z_h		;Send to Data-H
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;z_HL = VRAM Dest
;z_BCs = Tilemap
;z_DEs = Pattern data
;z_IYL = TileMap Width

DoStrip:
	st0 #0				;Select Memory Write Reg
	lda z_L
	sta $0102 			;st1 - L address
	lda z_H
	sta $0103 			;st2 - H Address
	st0 #2				;Select Data reg
	ldx #0
NextTile:
DrawTile:
	lda (z_bcs,x)		;BC=Tilemap data
	beq EmptyTile		;EmptyTile
	tay
		lda TileClear
		beq NoClear
		txa
		sta (z_bcs,x)	;Zero tile in cache
NoClear:
	tya					;Tilenumber (L)
	
	clc
	adc z_es			;Add DE Base Tile 
	sta $0102			;Send to Data-L
	lda z_ds		
	adc #0
	sta $0103			;Send to Data-H
	
	inc z_l				;Update Vram Dest
	bne TileVramHok		
	inc z_h
TileVramHok:

EmptyTile3:
	inc z_Cs			;Update TIlemap Source
	bne	TileMapBok
	inc	z_Bs
TileMapBok:

	dec z_iyl
	beq TileDone2
		jmp	NextTile
TileDone2:
	rts
	
EmptyTile:
	inc z_l
	bne EmptyTile2
	inc z_h
EmptyTile2:

	st0 #0				;Select Memory Write Reg
	lda z_L
	sta $0102 			;st1 - L address (%nnnnnnnn)
	lda z_H
	sta $0103 			;st2 - H Address (%ppppNNNN)
	st0 #2				;Select Data reg
	jmp EmptyTile3
	
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
DoStripRev:
	st0 #0				;Select Memory Write Reg
	lda z_L
	sta $0102 			;st1 - L address
	lda z_H
	sta $0103 			;st2 - H Address
	st0 #2					;Select Data reg
	ldx #0
NextTileRev:

	lda (z_bcs,x)	;BC=Tilemap data
	beq EmptyTileRev	;EmptyTile
	tay
		lda TileClear
		beq NoClearRev
		txa
		sta (z_bcs,x)
NoClearRev:
	tya
	
	clc
	adc z_es
	sta $0102		;Send to Data-L (%nnnnnnnn)
	
	lda z_ds
	adc #4			;Reversed tile set (Vram $4000+)
	sta $0103		;Send to Data-H	(%ppppNNNN)

CustomTileDoneRev:
	inc z_l
	bne TileVramHok2
	inc z_h
TileVramHok2:

EmptyTileRev3:
	lda z_Cs
	bne	TileMapBok2
	DEC z_Bs
TileMapBok2:		
	DEC	z_Cs

	dec z_iyl
	beq TileDone2Rev
		jmp	NextTileRev
TileDone2Rev:
	rts
	
EmptyTileRev:
	inc z_l
	bne EmptyTileRev2
	inc z_h
EmptyTileRev2:

	st0 #0				;Select Memory Write Reg
	lda z_L
	sta $0102 			;st1 - L address
	lda z_H
	sta $0103 			;st2 - H Address
	st0 #2				;Select Data reg
	jmp EmptyTileRev3
	


	
	
	
	
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
	;R:      3210
	;W:		   CS			C=Clear S=Select key/dir
	
	;Reset the Multitap... following reads will read in 
		;from joysticks 1-5
	ldx #%00000001			;Reset Multitap 1
	jsr JoypadSendCommand
	ldx #%00000011			;Reset Multitap 2
	jsr JoypadSendCommand

	jsr Player_ReadControlsOne	;Read Pad 1
	sta z_h
	rts
	
								;Read Pad 2
Player_ReadControlsOne:	
	ldx #%00000001				
	jsr JoypadSendCommand	;----LDRU (Left/Down/Right/Up)
	jsr JoypadShiftFourBitsA
	dex
	jsr JoypadSendCommand	;---RSBA (Run/Start/B/A)
	jsr JoypadShiftFourBits
	lda z_as
	sta z_l
	rts
	
JoypadShiftFourBitsA:		;Swap LDRU to RLDU
	ror						;Up
	ror z_as		
	ror						;Right (for later)
	ror z_l			
	ror						;Down
	ror z_as
	ror						;Left
	ror z_as
	rol z_l					;Right
	ror z_as
	rts	

JoypadShiftFourBits:		;Shift RSBA in to z_as
	ldx #4
JoypadShiftFourBitsB:
	ror
	ror z_as
	dex
	bne JoypadShiftFourBitsB
	rts
	
JoypadSendCommand:
	stx $1000			;Set option from X
	
	PHA 				;Delay
	PLA 
	NOP 
	NOP
	
	lda $1000			;Load result
	rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



	
	
	;BC=Bytes
	;DE=Destination Ram
	;HL=Source Bytes
	
	
DefineTilesCombo:
	lda z_b
	pha
	lda z_h
	pha
		lda #0			;Unflipped
		jsr DefineTiles
	pla
	sta z_h
	pla 
	sta z_b

	lda #$40			;Xflipped from $4000+
	clc
	adc z_d
	sta z_d
	
	lda #1				;Xflip
DefineTiles:		

	sta z_iyl			;Flip mode
	
	st0 #0				;Select Memory Write Reg
	lda z_e
	sta $0102 			;st1 - L address
	lda z_d
	sta $0103 			;st2 - H Address
	
	st0 #2				;Select Data reg
	ldx z_C				;B=High byte of count - X=Low byte
	ldy #0	
DefineTilesAgain:
	jsr DoSourcePatternByte
	sta $0102			;Store Low byte
	jsr DoSourcePatternByte
	sta $0103			;Store High Byte
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
	
	
;Get (and flip if required) a source byte

DoSourcePatternByte:
	lda z_iyl			;Mode (0=normal 1=xflip)
	bne DoXFlipByte
	lda (z_HL),Y		;Load a byte
	iny
	rts
	
DoXFlipByte:
	lda (z_HL),Y		;Load a byte
	
	rol					;Xflip it
	ror z_d
	rol
	ror z_d
	rol
	ror z_d
	rol
	ror z_d
	rol
	ror z_d
	rol
	ror z_d
	rol
	ror z_d
	rol
	ror z_d
	
	lda z_d
	iny					;Inc Source offset
	rts	
	
	org $5ffe
	dw $E000			;Reset Vector 

	
	
	include "/srcALL/V1_MinimalTile.asm"
	include "\SrcAll\BasicFunctions.asm"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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


palette:
    dw %0000000000000000; ;0  %-------GGGRRRBBB
    dw %0000000000100111; ;1  %-------GGGRRRBBB
    dw %0000000111000111; ;2  %-------GGGRRRBBB
    dw %0000000111111111; ;3  %-------GGGRRRBBB
    dw %0000000000000100; ;4  %-------GGGRRRBBB
    dw %0000000000100100; ;5  %-------GGGRRRBBB
    dw %0000000100000100; ;6  %-------GGGRRRBBB
    dw %0000000110110110; ;7  %-------GGGRRRBBB
    dw %0000000100100100; ;8  %-------GGGRRRBBB
    dw %0000000000111000; ;9  %-------GGGRRRBBB
    dw %0000000111000000; ;10  %-------GGGRRRBBB
    dw %0000000111111000; ;11  %-------GGGRRRBBB
    dw %0000000000000111; ;12  %-------GGGRRRBBB
    dw %0000000000111111; ;13  %-------GGGRRRBBB
    dw %0000000111000111; ;14  %-------GGGRRRBBB
    dw %0000000111111111; ;15  %-------GGGRRRBBB

	
	org $ffff
	db $0			;End of cartridge
	
	