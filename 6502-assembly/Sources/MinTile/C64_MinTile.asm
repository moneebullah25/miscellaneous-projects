FourColor equ 1
z_Regs 		equ $30

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



;Current player pos
spritehclip equ $60




ScreenBase equ $6020


	include "\SrcAll\BasicMacros.asm"
	

GameRam equ $6000

;Init Routine
*=$0801
	db $0E,$08,$0A,$00,$9E,$20,$28,$32,$30,$36,$34,$29,$00,$00,$00  
*=$0810	;Start at $0810

	;	  LXMSHVVV - L=Cur Line X=extended BG M=mode 
				;(Txt/Bmp) S=screen on H=height V=Vert scroll
	lda #%00111011	;turn on graphics mode
	sta $D011
	
	
	;     ---MWHHH - M=Multicolor W=scr width H=horiz scroll
	ifdef FourColor
		lda #%11011000  ;1=Multicolor 4 coor 
	else
		lda #%11001000  ;0=standard 2 color 
	endif
	sta $D016

	;     SSSSTTT- - T=Text/Bmp screen address S=Screen (color) address
	lda #%00011000  ;T=1 Screen at $2000 					
	sta $D018			;(Other bits have no function in bitmap mode)
	
	lda $DD00
	and #%11111100
	ora #%00000010	;Screen base at $4000 range
	sta $DD00
	
	;	  ----CCCC
	lda #%00000000
	sta $D021		;Background color (only bits #0-#3).	
	lda #%00000000
	sta $D020		;Border 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	loadpair z_hl,$4400		
	loadpair z_bc,$0400		
	ifdef FourColor
		lda #$43			;Magenta / Cyan 
	else 
		lda #$40			;Magenta / Black
	endif
	jsr cldir			;Clear Colors 1
	
	ifdef FourColor
		loadpair z_hl,$D800		
		loadpair z_bc,$03E7
		lda #$01			;White
		jsr cldir			;Clear Colors 2
	endif

	loadpair z_hl,$6000		
	loadpair z_bc,$2000		
	jsr cldir0			;Clear Screen
	
;;;;;;;;;;;;;;;;;;;;;;

;Build the X-flip LUT
	
	ldy #0
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
	sta (z_hl),y
	iny
	bne FillLutAgain
	

	
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
	
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	
	
	
;Address= (X * 8) + (Top5BitsOfY * 40) + $2000
GetScreenPos:
	ldx #0
	stx z_h
	
	lda z_b		;Multiple X (in bytes) by 8
	and #%11111100 ;Convert LU to bytes
	
	asl
	rol z_h			;-----XXX XXXXX---
	sta z_l

	stx z_b			;B=0
	
;40 bytes per Yline =00000000 00101000
	lda z_c
	asl 			;LU to lines 
	and #%11111000	;00000000 YYYYYyyy
	asl
	rol z_b
	asl
	rol z_b
	asl				;00000000 00101000
	rol z_b			;00000YYY YYyyy000
	tax 
		adc z_l		;Add part to total L
		sta z_l
		lda z_b		;Add part to total H
		adc z_h
		sta z_h
	txa 
	asl
	rol z_b
	asl				;00000000 00101000
	rol z_b			;000YYYYY yyy00000
	
	adc #$20		;Offset +4 blocks (Center 32 tile screen)
	adc z_l			;Add part to total L
	sta z_l
	
	lda z_b			;Add part to total H
	adc z_h
	adc #$60		;Screen Base $4000+$2000
	sta z_h
	rts
	


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


DoStrip:
	ldx #0
NextTile:
	lda (z_bcs,x)	;BC=Tilemap data
	beq EmptyTile		
	tay
	
	lda TileClear		;Clear Cache entry?
	beq NoClear
	txa
	sta (z_bcs,x)		;Yes!
		
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
		
	lda (z_HLs),y	;Get a source byte
	sta (z_HL),y	;Store to VRAM
	iny				;Next Byte
	
	lda (z_HLs),y	;Get a source byte
	sta (z_HL),y	;Store to VRAM
	iny				;Next Byte
	
	lda (z_HLs),y	;Get a source byte
	sta (z_HL),y	;Store to VRAM
	iny				;Next Byte
	
	lda (z_HLs),y	;Get a source byte
	sta (z_HL),y	;Store to VRAM
	iny				;Next Byte
	
	lda (z_HLs),y	;Get a source byte
	sta (z_HL),y	;Store to VRAM
	iny				;Next Byte
	
	lda (z_HLs),y	;Get a source byte
	sta (z_HL),y	;Store to VRAM
	iny				;Next Byte
	
	lda (z_HLs),y	;Get a source byte
	sta (z_HL),y	;Store to VRAM
	iny				;Next Byte
	
	lda (z_HLs),y	;Get a source byte
	sta (z_HL),y	;Store to VRAM

EmptyTile:		
	INC z_Cs		;Inc source tilemap
	BNE	TilemapNoB
	INC	z_Bs
TilemapNoB:

	lda z_l
	clc
	adc #8
	sta z_l
	bcc VramDestNoH
	inc z_h
VramDestNoH:

	dec z_iyl
	beq TileDone2
		jmp	NextTile
TileDone2:
	rts



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	

DoStripRev:
	lda #>FlipLUT
	sta z_b
	ldx #0
NextTileRev:
	lda (z_bcs,x)	;BC=Tilemap data
	beq EmptyTiler

	tay
		
	lda TileClear		;Clear Cache entry?

	beq NoClearRev		;YES!
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
		
		lda (z_HLs),y	;Get a source byte
		sta z_c			;Set LUT source
		lda (z_bc,x)	;Get flipped byte
		sta (z_HL),y	;Store to VRAM
		iny				;Next Byte
		
		lda (z_HLs),y	;Get a source byte
		sta z_c			;Set LUT source
		lda (z_bc,x)	;Get flipped byte
		sta (z_HL),y	;Store to VRAM
		iny				;Next Byte
		
		lda (z_HLs),y	;Get a source byte
		sta z_c			;Set LUT source
		lda (z_bc,x)	;Get flipped byte
		sta (z_HL),y	;Store to VRAM
		iny				;Next Byte
		
		lda (z_HLs),y	;Get a source byte
		sta z_c			;Set LUT source
		lda (z_bc,x)	;Get flipped byte
		sta (z_HL),y	;Store to VRAM
		iny				;Next Byte
		
		lda (z_HLs),y	;Get a source byte
		sta z_c			;Set LUT source
		lda (z_bc,x)	;Get flipped byte
		sta (z_HL),y	;Store to VRAM
		iny				;Next Byte
		
		lda (z_HLs),y	;Get a source byte
		sta z_c			;Set LUT source
		lda (z_bc,x)	;Get flipped byte
		sta (z_HL),y	;Store to VRAM
		iny				;Next Byte
		
		lda (z_HLs),y	;Get a source byte
		sta z_c			;Set LUT source
		lda (z_bc,x)	;Get flipped byte
		sta (z_HL),y	;Store to VRAM
		iny				;Next Byte
		
		lda (z_HLs),y	;Get a source byte
		sta z_c			;Set LUT source
		lda (z_bc,x)	;Get flipped byte
		sta (z_HL),y	;Store to VRAM
		
EmptyTiler:		
		lda z_Cs
		bne TilemapNoBr
		dec	z_Bs
TilemapNoBr:
		dec z_Cs
		
		lda z_l
		clc
		adc #8
		sta z_l
		bcc VramDestNoHr
		inc z_h
VramDestNoHr:

		dec z_iyl
		beq TileDone2Rev
			jmp	NextTileRev
TileDone2Rev:
	rts
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	align 8
FlipLut:
	ds 256		;Look up table for Xflip
	


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
TestSprite:
	ds 8
		incbin "\ResAll\Yquest\C64_YQuest.RAW"
TestChibiko:
	incbin "\ResALL\MinTile\Chibiko2_C64.raw"


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


TileCache:
	ds 24*32

offset: db 0
offset2: db 0
TileClear: db 0

ChibicloneDef:
	dw TestSpriteList	;Tilemap
	dw TestChibiko		;Pattern Data
	db 20,32		;Width,Height
	db 64,128		;X,Y
	db 1,1			;RefreshTile,Sprite
	db 64,128		;X,Y
	db 0,0			;Flags
ChibikoDef:
	dw TestSpriteList	;Tilemap
	dw TestChibiko		;Pattern Data
	db 20,32		;Width,Height
	db 96,96		;X,Y
	db 1,1			;RefreshTile,Sprite
	db 64,128		;X,Y
	db 0,0			;Flags

	
	
	


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

	
	
ReadJoystick:	
	;lda $DC00			;Read in Joystick 1
	lda $DC01			;Read in Joystick 2
	sta z_h			;%---FRLDU
	rts
		
PrintChar:
	rts		


	
	include "/srcALL/V1_MinimalTile.asm"
	include "\SrcAll\BasicFunctions.asm"