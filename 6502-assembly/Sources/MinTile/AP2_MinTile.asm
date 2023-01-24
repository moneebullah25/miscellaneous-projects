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


	include "\SrcAll\BasicMacros.asm"
	

FlipLut equ $200	;Lookup table for X-flip

ScreenBase equ $4000+4

	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	

	ORG $0C00	;Program Start
	sei 		;Disable interrupts
	
	lda $C050 	;TXTCLR:   Display Graphics
	lda $C052 	;MIXCLR:   Display Full Screen
	lda $c057 	;HIRES:    Display HiRes Graphics
	lda $C055 	;TXTPAGE2: If 80STORE Off: Display Page 2
	
	loadpair z_hl,$4000
	loadpair z_bc,$1F00
	jsr cldir0			;Clear Screen
	
	
;Build the lookup table
	
	ldy #0
	loadpair z_hl,FlipLUT	;X-Flip Look up table
FillLutAgain:
	tya
	rol
	rol z_c			;Color bit
	
	tya
	ror
	rol z_c			;1
	ror
	rol z_c			;2
	ror
	rol z_c			;3
	ror
	rol z_c			;4
	ror
	rol z_c			;5
	ror
	rol z_c			;6
	ror
	rol z_c			;7
	lda z_c
	sta (z_hl),y	;Store flipped Byte

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
	
	 	
	
	
TileCache:
	ds 24*32

offset: db 0
offset2: db 0
TileClear: db 0
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	
	;Screen layout is split in 3 parts according to Y line
	;AABBBCCC - AA*$0028  BBB*$0080  CCC*$0400
	
GetScreenPos:
	lda #0
	sta z_l
	
	lda z_c				;Ypos
	asl 				;2 Lines per LU
	tay 				;--BBB---	;Multiply by $0080
		and #%00111000
		lsr
		lsr
		lsr					
		lsr				;Shift 1 bit right 
		ror z_l
		adc #$40		;Screen base $4000
		sta z_h
	tya					;AA------		;multiply by $0028
	rol 				;Get 1st A from AA------ 
	bcc GetScreenPos_SecondThird
	
GetScreenPos_ThirdThird:
	lda z_l
	clc
	adc #$50			;3/3 = Add $0050 to address
	jmp GetScreenPos_ThirdDone
	
GetScreenPos_SecondThird:
	rol 				;Get 2nd A from AA------ 
	bcc GetScreenPos_FirstThird
	lda z_l
	clc
	adc #$28			;2/3 = Add $0028 to address
	
GetScreenPos_ThirdDone:	
	sta z_l
	
GetScreenPos_FirstThird:;1/3 = Add nothing to addreess

	lda z_b				;Xpos
	lsr 
	lsr 				;4 LU per tile
	
	clc
	adc #4				;Center 32 tile screen
	adc z_l				;Add X to calculated address
	sta z_l
	rts
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	
DoStrip:
	ldx #0
NextTile:
	lda (z_bcs,x)		;BC=Tilemap data
	beq EmptyTile
	tay
		lda TileClear
		beq NoClear
		txa
		sta (z_bcs,x)		;Zero entry in tilemap
		
NoClear:
		stx z_hs			;=0
	tya 				;Tilenum
	
	asl 
	rol z_hs			;*2
	asl 
	rol z_hs			;*4
	asl 
	rol z_hs			;*8

	adc z_es			;DE=Pattern Source address
	sta z_ls
	lda z_hs
	adc z_ds
	sta z_hs			;HLs=Bitmap Source
	
	
	ldy #0				;Y=Pattern source
	lda z_h				;HL=Vram Dest
	pha
DrawTileMore:		
		lda (z_HLs),y	;Get a source byte
		sta (z_HL,x)	;Write to screen
		
		inc z_H			;Down &0400 (1 line)
		inc z_H
		inc z_H
		inc z_H
		
		iny
		cpy #8			;Repeat 8 times
		bne DrawTileMore

	pla 
	sta z_h				;reset Vram Dest
	
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
	lda #>FlipLUT		;Top byte of LUT address
	sta z_b				;BC=X-flip LUT

	ldx #0
NextTileRev:
	lda (z_bcs,x)		;BC=Tilemap data
	tay
	bne NotEmptyTileR
		jmp EmptyTileR	;EmptyTile
NotEmptyTileR:
		sta z_hs
		
		lda TileClear

		beq NoClearRev
		txa
		sta (z_bcs,x)	;Zero entry in tilemap
		
NoClearRev:
		stx z_hs		;=0
	tya 				;Tilenum
	
	asl 
	rol z_hs			;*2
	asl 
	rol z_hs			;*4
	asl 
	rol z_hs			;*8

	adc z_es			;DE=Pattern Source address
	sta z_ls
	lda z_hs
	adc z_ds
	sta z_hs			;HLs=Bitmap Source
	
	ldy #0				;Y=Pattern source
	lda z_h				;HL=Vram Dest
	pha
DrawTileMoreRev:		
		lda (z_HLs),y	;Get a source byte
		sta z_c			;Set LUT address
		lda (z_BC,x)	;Get Flipped Byte
		sta (z_HL,x)	;Store flipped byte
		
		inc z_H			;Down &0400 (1 line)
		inc z_H
		inc z_H
		inc z_H
		iny
		
		cpy #8			;Repeat 8 times
		bne DrawTileMoreRev
	pla 
	sta z_h

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
	

;;;;;;;;;;;;;;;;;;;;;;

	
	align 4
TestSprite:
	ds 8
		incbin "\ResAll\Yquest\AP2_YQuest.raw"
TestChibiko:
	incbin "\ResALL\MinTile\Chibiko2TilesAP2.RAW"


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
		

	
;Apple Joysticks are annoying!
;they are analog... we have to strobe the port 
;then read from the X and Y ports, and count up until the top bit changes
;this is a 'timer'...using just 1 bit (the top one) 
;it effectively returns an 'analog' value from about 0-100
	
ReadJoystick:	;---FRLDU
	lda $C061			;Fire 1
	and #%10000000
	rol	;Move in the fire button
	rol z_h
	
	lda $C070			;Strobe Joypads
	ldy #0
	ldx #0 
Joy_ReadAgain:
	pha
	pla					;delay
Joy_gotPDL1:			;Jump backhere when we get X
Joy_ChkPDl0:
	lda	$C064 			;<--SM ***   Y
JoySelfModAA_Plus2:
	bpl Joy_gotPDL0		;Have we got Y?
	nop
	iny	
	lda $C065			;<--SM ***   X
JoySelfModB_Plus2:
	bmi Joy_nogots		;Have we got X?
	bpl Joy_gotPDL1
Joy_nogots:
	inx
	jmp Joy_ChkPdl0
Joy_gotPDL0:			;We've Got Tpos - just waiting for X
	lda  $C065			;<--SM ***   X
JoySelfModBB_Plus2:
	bmi Joy_Nogots
	
	tya
	jsr JoyConvertAnalog;Convert Y
	txa
						;Convert X
	jsr JoyConvertAnalog;Convert Y
	lda z_h
	eor #%11111111
	sta z_h

PrintChar:
	rts
	
JoyConvertAnalog:	;covert analog from 0-100 into L/R or U/D
	cmp #$66
	bcs Joy_Rbit
	cmp #$33
	bcc Joy_Lbit
	clc 
	bcc Joy_Cbit
Joy_Rbit:
	sec 
Joy_Cbit:
	rol z_h
	clc
	rol z_h
	rts
	
Joy_Lbit:
	clc
	rol z_h
	sec 
	rol z_h
	rts