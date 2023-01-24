

;Current player pos
PlayerX 	equ $60	
PlayerY 	equ PlayerX+1

;Last player pos (For clearing sprite)
PlayerX2 	equ PlayerX+2
PlayerY2 	equ PlayerX+3


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



TileCache equ $1400


offset equ TileCache-2
offset2 equ TileCache-1
TileClear equ TileCache-3
spritehclip equ TileCache-4
striproutine equ TileCache-5
	 
ChibikoDef equ $0300
ChibicloneDef equ $0310

z_Regs 		equ $20

ScreenBase equ $8000+4


	include "\SrcAll\BasicMacros.asm"
	
	


*=$0401
	db $0e,$04,$0a,$00,$9e,$20,$28, $31,$30,$34,$30,$29,$00,$00,$00
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	INIT

ProgramStart:        
	sei             ;Disable interrupts
	
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
	
	
	loadpair z_hl,$8000		
	loadpair z_bc,(40*25)
	lda #32
	jsr cldir			;Clear Screen
	
	
	lda #0
	sta offset

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
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		
;We Return ---FRLDU in z_h for Player 0, and z_L for Player 1

ReadJoystick:	;Returns %--21RLDU
	lda #255
	sta z_h				;Cursor buildup
	sta z_l				;Unused 
	
	lda #6				;Line 6
	ldx #%00100000		;Fire 2 (Enter)
	jsr TestCursorBit
	
	lda #9				;Line 9
	ldx #%00000100		;Fire 1 (Space)
	jsr TestCursorBit
	
	lda #4				;Line 4
	ldx #%10000000		;Right (Numpad 6)
	jsr TestCursorBit
	
	;lda #4				;Line 4
	ldx #%01000000		;Left (Numpad 4)
	jsr TestCursorBit
	
	lda #7				;Line 7
	;ldx #%01000000		;Down (Numpad 2)
	jsr TestCursorBit
	
	lda #3				;Line 3
	;ldx #%01000000		;Up	(Numpad 8)
	jsr TestCursorBit
	lda z_h
	rts
	
TestCursorBit:				;A=Line X=Mask
	pha
		sta $E810			;Select line
		txa
		and $E812			;test key
		clc					;Clear carry (Pressed)
		beq TestCursorBitB		
		sec					;Set carry (not pressed)
TestCursorBitB:	
		rol z_h				;shift the key into the buildup
	pla
	rts
	
PrintChar:
	Rts


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;$8000 + Ypos *40 + Xpos		Ypos*40=32+8
	
	;40 bytes per line = * %00000000 00101000
	;We shift our Ypos 3 to the right, add it, then another 2, eg
	
	;%00000000 00101000	=40 (32+8)
	;%YYYYYYYY 00000000
	;%000YYYYY YYY00000	= Y*32
	;%00000YYY YYYYY000 = Y*8
	
GetScreenPos:
	lda #0
	sta z_h
	
	lda z_c			;2 pixel lines per LU
	and #%11111100
	asl 			
	
	sta z_l			;Ypos *8
	asl
	rol z_h
	asl
	rol z_h
	adc z_l
	sta z_l			;Ypos * 32 
	
	lda z_h
	adc #$80			;Screen base is $8000
	sta z_h
		
	lda z_b			;4 LU per tile
	lsr
	lsr
	adc #4			;Center Virtual screen
	clc
	adc z_l
	sta z_l
	bcc GetVDPScreenPos_Done
	inc z_h
GetVDPScreenPos_Done:
	rts
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DoStrip:
	ldx #0
NextTile:
	lda (z_bcs,x)			;BC=Tilemap data
	
	beq CustomTileDone		;EmptyTile
	tay
	
	lda TileClear
	beq NoClear				;Are we clearing tilemap?
	txa
	sta (z_bcs,x)			;Zero entry in tilemap
NoClear:
	
	lda (z_DEs),y			;Get source block
	sta (z_HL,x)			;Write to screen
	
CustomTileDone:
	INC z_Cs				;Move to next source tile
	BNE	CustomTileDoneC
	INC	z_Bs
CustomTileDoneC:

	inc z_l					;Move to VRAM destination
	bne CustomTileDoneB
	inc z_h
CustomTileDoneB:
	dec z_iyl
	beq TileDone2
		jmp	NextTile
TileDone2:
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DoStripRev:
	lda #>FlipLUT			;Load Xflip Lookup table
	sta z_b
	lda #<FlipLUT
	sta z_c
	ldx #0
	
NextTileRev:
	lda (z_bcs,x)			;BC=Tilemap data
	bne NotCustomTileDoneRev
		jmp CustomTileDoneRev	;EmptyTile
NotCustomTileDoneRev:
	tay
	
	lda TileClear
	beq NoClearRev			;Are we clearing tilemap?
	txa
	sta (z_bcs,x)			;Zero entry in tilemap
	
NoClearRev:
	lda (z_DEs),y			;Get source block
	sta z_hs				;store in temp

	ldy #0					;offset in Xflip LUT
LookUpAgain:		
	lda (z_bc),y
	beq LookUpNotFound		;End of list?
	
	iny
	cmp z_hs				;Does item 1 match?
	beq LookUpFound1
	
	lda (z_bc),y
	iny
	cmp z_hs				;Does item 2 match?
	bne LookUpAgain
	
;LookUpFound2
	dey
	dey
	lda (z_bc),y			;Item 2 match - return item 1
	jmp LookUpFound
	
LookUpFound1:		
	lda (z_bc),y			;Item 1 match - return item 2
	jmp LookUpFound
	
LookUpNotFound:
	lda z_hs				;Can't flip, just use original
LookUpFound:		
	sta (z_HL,x)			;Write to screen
	
	
CustomTileDoneRev:
	lda z_Cs				;Move to next source tile (reverse)
	bne CustomTileDoneCRev
	dec	z_Bs
CustomTileDoneCRev:
	dec z_Cs
	
	inc z_l					;Move to VRAM destination
	bne CustomTileDoneBRev
	inc z_h
CustomTileDoneBRev:
	dec z_iyl
	beq TileDone2Rev
		jmp	NextTileRev
TileDone2Rev:
	rts
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FlipLUT: 					;Src,Dest Substitution list
	db $3C,$3E	;<>
	db $1C,$2F	;/
	db $1B,$1D  ;[]
	db $28,$29	;()
	
	db $7E,$7C	;TopLeft
	db $7B,$6C	;BottomLeft
	db $61,$E1	;Left
	
	db $FF,$7F ;Checkerboard
	
	db $EC,$FB ; BottomLeft Inv
	db $FC,$FE ; TopLeft Inv
	
	db 0
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	;Bitmap Data
TestSprite:
	db $80,$81,$82,$83,$84,$85,$86,$87,$88
TestChibiko:
	incbin "\ResAll\MinTile\TileTestPET.raw"
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	
;db 0,$2F,$45,$45,$1C,$42,$20,$57,$57,$42,$42,11,12,13,$42,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,$42,$42,$30,$3E,$3E,$20,36,37

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
		
		
		