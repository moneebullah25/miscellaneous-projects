

;Current player pos
PlayerX 	equ $60	
PlayerY 	equ PlayerX+1

;Last player pos (For clearing sprite)
PlayerX2 	equ PlayerX+2
PlayerY2 	equ PlayerX+3


TileSmoothXmove equ 1	;move in blocks <8 pixels
;TileSmoothYmove equ 1	;This would just waste cpu power

VscreenMinX equ 84		;Top left of visible screen in logical co-ordinates
VscreenMinY equ 80

;VscreenWid equ 24		;Visible Screen Size in logical units
;VscreenHei equ 24

;LIMITATION.. The Virtual screen cannot be smaller than the sprite or 
;the crop will malfunction! (It can be the same size)

VscreenWid equ 88		;Visible Screen Size in logical units
VscreenHei equ 92

	
VscreenWidClip equ 2	;alter right boundary due to working in words
VscreenHeiClip equ 0



TileCache equ $1100


offset equ TileCache-2
offset2 equ TileCache-1
TileClear equ TileCache-3
spritehclip equ TileCache-4
striproutine equ TileCache-5
	 
ChibikoDef equ $10C0
ChibicloneDef equ $10D0

z_Regs 		equ $20

ScreenBase equ $1E00


	include "\SrcAll\BasicMacros.asm"
	
	


;* = $1001
		; BASIC program to boot the machine language code
;		db $0b, $10, $0a, $00, $9e, $34, $31, $30, $39, $00, $00, $00


* = $A000
		dw ProgramStart
		dw ProgramStart
		db $41,$30,$C3,$C2,$CD		;ROM Header
ProgramStart:

	sei             ;Disable interrupts
	

;Screen Init
	ldx #16					;We're going to copy 16 registers 
ScreenInitAgain:	
	dex
	lda VicScreenSettings,x	;Get A parameter
	sta $9000,X				;Store to the video registers at $9000
	txa
	bne ScreenInitAgain

	
;Fill the color ram
	loadpair z_hl,$9600		;Color ram
	loadpair z_bc,$0200
	lda #3					;color to fill
	jsr cldir				;Clear screen bytes
		
		
		
		
	
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
	
	
	lda #0
	sta offset
	
	
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
		
	
PrintChar:
	Rts
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
GetScreenPos:
	lda #$1e			;Screen base is $1E00
	sta z_h					;Colors at $9600 (add $7800 offset)

	lda z_b				;Xpos - 4 LU per tile
	lsr
	lsr
	sta z_l

	lda z_c				;Ypos - 4 LU per tile
	lsr
	lsr
	tay
	beq GetVDPScreenPos_YZero
GetVDPScreenPos_Addagain:	;Repeatedly add screen width
	clc							; (22) Y times 
	lda z_l
	adc #22			;22 bytes per line
	sta z_l
	lda z_h
	adc #0			;Add Carry
	sta z_h
	
	dey
	bne GetVDPScreenPos_Addagain
GetVDPScreenPos_YZero:
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


		
FlipLUT: 	;Src,Dest Substitution list
	db $3C,$3E	;<>
	db $1C,$2F	;/
	db $1B,$1D  ;[]
	db $28,$29	;()
	
	db $7E,$7C	;TopLeft
	db $7B,$6C	;BottomLeft
	db $61,$E1	;Left
	
	db $FF,$7F 	;Checkerboard (corners)
	
	db $EC,$FB 	;BottomLeft Inv
	db $FC,$FE 	;TopLeft Inv
	
	db 0		;End of list
	
	
	;Bitmap Data
TestSprite:
	db $80,$81,$82,$83,$84,$85,$86,$87,$88
TestChibiko:
	incbin "\ResAll\MinTile\TileTestPET.raw"
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
		
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	
;We Return ---FRLDU in z_h for Player 0, and z_L for Player 1

ReadJoystick:	;Returns %--21RLDU	
	lda #%01111111
	sta $9122	;Set Data Direction of port B to READ (0=read)
	sta z_h

	lda $9120	;Port B (R------- Switch)
	sta z_as
		
	lda $911F	;Port A (--FLDU-- Switches)
	rol
	rol
	rol
	rol z_h		;Shift in Fire
	rol z_as		
	rol z_h		;Shift in Right
	rol
	rol z_h		;Shift in Left
	rol
	rol z_h		;Shift in Down
	rol
	rol z_h		;Shift in Up
	
	lda z_h	
	rts
	
		

VicScreenSettings:
	db $0C		;$9000 - horizontal centering
	db $26		;$9001 - vertical centering
	db $96		;$9002 - set # of columns / 
					;Bit7 = screen base bit ($16 for screen at $1000)
	db $AE		;$9003 - set # of rows
	db $7A		;$9004 - TV raster beam line
	db $F0		;$9005 - bits 0-3 start of character memory /  
					;bits 4-7 is rest of video address 
					;$(CF for screen at $1000)
	db $57		;$9006 - horizontal position of light pen
	db $EA		;$9007 - vertical position of light pen
	db $FF		;$9008 - Digitized value of paddle X
	db $FF		;$9009 - Digitized value of paddle Y
	db $00		;$900A - Frequency for oscillator 1 (low)
	db $00		;$900B - Frequency for oscillator 2 (medium)
	db $00		;$900C - Frequency for oscillator 3 (high)
	db $00		;$900D - Frequency of noise source
	db $00		;$900E - bit 0-3 sets volume of all sound / 
					;bits 4-7 are auxiliary color information
	db $00+8 	;$900F - Screen and border color register