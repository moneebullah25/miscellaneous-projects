
ChibiTracks_AllowRelocation equ 1			;Allow Alt Load address
ChibiTracks_AllowSpeedChange equ 1			;Allow SongSpeed

chibisoundram equ $4000

SongOffset equ ChibiSoundRam+ChannelDataLengthTotal 
; Dw 0	;Remap internal addresses in song (eg compiled for &8000, 
;	loaded to &2000 = offsets of -&6000

SongBase  equ SongOffset+2 ; dw Song1
Instrumentlist equ SongBase+2 ;
Patternlist equ Instrumentlist+2
SongChannels equ Patternlist+2 ; db 0
SongSpeed equ  SongChannels+1 ; db 0
RepPoint equ  SongSpeed+1 ; db 0




;Options For SMS/BBC
;SmsSimpleNoise equ 1	;This limits noise frequency to 0-2
						;Otherwise we use channel 2 for frequency
SmsTranspose equ 1		;Pitchshift

ScreenBase equ $5000


	include "\SrcAll\BasicMacros.asm"
	
	
; *** ALTERNATE sprite routine - allows single vline movement

	macro PushPair,ra	;Push a pair onto the stack (eg PushPair z_HL)
		lda \ra			
		pha				;Push lower Zpage entry
		lda \ra+1
		pha
	endm				;Push higher Zpage entry
	
	macro PullPair,ra	;Pull a pair onto the stack (eg PullPair z_HL)
		pla
		sta \ra+1		;Pull lower Zpage entry
		pla
		sta \ra			;Pull higher Zpage entry
	endm
	
	
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
;PlayerX 	equ $60		
;PlayerY 	equ PlayerX+1

spritehclip equ $60

z_Regs 		equ $20

RunLocation equ $0300

	ORG RunLocation  ;Actually our code runs at &3000 - but we shift it to here
BBCFirstByte:
	SEI			;Stop interrupts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
;Stop the sound chip making a noise!

	;&43 = Data Dir Reg A
	;&40 = I/O Reg B &40
	;&41 = I/O Reg A &41
	
	lda #255		;Set all bits to write
	sta $FE43 ; Data direction port
	
	;	  1CCOVVVV = CC=channel O=operation (1=volume) V=Value (Volume 15=off)
	lda #%10011111	;Turn off channel 0
	sta $FE41
		
	    ; ----BAAA   =A=address (0=sound chip, 3=Keyboard) B=new setting for address AAA
	lda #%00001000		;Send data to Sound Chip
	sta $FE40			
	lda #%00000000		;Stop sending data to sound chip
	sta $FE40
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Transfer program from load address ($3000) 
;To Run address $0200 (out the way of screen)
	
	lda #$30	;Source H $3000
	sta z_h
	
	lda #>(BBCLastByte-BBCFirstByte+256)
	sta z_b		;Byte count H
	
	lda #>RunLocation
	sta z_d		;Destination H $0200
	
	ldy #0		;Low byte of address
	sty z_l
	sty z_e

BBCLDIR:		
    lda (z_HL),Y
    sta (z_DE),Y
	iny
	BNE	BBCLDIR_SkipInc1
	INC	z_H ;Inc Ybytes of address
	INC	z_D
	DEC z_B
	BEQ	BBCLDIR_Done
BBCLDIR_SkipInc1:
	sec	;Relative jump (JR)
	bcs BBCLDIR	;this program code is relocated
BBCLDIR_Done:
;Jump to the new address in copied code ($0200)
	jmp start 
start:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;ScreenInit


	lda #$D8		;Mode 1
	sta $FE20		;Video ULA Control	
SendULA:
	ldx #0
NextULAreg	
	lda ULAConfig,X
	sta $FE21		;ULA Load in color config
	
	stx $FE00		;CRTC Reg Select
	lda CRTCConfig,X
	sta $FE01		;CRTC Reg Data
	
	inx
	cpx #16
	bne NextULAreg	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	ldy #0
		loadpair z_hl,FlipLUT
FillLutAgain:
		tya
		;sta z_b
		and #%10001000	;A
		clc
		ror
		ror
		ror
		sta z_c
		tya ;lda z_b
		and #%01000100	;B
		ror
		ora z_c
		sta z_c
		tya ;lda z_b
		and #%00100010	;C
		rol
		ora z_c
		sta z_c
		tya ;lda z_b
		and #%00010001	;D
		rol
		rol
		rol
		ora z_c
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	jsr ChibiSoundPro_Init	;Init Platform Driver
	
	lda #0
	sta SongOffset			;For relocating song in ram
	sta SongOffset+1			

	lda #<Song1
	sta SongBase			;address of song
	lda #>Song1
	sta SongBase+1			

	jsr ChibiTracks_StartSong	;Setup Song

	;lda #10
	;sta SongSpeed			;Override song speed
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
;Can't replace $FFFE-F directly, have to patch in to the OS Vector

	lda #<InterruptHandler
	sta $0204 ;$FFFE (All IRQ vector)
	
	lda #>InterruptHandler
	sta $0205 ;$FFFF 
	
	;lda #<InterruptHandlerB
	;sta $0206 ;$FFFE (Unrecognised IRQ vector)
	
	;lda #>InterruptHandlerB
	;sta $0207 ;$FFFF;
	
	cli				;Clear interrupt lock flag

;                                      	ld bc,&6060
	loadpair z_bc,$6060
infloop:
	pushpairsafe z_bc
		;jsr ChibiTracks_Play
	
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
	
	
	
	align 4
TestSprite:
	ds 16
		incbin "\ResAll\Yquest\BBC_YQuest.RAW"
TestChibiko:
	incbin "\ResALL\MinTile\TileTestBBC.RAW"

TileCache:
	ds 24*32

	
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
	
DoStrip:
	ldx #0
NextTile:
DrawTile:
		lda (z_bcs,x)	;BC=Tilemap data
		beq CustomTileDone	;EmptyTile
		sta z_hs
		
		lda TileClear

		beq NoClear
		txa
		sta (z_bcs,x)
		
NoClear:
		txa	;=0
		tay	;=0

		lsr z_hs
		ror
		lsr z_hs
		ror
		lsr z_hs
		ror
		lsr z_hs
		ror
		;clc
		adc z_es
		sta z_ls
		
		lda z_hs
		adc z_ds
		sta z_hs	;HL=Bitmap Sourc
		
DrawTileMore:		
		lda (z_HLs),y
		sta (z_HL),y
		iny
		
		lda (z_HLs),y
		sta (z_HL),y
		iny
		
		lda (z_HLs),y
		sta (z_HL),y
		iny
		
		lda (z_HLs),y
		sta (z_HL),y
		iny
		
		lda (z_HLs),y
		sta (z_HL),y
		iny
		
		lda (z_HLs),y
		sta (z_HL),y
		iny
		
		lda (z_HLs),y
		sta (z_HL),y
		iny
		
		lda (z_HLs),y
		sta (z_HL),y
		iny
		
		lda (z_HLs),y
		sta (z_HL),y
		iny
		
		lda (z_HLs),y
		sta (z_HL),y
		iny
		
		lda (z_HLs),y
		sta (z_HL),y
		iny
		
		lda (z_HLs),y
		sta (z_HL),y
		iny
		
		lda (z_HLs),y
		sta (z_HL),y
		iny
		
		lda (z_HLs),y
		sta (z_HL),y
		iny
		
		lda (z_HLs),y
		sta (z_HL),y
		iny
		
		lda (z_HLs),y
		sta (z_HL),y

		
CustomTileDone:
		INC z_Cs
		BNE	CustomTileDoneC
		INC	z_Bs
CustomTileDoneC:
		lda z_l
		clc
		adc #16
		sta z_l
		bcc CustomTileDoneB
		inc z_h
CustomTileDoneB:
		dec z_iyl
		beq TileDone2
			jmp	NextTile
TileDone2:
	rts



DoStripRev:
	lda #>FlipLUT
	sta z_b
	ldx #0
NextTileRev:
DrawTileRev:
		lda (z_bcs,x)	;BC=Tilemap data
		bne NotCustomTileDoneRev
		jmp CustomTileDoneRev	;EmptyTile
NotCustomTileDoneRev:
		sta z_hs
		
		lda TileClear

		beq NoClearRev
		txa
		sta (z_bcs,x)
		
NoClearRev:
		txa	;=0
		tay

		lsr z_hs
		ror
		lsr z_hs
		ror
		lsr z_hs
		ror
		lsr z_hs
		ror
		;clc
		adc z_es
		sta z_ls
		
		lda z_hs
		adc z_ds
		sta z_hs	;HL=Bitmap Sourc
		
		lda z_ls
		pha
		adc #8
		sta z_ls
		
DrawTileMoreRev:		
		lda (z_HLs),y
		sta z_c
		lda (z_bc,x)
		sta (z_HL),y
		iny
		
		lda (z_HLs),y
		sta z_c
		lda (z_bc,x)
		sta (z_HL),y
		iny
		
		lda (z_HLs),y
		sta z_c
		lda (z_bc,x)
		sta (z_HL),y
		iny
		
		lda (z_HLs),y
		sta z_c
		lda (z_bc,x)
		sta (z_HL),y
		iny
		
		lda (z_HLs),y
		sta z_c
		lda (z_bc,x)
		sta (z_HL),y
		iny
		
		lda (z_HLs),y
		sta z_c
		lda (z_bc,x)
		sta (z_HL),y
		iny
		
		lda (z_HLs),y
		sta z_c
		lda (z_bc,x)
		sta (z_HL),y
		iny
		
		lda (z_HLs),y
		sta z_c
		lda (z_bc,x)
		sta (z_HL),y
		iny
		
		pla
		sec
		sbc #8
		sta z_ls
		
		lda (z_HLs),y
		sta z_c
		lda (z_bc,x)
		sta (z_HL),y
		iny
		
		lda (z_HLs),y
		sta z_c
		lda (z_bc,x)
		sta (z_HL),y
		iny
		
		lda (z_HLs),y
		sta z_c
		lda (z_bc,x)
		sta (z_HL),y
		iny
		
		lda (z_HLs),y
		sta z_c
		lda (z_bc,x)
		sta (z_HL),y
		iny
		
		lda (z_HLs),y
		sta z_c
		lda (z_bc,x)
		sta (z_HL),y
		iny
		
		lda (z_HLs),y
		sta z_c
		lda (z_bc,x)
		sta (z_HL),y
		iny
		
		lda (z_HLs),y
		sta z_c
		lda (z_bc,x)
		sta (z_HL),y
		iny
		
		lda (z_HLs),y
		sta z_c
		lda (z_bc,x)
		sta (z_HL),y

		
CustomTileDoneRev:
		lda z_Cs
		bne CustomTileDoneCRev
		dec	z_Bs
CustomTileDoneCRev:
		dec z_Cs
		
		lda z_l
		clc
		adc #16
		sta z_l
		bcc CustomTileDoneBRev
		inc z_h
CustomTileDoneBRev:
		dec z_iyl
		beq TileDone2Rev
			jmp	NextTileRev
TileDone2Rev:
	rts
	
	align 8
FlipLut:
	ds 256
	

	
	
	
	
PrintChar:
	rts	

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
;BBC type is odd - the first 8 screen bytes go DOWN... 
;the 9ths goes back to the top 
;Effectively we're filling in 8x8 character blocks in a zigzag pattern

GetScreenPos:
		lda z_c
		and #%11111100
		asl 
		sta z_c
		
		lsr z_b
		
		lda #0
		sta z_h
		
		lda z_b			;Xpos
		asl
		rol z_h		;2
		asl 
		rol z_h		;4
		asl 
		rol z_h		;8		;8 bytes per X line
		sta z_l
		
		;We have to work in 8 pixel tall strips on the BBC
		lda z_c			;Ypos
		;and #%11111000	;Multiply Y strip num by $02				
		lsr			;$04 00
		lsr			;$02 00		
		
		adc #$50	;Screen Offset $5000
		adc z_h		;Add to D
		sta z_h
				
		lda z_c
		and #%00000111	;Add bottom bits of Y
		adc z_l
		sta z_l
	rts

	

readjoystick:
	sei

	lda #$F0				;Set port to read (For fire button)
	STA $FE43				;SN76489 - Data Direction
	sta z_as
	
	;lda #%00000000			;Get Channel 0 - Joy 1 LR
	jsr Player_ReadControlsGetData
	lda #%00000001			;Get Channel 1 - Joy 1 UD
	jsr Player_ReadControlsGetData
		
	lda $FE40
	and #%00010000			;Get the fire button 1 (PB4 / PB5)
	ora z_as
	eor #%11101111
	sta z_h

	cli
	rts
	
	;See page 429 of the 'BBC Microcomputer Advanced user Guide' 
	
Player_ReadControlsGetData:	;We need to convert analog to digital
	sta $FEC0						;Select channel
Player_ReadControlsDualWait:
	lda $FEC0						;Get Data
	and #%10000000
	bne Player_ReadControlsDualWait	;0= data ready
	
	lda $FEC1						;8 bit analog data
	cmp #255-32
	bcs Player_ReadControlsDualHigh
	cmp #32				
	bcc Player_ReadControlsDualLow 	;Centered
	clc
	bcc Player_ReadControlsDualB	;efective branch always
;	rol z_as
;	clc
;	rol z_as
;	rts
	
Player_ReadControlsDualLow:		;R/D
	sec
Player_ReadControlsDualB:
	rol z_as
	clc
	rol z_as
	rts
Player_ReadControlsDualHigh:	;U/L
	clc
	rol z_as
	sec
	rol z_as
	rts

	include "/srcALL/V1_MinimalTile.asm"
	
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	include "\SrcALL\Multiplatform_ChibiSoundPro.asm"
	include "\SrcALL\Multiplatform_ChibiTracks_Tweener.asm"
	include "\SrcALL\Multiplatform_Fraction16.asm"
	include "\SrcAll\Multiplatform_ChibiTracks.asm"
	
Song1:
	incbin  "\ResALL\ChibiSoundPro\ChibiAkumasTheme.cbt"
	
	;include "\ResALL\ChibiSoundPro\CBT1.asm"
	;include "\ResALL\ChibiSoundPro\CBT2.asm"
	;incbin  "\ResALL\ChibiSoundPro\song.cbt"
	;incbin  "\ResALL\ChibiSoundPro\song2.cbt"
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		
	
InterruptHandler:
;The original processor status byte and return address are
;already stacked ready for an RTI instruction.
;The original X and Y states are still in their registers. The
;original A register contents are in location &FC.

	lda $FE4D
		; v12ALSBK	V=systemVia 1=Timer1 (100hz) 2=Timer2 (speech) 
	and #%00000010	;A=Analog conv L=Lightpen S=Shift reg B=Vblank 
	beq InterruptAbort			;K=Keypress
	
	txa
	pha
	tya 
	pha
		pushpair z_hl
		pushpair z_de
		pushpair z_bc
		pushpair z_ix
			jsr ChibiTracks_Play
		pullpair z_ix
		pullpair z_bc
		pullpair z_de
		pullpair z_hl
	pla
	tay
	pla
	tax
InterruptAbort:	
	lda #255	;Clear all interrupts
	sta $FE4D	;System VIA interrupt
	lda $fc		;The system interrupt handler backs up A at %00FC
	rti
	
;$FE4D Interrupt flag register- %7654321
; 7 = IRQ
; 6 = Timer 1
; 5 = Timer 2
; 4 = CB1 Active Edge
; 3 = CB2 Active Edge
; 2 = Shift Reg complete 8 shifts
; 1 = CA1 Active Edge
; 0 - CA2 Active Edge
	
	include "\SrcAll\BasicFunctions.asm"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	
CRTCConfig:
	db $7F		;0 - Horizontal total
	db $40		;1 - Horizontal displayed characters
	db $5A		;2 - Horizontal sync position
	db $28		;3 - Horizontal sync width/Vertical sync time
	db $26			;4 - Vertical total
	db $00			;5 - Vertical total adjust
	db 24			;6 - Vertical displayed characters (25)
	db 31			;7 - Vertical sync position
	db $01			;8 - Interlace/Display delay/Cursor delay
	db $07			;9 - Scan lines per character
	db %00110000	;10 - Cursor start line and blink type
	db $0			;11 - Cursor end line
	db $0A		;12 - Screen start address H (Address /8)
	db $00		;13 - Screen start address L 

	
ULAConfig:	
Palette0:	;Colours
;		SC  SC		-	S=Screen C=Color
	db $07,$17	;0
	db $47,$57	;0
Palette1:
	db $22,$32		;1
	db $62,$72		;1
Palette2:
	db $81,$91			;2
	db $C1,$D1			;2
Palette3:
	db $A0,$B0				;3
	db $E0,$F0				;3
	
;EOR True   Color
;7  (0) 	black
;6  (1) 	red
;5  (2) 	green
;4  (3) 	yellow (green—red)
;3  (4) 	blue
;2  (5) 	magenta (red—blue)
;1  (6) 	cyan (green—blue)
;0  (7) 	white

BBCLastByte: db 0