
PetTimerA equ 1 		;MakeChibisoundPro Keep Timer on
SmsTranspose equ 1		;Pitchshift low values

ChibiTracks_AllowRelocation equ 1		;Allow Alt Load address
ChibiTracks_AllowSpeedChange equ 1		;Allow SongSpeed

chibisoundram equ $3000

SongOffset equ ChibiSoundRam+ChannelDataLengthTotal 
; Dw 0	;Remap internal addresses in song (eg compiled for &8000, 
;	loaded to &2000 = offsets of -&6000
SongBase  equ SongOffset+2 ; dw Song1
Instrumentlist equ SongBase+2 ;
Patternlist equ Instrumentlist+2
SongChannels equ Patternlist+2 ; db 0
SongSpeed equ  SongChannels+1 ; db 0
RepPoint equ  SongSpeed+1 ; db 0




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


TileCache equ $3400


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
	
	
	jsr ChibiSoundPro_Init
	

	;lda #0
	;sta SongOffset	;For relocating song in ram
	;sta SongOffset+1	;For relocating song in ram

	lda #<Song1
	sta SongBase	;address of song.
	lda #>Song1
	sta SongBase+1	;address of song.
	
	jsr ChibiTracks_StartSong
	
		
	lda #<InterruptHandler
	sta $0219 ;$0090 on later models
	lda #>InterruptHandler
	sta $021A ;$0091 on later models
	
	
;(You don't actually need to do this, interrupts on by default)

	lda #%00000101	;Bit0=Enable Vblank CB1
	sta $e813		;  Bit2 keeps keyboard working!
	
	
;$E813	PIA1:Screen / Tape	%R-CCCDrr
;	R=Retrace I flag / C=Cassette #1 Motor output CB2 / D=DDRB Access (Data Direction Reg B)
;   r= retrace interrupt control CB1
	
	
;$E84E	VIA:Interrupt enable register	%ETtBbSAa
;  E=Enable or disable following ints / T=T1 interrupt / t=T2 interrupt
;  B=CB1 interrupt / b=CB2 Interrupt / S=SR interrupt / A=CA1 Interrupt
;  a=CA2 interrupt

;$E84B	VIA:Aux Control Register	%1O2SSSPP
;	1=T1 Control PB7 Out / O=One shot free run / 2=T2 control PB6 sense 
;	S=Shift register control / P=PB PA Latch control ... (16= Sound On)	
	
	
	

;Alternatively we can Use the Timer!!!!!!!!!!!!!!!

	lda #%11111110	;Disable Vblank CB1
	sta $e813
  
	lda #0			;Speed L %LLLLLLLL
	sta $e844
	lda #$30		;Speed H %HHHHHHHH
	sta $e845

	lda #%01111111	;Disable all interrupts
	sta $E84E		;(Bit7=0 means Disable, Bit0-6=Interrupts to change)
		
	lda #%11000000	;Enable Timer A (Bit7=1 means Enable)
	sta $E84E		;VIA:Interrupt enable register  Bit6 = T1 interrupt
	
	Lda #%01000000 	;Enable Timer A (Aux)
	sta $E84B		;VIA:Aux Control Register
	
;End of timer code.	
	
	cli
	
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
		
	
	
	;40 bytes per line = * %00000000 00101000
	;We shift our Ypos 3 to the right, add it, then another 2, eg
	
	;%00000000 00101000	=40 (32+8)
	;%YYYYYYYY 00000000
	;%000YYYYY YYY00000	= Y*32
	;%00000YYY YYYYY000 = Y*8
GetScreenPos:

	

	;$8000 + Ypos *40 + Xpos		Ypos*40=32+8
	lda #0
	sta z_h
	
	lda z_c
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
	
	
	
	lda z_b
	lsr
	lsr
	adc #4
	clc
	adc z_l
	sta z_l
	bcc GetVDPScreenPos_Done
	inc z_h
GetVDPScreenPos_Done:

	rts
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
		
		
		ldy Z_hs
		lda (z_DEs),y
		sta (z_HL,x)
		
CustomTileDone:
		INC z_Cs
		BNE	CustomTileDoneC
		INC	z_Bs
CustomTileDoneC:
		inc z_l
		bne CustomTileDoneB
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
	lda #<FlipLUT
	sta z_c
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
		ldy Z_hs
		lda (z_DEs),y
		sta z_hs

		ldy #0
LookUpAgain:		
		lda (z_bc),y
		beq LookUpNotFound
		iny
		cmp z_hs
		beq LookUpFound1
		
		lda (z_bc),y
		iny
		cmp z_hs
		bne LookUpAgain
;LookUpFound2		
		dey
		dey
		lda (z_bc),y
		jmp LookUpFound
LookUpFound1:		
		lda (z_bc),y
		jmp LookUpFound
LookUpNotFound:
		lda z_hs
LookUpFound:		
		sta (z_HL,x)	
		
		
CustomTileDoneRev:
		lda z_Cs
		bne CustomTileDoneCRev
		dec	z_Bs
CustomTileDoneCRev:
		dec z_Cs
		
		inc z_l
		bne CustomTileDoneBRev
		inc z_h
CustomTileDoneBRev:
		dec z_iyl
		beq TileDone2Rev
			jmp	NextTileRev
TileDone2Rev:
	rts
	
FlipLUT: 				;Src,Dest Substitution list
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
	
	;Bitmap Data
TestSprite:
	db $80,$81,$82,$83,$84,$85,$86,$87,$88
TestChibiko:
	incbin "\ResAll\MinTile\TileTestPET.raw"
	db 0,$2F,$45,$45,$1C,$42,$20,$57,$57,$42,$42,11,12,13,$42,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,$42,$42,$30,$3E,$3E,$20,36,37

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
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	
	include "\SrcALL\Multiplatform_ChibiSoundPro.asm"
	
	include "\SrcALL\Multiplatform_ChibiTracks_Tweener.asm"
	include "\SrcALL\Multiplatform_Fraction16.asm"
	include "\SrcAll\Multiplatform_ChibiTracks.asm"
	
Song1:
	;include "\ResALL\ChibiSoundPro\CBT1.asm"
	;include "\ResALL\ChibiSoundPro\CBT2.asm"
	;incbin  "\ResALL\ChibiSoundPro\song.cbt"
	;incbin  "\ResALL\ChibiSoundPro\song2.cbt"
	incbin  "\ResALL\ChibiSoundPro\ChibiAkumasTheme.cbt"
	
InterruptHandler:		;A X Y were pushed by the interrupt handler

		;LDA $E812		;Clear CB1 interrupt (Vblanks)

		lda #%11111111
		sta $E84D		;Clear the timer interrupt
		
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
	pla
	rti

	
	
	include "\SrcAll\BasicFunctions.asm"
		
		
		