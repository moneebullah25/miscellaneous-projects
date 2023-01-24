
ChibiTracks_AllowRelocation equ 1		;Allow Alt Load address
ChibiTracks_AllowSpeedChange equ 1		;Allow SongSpeed

chibisoundram equ $2300	

SongOffset equ ChibiSoundRam+ChannelDataLengthTotal 
; Dw 0	;Remap internal addresses in song (eg compiled for &8000, 
;	loaded to &2000 = offsets of -&6000
SongBase  equ SongOffset+2 ; dw Song1
Instrumentlist equ SongBase+2 ;
Patternlist equ Instrumentlist+2
SongChannels equ Patternlist+2 ; db 0
SongSpeed equ  SongChannels+1 ; db 0
RepPoint equ  SongSpeed+1 ; db 0


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

TileCache equ $2400

offset equ TileCache-2
offset2 equ TileCache-1
TileClear equ TileCache-3
spritehclip equ TileCache-4
striproutine equ TileCache-5
	 
ChibikoDef equ $22C0
ChibicloneDef equ $22D0



TilePatterns equ $100 	
ChibikoPatterns equ $200
YaritaPatterns equ $300

;Current player pos
;PlayerX 	equ $60		
;PlayerY 	equ PlayerX+1



z_Regs 		equ $20



	

	org $4000		;bank $0	
	setdp $2000			;Define the direct page as #$2000
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
	tam #%00000100	;TAM2 (4000-5FFF)
	lda #$01		;map in ROM
	tam #%00001000	;TAM3 (6000-7FFF)
	lda #$02		;map in ROM
	tam #%00010000	;TAM4 (8000-9FFF)
	lda #$03		;map in ROM
	tam #%00100000	;TAM5 (A000-BFFF)
	lda #$04		;map in ROM
	tam #%01000000	;TAM6 (C000-DFFF)
	
	
	jmp Restart		;Jump to $4000
Restart:		
	;Page in last bank (We were running here before)
	lda #$05		;map in ROM
	tam #%10000000	;TAM7 (E000-FFFF)
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		

;	ScreenInit
	st0 #5				;RegSelect 5
		 ;BSXXIIII	Backgroundon Spriteon eXtendedsync Interruptenable
	st1 #%10000000		;Background ON
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

	lda #255
	sta z_ixh
	lda #255
	sta z_ixl
	
	loadpair z_hl,bmpTiles
	loadpair z_bc,(bmpTiles_End-bmpTiles)
	loadpair z_de,TilePatterns*16	;Tile 256
	jsr DefineTilesDuo ;jsr DefineTiles		;Define the tile patterns
	
	
	
	lda #255
	sta z_ixh
	lda #0
	sta z_ixl
	
	
	loadpair z_hl,bmpChibiko
	loadpair z_bc,(bmpChibiko_End-bmpChibiko)
	loadpair z_de,ChibikoPatterns*16	
	jsr DefineTilesDuo ;jsr DefineTiles		;Define the tile patterns
	
	
	lda #0
	sta z_ixh
	lda #255
	sta z_ixl
	
	
	loadpair z_hl,bmpYarita
	loadpair z_bc,(bmpYarita_End-bmpYarita)
	loadpair z_de,YaritaPatterns*16	
	jsr DefineTilesDuo ;jsr DefineTiles		;Define the tile patterns
	
	
	
	
	
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
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	jsr ChibiSoundPro_Init	;init platform driver
	
	lda #<Song1
	sta SongBase	;address of song.
	lda #>Song1
	sta SongBase+1	
			
	jsr ChibiTracks_StartSong
		
	lda #6			;Slow down song for 60hz speed
	sta SongSpeed	;(Song was written for 50 hz systems)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;ScreenInit & Vblank Interrupt enable

	st0 #5				;RegSelect 5
		 ;BSXXIIII	Backgroundon Spriteon eXtendedsync Interrupt-enable
	st1 #%10001000		;Background ON (I=8.. Vblank)
	st2 #0
	
	lda #%00000101
	sta $1402	;Interrupt Disable	-----T12 (1=disabled 0=Enabled)	
				; T=Timer interrupt request, 1= IRQ1 (Vblank) 2=IRQ2
	
	
	
	
	
	
	
	
	loadpair z_hl,TilePatterns
	loadpair z_de,TileCache
	jsr cls
	

	loadpair z_ix,ChibikoDef
	jsr DrawSpriteAlways	;Draw Player Sprite


	
	
;                                      

;                                      	ld bc,&6060
	loadpair z_bc,$6060
infloop:
	cli
	;sed
	pushpairsafe z_bc
	
		;jsr ChibiTracks_Play
	
		jsr readjoystick
	pullpairsafe z_bc
	;sei
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
		loadpair z_hl,TilePatterns
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
	st0 #0				;Select Memory Write Reg
	lda z_L
	sta $0102 			;st1 - L address
	lda z_H
	sta $0103 			;st2 - H Address
	st0 #2					;Select Data reg
	ldx #0
NextTile:
DrawTile:
		lda (z_bcs,x)	;BC=Tilemap data
		beq CustomTileDone2	;EmptyTile
		sta z_hs
		
		lda TileClear

		beq NoClear
		txa
		sta (z_bcs,x)
NoClear:
		txa	;=0
		tay	;=0

DrawTileMore:	
		lda z_hs
		clc
		adc z_es
		sta $0102		;Send to Data-L
		
		lda z_ds
		adc #0
		;lda #1
		sta $0103		;Send to Data-H	
	
CustomTileDone:
		lda z_l
		clc
		adc #1
		sta z_l
		bcc CustomTileDoneB
		inc z_h
CustomTileDoneB:
		INC z_Cs
		BNE	CustomTileDoneC
		INC	z_Bs
CustomTileDoneC:

		dec z_iyl
		beq TileDone2
			jmp	NextTile
TileDone2:
	rts
CustomTileDone2:
		lda z_l
		clc
		adc #1
		sta z_l
		bcc CustomTileDoneD
		inc z_h
CustomTileDoneD:

		st0 #0				;Select Memory Write Reg
		lda z_L
		sta $0102 			;st1 - L address
		lda z_H
		sta $0103 			;st2 - H Address
		st0 #2					;Select Data reg
		ldx #0
	jmp CustomTileDoneB
	
	
	
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
		beq CustomTileDone2Rev	;EmptyTile
		sta z_hs
		
		lda TileClear

		beq NoClearRev
		txa
		sta (z_bcs,x)
NoClearRev:
		txa	;=0
		tay	;=0

		lda z_hs
		clc
		adc z_es
		sta $0102		;Send to Data-L
		
		lda z_ds
		adc #4
		;lda #1
		sta $0103		;Send to Data-H	
	
CustomTileDoneRev:
		lda z_l
		clc
		adc #1
		sta z_l
		bcc CustomTileDoneBRev
		inc z_h
CustomTileDoneBRev:
		lda z_Bs
		BNE	CustomTileDoneCRev
		DEC z_Bs
CustomTileDoneCRev:		
		DEC	z_Cs


		dec z_iyl
		beq TileDone2Rev
			jmp	NextTileRev
TileDone2Rev:
	rts
CustomTileDone2Rev:
		lda z_l
		clc
		adc #1
		sta z_l
		bcc CustomTileDoneDRev
		inc z_h
CustomTileDoneDRev:

		st0 #0				;Select Memory Write Reg
		lda z_L
		sta $0102 			;st1 - L address
		lda z_H
		sta $0103 			;st2 - H Address
		st0 #2					;Select Data reg
		ldx #0
	jmp CustomTileDoneBRev
	


	
	
	
	
PrintChar:
	rts	


  
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
		

		lda z_b
		lsr
		lsr
		sta z_b

		st0 #0			;Select Vram Write
		lda z_c
		clc
		adc #8
		sta z_c		
		
		and #%00011100	;Multiply Ypos by 32 - low byte
		asl
		asl
		asl
		clc
		adc z_b			;Add Xpos
		sta z_l		;Send to Data-L
		
		lda z_c
		and #%11100000	;Multiply Ypos by 32 
		lsr
		lsr
		lsr
		lsr
		lsr
		sta z_h		;Send to Data-H
		
	rts

	

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



prepareVram:		;z_HL=VRAM address to select

	st0 #0				;Select Memory Write Reg
	lda z_e
	sta $0102 			;st1 - L address
	lda z_d
	sta $0103 			;st2 - H Address
	rts

	;BC=Bytes
	;DE=Destination Ram
	;HL=Source Bytes
		
DefineTiles:							
	jsr prepareVram			;Select Ram address
	st0 #2					;Select Data reg
	ldx z_C					;B=High byte of count - X=Low byte
	ldy #0	
DefineTilesAgain:
	ldx #8
DefineTilesAgainD:
		lda (z_HL),Y		;Bitplane 0
		sta $0102			;Store Low byte
		iny
		lda (z_HL),Y		;Bitplane 1
		sta $0103			;Store High Byte
		iny
	dex
	bne DefineTilesAgainD
	ldx #8
DefineTilesAgain2:
	
		lda z_ixh	
		sta $0102
		lda z_ixl	
		sta $0103
	dex
	bne DefineTilesAgain2
		tya
		bne DefineTilesAgainYok
		inc z_h				;INC High byte Y=low byte
DefineTilesAgainYok:		
		lda z_C				;Is Low Byte Zero?
		bne DefineTilesDecBC_C
		lda z_B				;Are We done
		beq DefineTilesAgainDone
		DEC z_B				;DEC high byte (X is low byte)
DefineTilesDecBC_C:	
		lda z_C
		sec
		sbc #8
		sta z_C
		jmp DefineTilesAgain
DefineTilesAgainDone:
	rts
	
	
	
DefineTilesDuo:	
	pushpair z_de
	pushpair z_hl
	pushpair z_bc
		jsr DefineTiles ;jsr DefineTiles		;Define the tile patterns
	pullpair z_bc
	pullpair z_hl
	pullpair z_de

	AddPair z_de,($400*16)
	
DefineTilesReversed:
	jsr prepareVram			;Select Ram address
	st0 #2					;Select Data reg
	ldx z_C					;B=High byte of count - X=Low byte
	ldy #0	
DefineTilesAgainRev:
	ldx #8
DefineTilesAgainDRev:
		lda (z_HL),Y		;Bitplane 0
		jsr DoFlipByte
		sta $0102			;Store Low byte
		iny
		lda (z_HL),Y		;Bitplane 1
		jsr DoFlipByte
		sta $0103			;Store High Byte
		iny
	dex
	bne DefineTilesAgainDRev
	ldx #8
DefineTilesAgain2Rev:
	
		lda z_ixh	
		sta $0102
		lda z_ixl	
		sta $0103
	dex
	bne DefineTilesAgain2Rev
		tya
		bne DefineTilesAgainYokRev
		inc z_h				;INC High byte Y=low byte
DefineTilesAgainYokRev:		
		lda z_C				;Is Low Byte Zero?
		bne DefineTilesDecBC_CRev
		lda z_B				;Are We done
		beq DefineTilesAgainDoneRev
		DEC z_B				;DEC high byte (X is low byte)
DefineTilesDecBC_CRev:	
		lda z_C
		sec
		sbc #8
		sta z_C
		jmp DefineTilesAgainRev
DefineTilesAgainDoneRev:
	rts
	
DoFlipByte:
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
	rol
	ror z_d
	
	lda z_d
	rts	
	
	org $5ffe	;Reset Vector ($001FFE Physical addr)
	dw $E000			

	
	
	include "/srcALL/V1_MinimalTile.asm"
	include "\SrcAll\BasicFunctions.asm"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


xChibicloneDef:
	dw Sprite_1		;Tilemap
	dw ChibikoPatterns		;Pattern Data
	db 20,32		;Width,Height
	db 64,128		;X,Y
	db 1,1			;RefreshTile,Sprite
	db 64,128		;X,Y
	db 0,0			;Flags
xChibikoDef:
	dw Yarita_1		;Tilemap
	dw YaritaPatterns			;Pattern Data
	db 20,32		;Width,Height
	db 96,96		;X,Y
	db 1,1			;RefreshTile,Sprite
	db 64,128		;X,Y
	db 1,1			;Flags

	
	
Sprite_1:
  db 1,2,3,4,5
  db 6,7,8,9,10
  db 11,12,13,14,15
  db 16,17,18,19,20
  db 26,27,28,29,30
  db 21,22,23,24,25
  db 0,31,32,33,0
  db 0,34,35,36,0


Yarita_1:
  db 0,1,2,3,0
  db 4,5,6,7,8
  db 9,10,11,12,0
  db 13,14,15,16,17
  db 18,19,20,21,22
  db 0,23,24,25,0
  db 0,26,27,28,0
  db 0,29,30,31,0
	
bmpTiles:
	
	incbin "\ResAll\ChibiFighter\GBC_Back1.RAW"
bmpTiles_End:

bmpChibiko:
	incbin "\ResALL\ChibiFighter\GBC_Chibiko.RAW"
bmpChibiko_End:

bmpYarita:
	incbin "\ResALL\ChibiFighter\GBC_Yarita.RAW"
bmpYarita_End:	

palette:
    dw %0000000000000000; ;0  %-------GGGRRRBBB
    dw %0000000000100111; ;1  %-------GGGRRRBBB
    dw %0000000111000111; ;2  %-------GGGRRRBBB
    dw %0000000111111111; ;3  %-------GGGRRRBBB
	
    dw %0000000000000000; ;4  %-------GGGRRRBBB
    dw %0000000000011011; ;5  %-------GGGRRRBBB
    dw %0000000111000111; ;6  %-------GGGRRRBBB
    dw %0000000111111111; ;7  %-------GGGRRRBBB
	
    dw %0000000000000000; ;8  %-------GGGRRRBBB
    dw %0000000001111111; ;9  %-------GGGRRRBBB
    dw %0000000111001111; ;10  %-------GGGRRRBBB
    dw %0000000111111011; ;11  %-------GGGRRRBBB
	
    dw %0000000000000000; ;12  %-------GGGRRRBBB
    dw %0000000001000000; ;13  %-------GGGRRRBBB
    dw %0000000011000000; ;14  %-------GGGRRRBBB
    dw %0000000111000000; ;15  %-------GGGRRRBBB

InterruptHandler:
	pha
		lda $1403	;Interrupt request 	-----T12
					   ; T=Timer interrupt request, 
		and #%00000101 ; 1= IRQ1, 2=IRQ2 (1=occurred)
		bne InterruptHandlerNotVblank
		
		lda $0100	;Read VDC status to clear interrupt
		phy
		phx
			 pushpair z_hl
			pushpair z_de
			pushpair z_bc
			pushpair z_ix
				jsr ChibiTracks_Play	;Update music
			pullpair z_ix
			pullpair z_bc
			pullpair z_de
			pullpair z_hl
		plx	
		ply
InterruptHandlerNotVblank:
		lda #255
		sta $1403	;Acknowledge interrupt
	pla
InterruptHandlerDummy:
	rti
	
	
	include "\SrcALL\Multiplatform_ChibiSoundPro.asm"
	include "\SrcALL\Multiplatform_ChibiTracks_Tweener.asm"
	include "\SrcALL\Multiplatform_Fraction16.asm"
	include "\SrcAll\Multiplatform_ChibiTracks.asm"
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
Song1:
	;include "\ResALL\ChibiSoundPro\CBT1.asm"
	;include "\ResALL\ChibiSoundPro\CBT2.asm"
	;incbin  "\ResALL\ChibiSoundPro\song.cbt"
	;incbin  "\ResALL\ChibiSoundPro\song2.cbt"
	incbin  "\ResALL\ChibiSoundPro\ChibiAkumasTheme.cbt"
	
	
	org $fff6
	dw InterruptHandler		;$FFF6-7 IRQ2 / BRK
	dw InterruptHandler		;$FFF8-9 IRQ1 (Vblank)
	dw InterruptHandler		;$FFFA-B Timer
	dw InterruptHandler		;$FFFC-D NMI
	dw InterruptHandler		;$FFFE-F unused ... 001FFE/F RESET
	
	
	