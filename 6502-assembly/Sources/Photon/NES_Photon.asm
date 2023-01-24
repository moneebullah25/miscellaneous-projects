;fake 25x20 pixel screen
	include "\SrcAll\BasicMacros.asm"

sppage equ $0100

z_Regs 		equ $40

	org $BFF0

	db "NES",$1a		;ID
	db $01				;Rom pages (16k each)
	db $0				;CHR-ROM pages
	db %01000010		;mmmmFTBM		mmmm = mapper no bottom 4 bits , Four screen vram layout, Trainer at &7000, Battery ram at &6000, Mirror (0=horiz, 1=vert)
	db %00000000		;mmmm--PV 		mapper (top 4 bits...  Pc10 arcade, Vs unisystem )
	db 0				;Ram pages
	db 0,0,0,0,0,0,0
						;We selected Mapper 4 - it has 8k VRAM , 8K Sram and 128k rom
	
vblanked 	equ $7F		;Zero page address of Vblank count

UserRam equ $200		;Game Vars

ScreenWidth20 equ 1		;Screen size=25x10 tiles
ScreenWidth equ 200		
ScreenHeight equ 160	;First 80 lines cached at $6000, rest at $7000
						;10x25=250 tiles of each map used.
Color4 equ 1
Color3 equ 3
Color2 equ 2
Color1 equ 1

RamAddrL	equ $82		;Address of Cache to copy - only 8 tiles 
RamAddrH	equ $83			;copied per NMI

;Three tile patterns that are updated every frame 

FastRefresh1_L equ $90
FastRefresh1_H equ $91

FastRefresh2_L equ $92
FastRefresh2_H equ $93

FastRefresh3_L equ $94
FastRefresh3_H equ $95

ProgramStart:
	sei				;Interrupts off
	cld				;Clear Decimal flag
	
	ldx #$ff		;Set up stack
	txs
	
	lda #%10000000	;Turn on extra ram at $6000-%7FFF 
	sta $A001			;(used for VRAM cache)
	
	lda #%11000000	;Sound IRQ off
	sta $4017
	
;Turn ON the screen
;(Sprite enable/back enable/Sprite leftstrip / backleftstrip)
	lda #%00011110 	
	sta $2001		;PPUMASK
	
;Clear vars used by Vblank tile copy	
	lda #$00
	sta RamAddrL		;Reset copy ranges.
	sta FastRefresh1_L
	sta FastRefresh1_H
	sta FastRefresh2_L
	sta FastRefresh2_H
	sta FastRefresh3_L
	sta FastRefresh3_H
	lda #$60
	sta RamAddrH		;Base of cache
	
	
		
	;lda #160-8			;Could try to update boost with extra tile
	;sta z_c
	;lda #0
	;sta z_b
	;jsr CalcVramAddr
	
	;lda z_l
	;and #%11110000
	;sta FastRefresh1_L
	;lda z_h
	;and #%00011111
	;sta FastRefresh1_H
	
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
;Fill unused areas with spare tile (255)
	lda #$20
	sta $2006
	lda #$0
	sta $2006				;Select start of Tilemap ($2000+)
	
	ldx #4
	ldy #$C0				;Fill $3C0
TileFill:
	lda #255
	sta $2007				;Fill Tilemap with tile 255 (Blank)
	dey 
	bne TileFill
	dex
	bne TileFill
	
;Set the Tilemaps (0-250) (First 80 lines)
	lda #4					;Start SX
	sta z_b
	lda #4+1				;Start SY
	sta z_c
	ldx #25					;Width in tiles
	ldy #10					;Height in tiles
	
	lda #0					;TileStart
	jsr FillAreaWithTiles	;Draw the tiles to screen
	
;Set the Tilemaps (256-506) (2nd 80 lines)
	lda #4					;Start SX
	sta z_b
	lda #10+4+1				;Start SY
	sta z_c
	
	ldx #25					;Width in tiles
	ldy #10					;Height in tiles
	
	lda #0					;TileStart
	jsr FillAreaWithTiles	;Draw the tiles to screen

	
;Palette
	lda #$3F		;Select Palette ram &3F00
	sta $2006		;PPUADDR H
	lda #0
	sta $2006		;PPUADDR L
	
	ldx #4
PaletteAgain
	lda Palette-1,x 
	sta $2007		;PPUDATA
	dex 
	bne PaletteAgain
	
		
	
	lda #0					;Init the cyclic copy.
	sta RamAddrL
	lda #$60
	sta RamAddrH
	
	jsr NesEnableScreen
	lda #$80+$20			;NMI enable (Vblank)
	sta $2000				;PPUCTRL - VPHB SINN
	cli         			;Enable IRQs.

	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	loadpair z_hl,userram
	loadpair z_bc,256
	jsr CLDIR0
		
	jsr MainMenu
		
	

infloop:
	ldx #0					;Delay to allow level to redraw
	ldy #0
RedrawWait:	
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	dex
	bne RedrawWait
	dey
	bne RedrawWait
		
infloopB:				;main loop
	lda tick
	clc
	adc #1
	and #%00000001		;Update Game Tick
	sta tick

	loadpair z_bc,200	;slow down delay

	lda boost

	bne boostoff
	loadpair z_bc,1		;boost - no delay (compensate for font draw)

boostoff:
	lda #0
	sta z_d					;key buffer
	
pausebc:
	pushpair z_bc
		pushpair z_de
			jsr Player_ReadControlsDual	
		pullpair z_de

		lda z_h
		cmp #0				;Key Pressed?
		beq pausenokey

		sta z_d
		jmp keysdown		;store any pressed joystick buttons
pausenokey:
		lda #0
		sta keytimeout		;released - nuke key, and relese keypress
	
		lda #0
		sta z_d
keysdown:
	pullpair z_bc
	jsr decbc

	lda z_b
	ora z_c
	bne pausebc

startdraw:
	lda keytimeout			;See if keytimeout is set
	bne joyskip
	
	lda #1					;Turn off boost
	sta boost
	
processkeys:
	loadpair z_ix,playerxacc ;point ix to player accelerations 

	lda z_d
	and #%01000000			;RLDUSsBA - L
	beq joynotleft
	
	dec playerdirection
	jsr setplayerdirection
	lda #1					;ignore keypresses		
	sta keytimeout

joynotleft:
	lda z_d
	and #%10000000			;RLDUSsBA - R
	beq joynotright
	
	inc playerdirection
	jsr setplayerdirection
	lda #1					;ignore keypresses		
	sta keytimeout

joynotright:
	lda z_d
	and #%00000010			;RLDUSsBA - Fire
	beq joynotfire

	lda boostpower			;check if boost power remains
	beq joynotfire

	lda #0
	sta boost
joynotfire:
joyskip:
	jsr handleplayer			;draw and update player
	jsr handlecpu				;draw and update cpu
	
	lda playery				;Use Fastrefresh tile to update Player pos
	sta z_c
	lda playerx
	sta z_b
	jsr CalcVramAddr
	lda z_l
	and #%11110000
	sta FastRefresh1_L
	lda z_h
	and #%00011111
	sta FastRefresh1_H
		
	lda cpuy					;Use Fastrefresh tile to update CPU pos
	sta z_c
	lda cpux
	sta z_b
	jsr CalcVramAddr
	lda z_l
	and #%11110000
	sta FastRefresh2_L			
	lda z_h
	and #%00011111
	sta FastRefresh2_H
	
	jmp infloopB				;Repeat main loop

	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

cls:
	lda #$60					;Clear $6000+
	sta z_h
	ldy #0
	sty z_l
	
	ldx #$20					;Clear $2000 bytes
FillAgain:
	lda #0
	sta (z_hl),y
	iny
	bne FillAgain
	inc z_h
	dex 
	bne FillAgain	
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
waitforfire:
	jsr dorandom				;reseed random numbers
	jsr Player_ReadControlsDual	;RLDUSsBA 
	and #%00000001
	bne waitforfire

waitforfireb:
	jsr dorandom				;reseed random numbers
	jsr Player_ReadControlsDual	;RLDUSsBA 
	and #%00000001
	beq waitforfireb
	rts
Player_ReadControlsDual:
	txa
	pha
		;Strobe joysticks to reset them
		ldx #$01		;Send a 1 to joysticks (strobe reset)
		stx $4016		;JOYPAD1 port
		
		dex 			;Send a 0 to joysticks (read data)
		stx $4016		;JOYPAD1 port

		ldx #8			;Read in 8 bits from each joystick
Player_ReadControlsDualloop:
		lda $4016		;JOYPAD1
		lsr 	   		; bit0 -> Carry
		ror z_h  		;Add carry to Joy1 data
		dex 
		bne Player_ReadControlsDualloop
	pla 
	tax
	lda z_h
	rts
  
	
; $4016/$4017 - 1=Pressed / 0=NotPressed

; Read  1 - A
; Read  2 - B
; Read  3 - Select
; Read  4 - Start
; Read  5 - Up
; Read  6 - Down
; Read  7 - Left
; Read  8 - Right

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	
Palette:  
; 	Color   3   2   1  0
		db $2A,$15,$21,$0D

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	
pset:			;x=ab y=c color=d
	lda z_b
	cmp #250
	bcc PsetOK
	rts
PsetOK:
	pushpair z_bc
		loadpair z_hl,PixelMask
		
		lda z_b
		and #%00000111			;Calc pixel mask
		tay
		lda (z_hl),y
		pha
			jsr CalcVramAddr	;Calc Cache pos in z_HL
		pla
		sta z_b					;Store Pixel mask
		eor #255
		sta z_c					;Store background mask
		ldy #0
		lda (z_hl),y			;Get current byte
		and z_c					;Keep background
		ror z_d
		bcc PsetSkip1
		ora z_b					;Or in pixel
PsetSkip1:
		sta (z_hl),y			;Update pixel
		
		ldy #8					;Move to second bitplane
		
		lda (z_hl),y			;Get current byte
		and z_c					;Keep background
		ror z_d
		bcc PsetSkip2
		ora z_b					;Or in pixel
PsetSkip2:
		sta (z_hl),y			;Update pixel
	pullpair z_bc
	rts
	
	align 4
PixelMask:
	db %10000000,%01000000,%00100000,%00010000
	db %00001000,%00000100,%00000010,%00000001

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	

point:			;x=ab y=c 
	pushpair z_bc
		loadpair z_hl,PixelMask
		
		lda z_b
		and #%00000111			;Calc pixel mask
		tay
		lda (z_hl),y
		pha
			jsr CalcVramAddr	;Calc Cache pos in z_HL
		pla
		sta z_b
		lda #0
		sta z_as				;Buildup for resulting pixel
		ldy #8
		lda (z_hl),y
		and z_b					;Get pixel from screen cache
		beq PointSkip1
		sec
		rol z_as				;Set bitplane 0
PointSkip1:
		ldy #0
		lda (z_hl),y
		and z_b					;Get pixel from screen
		beq PointSkip2
		sec
		rol z_as				;Set bitplane 1
PointSkip2:
	pullpair z_bc
	lda z_as
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
CalcVramAddr:
	pushpair z_de
		loadpair z_hl,$6000		;Using Ram area $6000+ as a cache
		lda z_c
		cmp #80					;lines >80 are in second pattern bank
		bcc ScreenFirstHalf
		sub #80
		sta z_c
		lda #$70				;Second Bank at $7000
		sta z_h
ScreenFirstHalf:	
		loadpair z_de,$0000
		
		lda z_b
		and #%11111000	;8 lines - 2 bitplanes per line
		asl
		rol z_d
		sta z_e
		jsr addhl_de
		
		lda #0
		sta z_d
		
		lda z_c
		and #%00000111	;Line
		sta z_e
		jsr addhl_de
		
		lda z_c
		and #%11111000	;*25*2 (25=1+8+16)
		asl
		rol z_d
		sta z_e
		jsr addhl_de	;*2
		
		asl z_e
		rol z_d
		asl z_e
		rol z_d
		asl z_e
		rol z_d
		jsr addhl_de	;*8
		asl z_e
		rol z_d
		jsr addhl_de	;*16
	pullpair z_de
	rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	

GetVDPScreenPos:			;BC=XYpos	
		lda z_c
		;and #%00000111		;Ypos * 32 tiles per line
		asl
		asl
		asl
		asl
		asl
		ora z_b				;Add Xpos
		sta z_l				;Store in L byte
		lda z_c
		and #%11111000		;Other bits of Ypos for H byte
		lsr
		lsr
		lsr
		clc
		adc #$20			;$2000 ofset for base of tilemap
		
		
		sta $2006			;PPUADDR
		lda z_L
		sta $2006			;PPUADDR
	rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	

		
FillAreaWithTiles:
	sta z_d					;Backup tilenum
FillAreaWithTiles_Yagain:
	txa
	pha
		jsr GetVDPScreenPos	;Calculate Tilemap mempos
	
		lda z_d
FillAreaWithTiles_Xagain:
		sta $2007			;PPUDATA - Save Tile selection to Vram
		clc
		adc #1				;Move to next tile
		dex 
		bne FillAreaWithTiles_Xagain
		sta z_d
		inc z_c				;INC Ypos
	pla
	tax
	dey
	bne FillAreaWithTiles_Yagain
	rts					
	;Need to reset scroll after writing to VRAM

	
prepareVram:	;Select a destination address
	lda z_d				;MSB - DEST ADDR
	sta $2006			;PPUADDR
	lda z_e				;LSB - Dest ADDR
	sta $2006			;PPUADDR
	rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	



		
NesEnableScreen:		;Turn ON the screen
;(Sprite enable/back enable/Sprite leftstrip / backleftstrip)
	lda #%00011110 	
	sta $2001			;PPUMASK
	
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


	include "\SrcAll\MultiPlatform_MultDiv.asm"
	include "\SrcAll\BasicFunctions.asm"
	
	include "PH_Title.asm"
	include "PH_RamDefs.asm"
	include "PH_DataDefs.asm"
	include "PH_Multiplatform.asm"
	include "\ResAll\Vector\VectorFont.asm"
	include "\srcALL\MultiPlatform_ShowDecimal.asm"
	include "PH_Vector.asm"
	
	
	include "\SrcAll\monitor.asm"			;Debugging tools	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
nmihandler:		;This procuedure runs after each frame
	pha			
	tya
	pha
	txa
	pha
		inc vblanked		;Alter Vblank Zero page entry
		
		lda RamAddrL		;Back up source address
		pha
			lda RamAddrH
			pha
;3 Fast Refresh tiles
				lda FastRefresh1_H	
				ldx FastRefresh1_L
				jsr SetAndSendOneTile
				
				lda FastRefresh2_H	
				ldx FastRefresh2_L
				jsr SetAndSendOneTile
				
				lda FastRefresh3_H	
				ldx FastRefresh3_L
				jsr SetAndSendOneTile
				
			pla
			sta RamAddrH
			and #$1F
			sta $2006	;PPUADDR 	Destination address - H
		pla	
		sta $2006	
		sta RamAddrL	
		
;8 Sequential tiles	
		ldy #0
		ldx #8			;Transfer 8 tiles from chache to screen
CopyAgain		
		jsr SendOneTile
		 
		dex
		bne CopyAgain	;Do Next Tile
		 
		tya
		clc
		adc RamAddrL	;Update address to continue next time
		sta RamAddrL
		 
		lda RamAddrH
		adc #0
		and #$1F		;$2000 per pattern bank
		ora #$60		;Cache starts at $6000
		sta RamAddrH

;Reset Scroll
		lda #0+4		;Scroll X
		sta $2005
		sta $2005		;Scroll y
		
;We need to switch in other tileset for lower screen area.
		
		lda #$80+$08		;Page in Tiles  0-255 ($0000+)
;Sprite address must be different to tile address or this will fail
		sta $2000		;PPUCTRL - VPHB SINN
				
		lda #116		;Screen middle
		sta $C000   	;Set Line interrupt (Mapper 3 function)	
		sta $C001   	;Reset counter (Mapper 3 function)	
		sta $E001   	;Enable IRQ (Mapper 3 function)		
	pla
	tax
	pla
	tay
	pla
	rti
		
irqhandler:				;Occurs at screen middle 
	pha
		lda #$80+$10	;Page in Tiles 256-511 ($1000+)
		
		sta $2000		;PPUCTRL - VPHB SINN
		sta $E000  		;Disable line interrupt
	pla
	rti

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	
;Transfer Tile with VRAM address $AAXX	from cache
SetAndSendOneTile:		
	sta $2006			;VRAM destination 
	ora #$60			
	sta RamAddrH		;Ram address of Cache ($6000+)
			
	stx $2006		
	stx RamAddrL
	ldy #0
SendOneTile:
	lda (RamAddrL),y
	sta $2007
	iny
	lda (RamAddrL),y
	sta $2007
	iny
	lda (RamAddrL),y
	sta $2007
	iny
	lda (RamAddrL),y
	sta $2007
	iny
	lda (RamAddrL),y
	sta $2007
	iny
	lda (RamAddrL),y
	sta $2007
	iny
	lda (RamAddrL),y
	sta $2007
	iny
	lda (RamAddrL),y
	sta $2007
	iny
	lda (RamAddrL),y
	sta $2007
	iny
	lda (RamAddrL),y
	sta $2007
	iny
	lda (RamAddrL),y
	sta $2007
	iny
	lda (RamAddrL),y
	sta $2007
	iny
	lda (RamAddrL),y
	sta $2007
	iny
	lda (RamAddrL),y
	sta $2007
	iny
	lda (RamAddrL),y
	sta $2007
	iny
	lda (RamAddrL),y
	sta $2007
	iny
	rts	
	
	
	
;Cartridge Footer	
	org $FFFA
	dw nmihandler			;FFFA - Interrupt handler (VBLANK)
	dw ProgramStart			;FFFC - Entry point
	dw irqhandler			;FFFE - IRQ Handler (Midscreen redraw)
	
	
	
	
	
	
	
	
	