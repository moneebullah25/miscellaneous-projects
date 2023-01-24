SpPage 		equ $0100	
z_Regs 		equ $60

	include "\SrcALL\BasicMacros.asm"
	
	
Cursor_X 	equ $40
Cursor_Y 	equ Cursor_X+1


LightGunY 	equ Cursor_X+2		;Used by scan version
LightGunX 	equ Cursor_X+3

VDP_CT equ z_Regs+18	;Position of next Tile in buffer
VDPBuffer equ $200		;Tilemap Buffer
SpriteBuffer equ VDPBuffer+$100	;SpriteBuffer


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

ProgramStart:
	sei					;Interrupts off
	cld					;Clear Decimal flag
	
	ldx #$ff			;Set up stack
	txs
	
;Palette
	lda #$3F		;Select Palette ram &3F00
	sta $2006		;PPUADDR H
	lda #0
	sta $2006		;PPUADDR L
	

;8 Palettes (4 tile / 4 sprite) + 1 back
	ldx #4*8		
PaletteAgain
	lda Palette-1,x 
	sta $2007		;PPUDATA
	dex 
	bne PaletteAgain

	
	lda #%10000000	;Turn on extra ram at $6000-%7FFF
	sta $A001
	
	

	lda #0
	sta Cursor_X		;For Text routines
	sta Cursor_Y
	
	lda #0
	sta LightGunY		;Make sure interrupt doesn't fire
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	
;Clear Sprite Buffer
	jsr RemoveGunTarget	
	
	
;Define font and (all important) Chibiko!	
	lda #<Bitmap			;Source Bitmap Data
	sta z_L
	lda #>Bitmap
	sta z_H

	lda #<(BitmapEnd-Bitmap);Source Bitmap Data Length
	sta z_C
	lda #>(BitmapEnd-Bitmap)
	sta z_B
	
	lda #<$0000				;Tile 128 (16 bytes per tile)
	sta z_E
	lda #>$0000
	sta z_D
	
	jsr DefineTiles			;Define the tile patterns
	

	jsr NesEnableScreen		;Turn on screen
	
	lda #%11000000			;Sound IRQ off
	sta $4017
	cli         			;Enable IRQs.
	
	
		;Load in the address of the Message into the zero page
	lda #>strConnectLightGun
	sta z_h			;H Byte
	lda #<strConnectLightGun
	sta z_l				;L Byte
	jsr PrintStr		;Show to the screen
	
NoLightGun:
	lda $4017			;Test Joy2
	cmp #$48			;Gun - No Fire or light
	bne NoLightGun
	
			
	;jmp ShowStats_Start		;See what the Lightgun's doing!
	;jmp ChibiShooter_Start	;Shoot one sprite
	jmp LightGunScan_Start	;Full screen scan for an X,Y pos
	
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;		Show contents of lightgun register
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		
ShowStats_Start:	

;Draw a white block
	lda #16				;Start SX
	sta z_b
	lda #10				;Start SY
	sta z_c
	ldx #6				;Width in tiles
	ldy #6				;Height in tiles
	lda #0
	sta z_d				;Increment
	lda #96				;TileStart
	jsr FillAreaWithTiles	;Draw the tiles to screen
		
ShowStats:	
	lda #0
	sta Cursor_X		;Zero Text cursor pos
	sta Cursor_Y
	
WaitForChange:	
	;lda $4016			;Read in the lightgun (Port 1)
	lda $4017			;Read in the lightgun (Port 2)
	cmp z_h
	beq WaitForChange	;and wait for change
	
	pha
		jsr monitor		;Dump Registers to screen
		jsr waitframe
	pla
	sta z_h

	jmp ShowStats		;Infinite loop

	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;		White out sprite to see if the gun is pointed at it
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	
ChibiShooter_Start:			
	Jsr DrawChibiko 		;Show the Chibiko

;Wait for fire
ChibiShooter:
	lda $4017
	and #%00010000
	beq ChibiShooter		;Fire Not Pressed

; Fire pressed - Check gun is pointing at screen
	jsr BlackOutChibiko		;Whiteout the Chibiko
	
	jsr TestLightGun
		; ---FL---			;F=Fire (1=Yes) L=Light (0=Yes)
	and #%00001000			;is the lightgun seeing Light
	beq ShowCheat			;Screen is black but light seen
							;player is pointing at a lightbulb!
	
;Check if player shot chibiko
	jsr WhiteOutChibiko		;Whiteout the Chibiko
	
	jsr TestLightGun
		; ---FL---			;F=Fire (1=Yes) L=Light (0=Yes)
	and #%00001000
	bne ChibiShooter_Start	;Player missed boo!
	
;Load in the address of the Message into the zero page
	lda #>strHit
	sta z_h				;H Byte
	lda #<strHit
	sta z_l				;L Byte
	
	jsr PrintStr			;Show to the screen
	jmp ChibiShooter_Start
	
ShowCheat:
;Load in the address of the Message into the zero page
	lda #>strCheat
	sta z_h				;H Byte
	lda #<strCheat
	sta z_l				;L Byte
	
	jsr PrintStr			;Show to the screen
	jmp ChibiShooter_Start
	
	
;Wait for a full screen scan, and get the MINIMUM lightgun state
TestLightGun:
	jsr waitframe			;Wait for Vblank
TestLightGunQuick:
		ldx #$00
		stx vblanked		;Zero Vblanked
		lda #255
TestLightGunWait:
		and $4017			;Get lowest lightgun value
		ldx vblanked		;Wait for the interrupt to change it
		beq TestLightGunWait
	rts

	
DrawChibiko:
	lda #8				;Start SX
	sta z_b
	lda #10				;Start SY
	sta z_c
	ldx #6				;Width in tiles
	ldy #6				;Height in tiles
	lda #1
	sta z_d				;Increment
	lda #97				;TileStart
	jmp FillAreaWithTiles	;Draw the tiles to screen
	
	
BlackOutChibiko:	
	lda #0				;TileStart
	jmp BlackOutChibikoB
WhiteOutChibiko:
	lda #96				;TileStart
BlackOutChibikoB:
	pha
		lda #8				;Start SX
		sta z_b
		lda #10				;Start SY
		sta z_c
		ldx #6				;Width in tiles
		ldy #6				;Height in tiles
		lda #0
		sta z_d				;Increment
	pla
	jmp FillAreaWithTiles	;Draw the tiles to screen	
	
strHit:				;255 terminated string
	db "Hit     ",255	
strCheat:				;255 terminated string
	db "Cheat   ",255	
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;		Scan the screen for approximate X,Y gun pos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
LightGunScan_Start:

TestScan:
	lda $4017
	and #%00010000		;Check if fire was pressed
	beq TestScan

;Start the Y-Scan
	lda #255			;Tell the NMI to do a Y-Scan
	sta LightGunY
TestScanWait:
	cmp LightGunY
	beq TestScanWait	;Wait until NMI finds LightGun Y
	

;Restore the proper palette!
	jsr waitframe
	
	lda #$3F		;Select Palette ram &3F00
	sta $2006		;PPUADDR H
	lda #0
	sta $2006		;PPUADDR L
	ldx #4*4
PaletteAgain3
	lda Palette-1,x 
	sta $2007		;PPUDATA
	dex 
	bne PaletteAgain3
	
;See if we got a Ypos 
	lda LightGunY
	cmp #40				;Ypos not found/valid
	bcs TestScan		
	
;Turn off background - We'll scan with sprites.
	lda #%00010110			;Turn off background
	sta $2001
	
	lda #0
	sta LightGunX			;Starting scan pos
		
ContinueXScan:	
	jsr ShowGunTarget		;Draw a white block at the test area
	
	jsr TestLightGunQuick	;Check Gun for this frame  
	and #%00001000				;(Quick=Start now, No Vblank wait)
	beq LightGunXFound		;Gun saw the light!
		
	lda LightGunX			;Move across one unit.
	clc
	adc #4
	sta LightGunX
	cmp #34					;Have we gone offscreen?
	bcc ContinueXScan
	
;Scan was always one frame behind, so decrease found pos by 1/2 block
;This is because next vblank occurs at end of TestLightGunQuick	
LightGunXFound:	
	;lda #%00001110			;Turn off sprites
	;sta $2001
	jsr RemoveGunTarget		;Clear sprites
	jsr NesEnableScreen		;Restore background
		
	dec LightGunX			;Xpos -2
	dec LightGunX		
		
		
	ldy LightGunY
	ldx LightGunX
	
	lda #0
	sta Cursor_X			;For monitor
	sta Cursor_Y
	
	jsr Monitor
TestScanInfLoop:	
	jmp TestScan			;Wait for next fire.
	
		
		
ShowGunTarget:				;Fill a 4x3 area with white!

	lda #3					;Height (3 sprites)
	sta z_b

	lda LightGunY			;Ypos * 8
	asl
	asl
	asl
	sta z_h
	ldy #0
	
ShowGunTarget_NextY:		;Xpos * 8	
	lda LightGunX
	asl
	asl
	asl
	sta z_l
	
	ldx #4					;Width (4 sprites)
	
ShowGunTarget_NextX:		
	lda z_h					;Ypos
	sta SpriteBuffer,y
	iny
	lda #96					;Sprite
	sta SpriteBuffer,y
	iny
	lda #0					;Attrib
	sta SpriteBuffer,y
	iny
	lda z_l					;Xpos
	clc
	adc #1
	sta SpriteBuffer,y
	iny

	lda z_l
	clc
	adc #8					;Move across
	sta z_l
	dex
	bne ShowGunTarget_NextX
	
	lda z_h
	clc
	adc #8					;Move Down
	sta z_h
	
	dec z_b
	bne ShowGunTarget_NextY
	
	rts



RemoveGunTarget:			;Clear all the sprites offscreen
	lda #>SpriteBuffer		;Start
	sta z_h
	lda #<SpriteBuffer		
	sta z_l
	lda #>$100				;Bytes
	sta z_b
	lda #<$100				
	sta z_c
	lda #255
	jmp cldir				;Zero Range
	
	
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;		NMI Interrupt Handler 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Nmihandler:						;Vblank Interrupt handler
	php
	pha
	tya
	pha
		lda LightGunY 
		cmp #255
		beq NMI_LightGun		;Branch to our Lightgun scan
		
		
	
		lda #SpriteBuffer/256	;Data to copy to sprites
		sta $4014 				;Start Spirte DMA transfer to OAM
		
		ldy #0
CustomNmihandlerAgain:	
		cpy VDP_CT		;See if there are any bytes left to write
		bcs CustomNmihandlerDone
		
		lda VDPBuffer,y
		iny	
		sta $2006	;PPUADDR 	Destination address - H
		
		lda VDPBuffer,y
		iny
		sta $2006	;PPUADDR 	Destination address - L
		
		lda VDPBuffer,y
		iny
		sta $2007 	;PPUDATA
		
		jmp CustomNmihandlerAgain;Process more bytes

CustomNmihandlerDone:		;Reset Scroll
		lda #0				;Scroll X
		sta VDP_CT
		sta $2005
		lda #0-8			;Scroll y
		sta $2005

		inc vblanked		;Alter Vblank Zero page entry
	pla
	tay
	pla
	plp
irqhandler:	;Do nothing
	rti						;Return from interrupt handler

	
	
	
NMI_LightGun:
	dec LightGunY
	txa
	pha
	
;Whiteout the screen
		lda #$3F		;Select Palette ram &3F00
		sta $2006		;PPUADDR H
		lda #0
		sta $2006		;PPUADDR L
	;8 Palettes (4 tile / 4 sprite) + 1 back
		ldx #4*4
PaletteAgain2
		lda PaletteW-1,x 
		sta $2007		;PPUDATA
		dex 
		bne PaletteAgain2
	
	
;Wait until we see the light!	
		ldy #0
LightGunScanAgain:	
		iny
		ldx #67					;1 block delay
LightGunScanDelay:
		lda $4017
			; ---FL---		;F=Fire (1=Yes) L=Light (0=Yes)
		and #%00001000
		beq LightGunScanFoundY

		dex
		bne LightGunScanDelay	;Delay for a block
		
		cpy #34					;At bottom of screen?
		beq LightGunScanFoundY
		
		jmp LightGunScanAgain

LightGunScanFoundY:	
		tya
		sec
		sbc #5					;first 5 are Vblank area
		sta LightGunY
	pla
	tax
	jmp CustomNmihandlerDone



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	
strConnectLightGun:				;255 terminated string
	db "Connect Lightgun to Port 2",255
	
PrintStr:
	ldy #0				;Set Y to zero
PrintStr_again:
	lda (z_hl),y			;Load a character from addr in $20+Y 
	
	cmp #255			;If we got 255, we're done
	beq PrintStr_Done
	
	jsr PrintChar		;Print Character
	iny					;Inc Y and repeat
	jmp PrintStr_again
PrintStr_Done:
	rts	

NewLine:
	lda #0
	sta Cursor_X	
	inc Cursor_Y
	rts
	
		
FillAreaWithTiles:
	sta z_As			;Backup tilenum
FillAreaWithTiles_Yagain:
	tya
	pha
	txa
	pha
		lda z_b
		pha
FillAreaWithTiles_Xagain:

		jsr GetVDPScreenPos	;Calculate Tilemap mempos		
			lda z_as		;Write Byte to VRAM
			sta VDPBuffer,y	;Save Tile selection to Vram Buffer
			iny
			sty VDP_CT		;INC and save Buffer Pos 
			
			inc z_b				;INC Xpos
			lda z_as 				;Move to next tile
			clc
			adc z_d
			sta z_as
			dex 
			bne FillAreaWithTiles_Xagain
		pla
		sta z_b
		
		inc z_c				;INC Ypos
	pla
	tax
	pla
	tay
	dey
	bne FillAreaWithTiles_Yagain
						
	rts

	
	
PrintChar:
	sec
	sbc #32			;No character below 32 in our font
	sta z_as
	
	txa
	pha
	tya
	pha	
		;Address= $2000 + (Ypos*32) + Xpos
		lda Cursor_Y
		asl				;%00000111
		asl
		asl
		asl
		asl				;%11100000
		ora Cursor_X
		pha				;L Byte
		
		lda Cursor_Y
		lsr				;%11111000
		lsr
		lsr				;%00011111
		clc
		adc #$20		;Tilemap Base (Nametable) = $2000
		pha				;Hbyte
			
		
		jsr GetVdpBufferCT	;Get the VDP buffer pos
		pla
		sta VDPBuffer,Y		;Store the address to the buffer
		iny
		pla
		sta VDPBuffer,Y
		iny					;We still need to write a data byte!
				
		lda z_as		;Write Byte to VRAM
		sta VDPBuffer,y	;Save Tile selection to Vram Buffer
		iny
		sty VDP_CT		;INC and save Buffer Pos 
		
		inc Cursor_X	;Move across screen
		lda Cursor_X
		cmp #32			;At end of line?
		bne PrintChar_NotNextLine
		jsr NewLine
PrintChar_NotNextLine:	
	pla
	tay
	pla
	tax
	rts

PaletteW:	
	db $30,$30,$30,$30
	db $30,$30,$30,$30
	db $30,$30,$30,$30
	db $30,$30,$30,$30
	
Palette:  
; 	Color   3   2   1  0
		db $3C,$21,$12,$0D	;Sprites 3 - Blues
		db $30,$23,$04,$0D	;Sprites 2 - Purples
		db $30,$16,$28,$0D	;Sprites 1 - Reds
		db $30,$2B,$13,$0D	;Sprites 0
PaletteB:		
		db $30,$2B,$13,$0D	;Tiles 3
		db $30,$2B,$13,$0D	;Tiles 2
		db $30,$2B,$13,$0D	;Tiles 1
		db $30,$2B,$13,$0D	;Tiles 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
Bitmap:
	incbin "\ResAll\Yquest\FontNES.raw"
	
;White block
	db $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
	db $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
	
	incbin "\ResALL\Sprites\RawNES.RAW"
BitmapEnd:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
waitframe:
	pha
		lda #$00
		sta vblanked		;Zero Vblanked
waitloop:
		lda vblanked		;Wait for the interrupt to change it
		beq waitloop
	pla
	rts
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	

;A=Hardware Sprite No. ixl,iyl = X,Y , E = Source Tile, l=Palette etc
SetHardwareSprite:	

	
	pha
		asl			;4 bytes per sprite - 64 sprites total
		asl
		sta $2003		;Select OAM address of sprite
		
		lda z_iyl		;Ypos
		sta $2004
		
		lda z_E			;Tilenum
		sta $2004
		
		lda z_l			;Attribs VHB---PP Vflip  Hflip  
		sta $2004			;Background priority  Palette
		
		lda z_ixl		;Xpos
		sta $2004
		
		jsr ResetScroll	;Writing to OAM messes up scroll
	pla
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
GetVDPScreenPos:			; BC=XYpos
	lda z_h
	pha
	lda z_l
	pha		
		lda z_c
		ifdef ScrWid256		;256x192
			clc
			adc #2
		endif
		and #%00000111		;Ypos * 32 tiles per line
		clc
		ror
		ror
		ror
		ror
		ora z_b				;Add Xpos
		sta z_l				;Store in L byte
		lda z_c
		ifdef ScrWid256	;256x192
			clc
			adc #2
		endif
		and #%11111000		;Other bits of Ypos for H byte
		clc
		ror
		ror
		ror
		clc
		adc #$20			;$2000 ofset for base of tilemap
		sta z_h
			
		jsr GetVdpBufferCT	;Get the VDP buffer pos
		lda z_h
		sta VDPBuffer,Y		;Store the address to the buffer
		iny
		lda z_l
		sta VDPBuffer,Y
		iny					;We still need to write a data byte!
	pla
	sta z_l
	pla 
	sta z_h
	rts
	
GetVdpBufferCT:	
	ldy VDP_CT
	cpy #32*3			;See if buffer is full
	bcc VdpNotBusy		
	jsr waitframe		;Buffer is full, so wait for Vblank
	ldy VDP_CT
VdpNotBusy:	
	lda #0
	sta VDP_CT			;Halt the queue
	rts
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
	;BC=Bytes
	;DE=Destination Ram
	;HL=Source Bytes		
DefineTiles:				;Send Data to tile definitions
	ldx z_C					;B=High byte of count - X=Low byte
	ldy #0
	jsr NesDisableScreen
	jsr prepareVram			;Calculate destination address
		
DefineTilesAgain	
	lda (z_HL),Y		
	sta $2007 				;PPUDATA - Write data to data-port
	iny
	bne DefineTilesAgainYok
	
	inc z_h					;INC High byte Y=low byte
DefineTilesAgainYok:		
	txa						;Is Low Byte Zero
	bne DefineTilesDecBC_C
	lda z_B					;Is High Byte Zero (Are We done?)
	beq DefineTilesAgainDone
	DEC z_B					;DEC Count High byte (X is low byte)
DefineTilesDecBC_C:	
	DEx						;Decrease Count Low Byte
	jmp DefineTilesAgain
DefineTilesAgainDone:
	jmp NesEnableScreen
	
	
prepareVram:				;Select a destination address

		lda z_d				;MSB - DEST ADDR
		sta $2006			;PPUADDR
		lda z_e				;LSB - Dest ADDR
		sta $2006			;PPUADDR
	rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	

NesDisableScreen:			;Turn OFF the screen
;(Sprite enable/back enable/Sprite leftstrip / backleftstrip)
	lda #%00000000	
	sta $2001				;PPUMASK
	;lda #$00				;NMI disable (Vblank)
	sta $2000				;PPUCTRL - VPHB SINN
	rts	

		
NesEnableScreen:			;Turn ON the screen
;(Sprite enable/back enable/Sprite leftstrip / backleftstrip)
	lda #%00011110 	
	sta $2001				;PPUMASK

	lda #$80				;NMI enable (Vblank)
	sta $2000				;PPUCTRL - VPHB SINN
	
	rts
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	


ResetScroll:	
	lda #0					;Scroll X
	sta $2005				;PPUSCROLL
	lda #0-8				;Scroll y
	sta $2005				;PPUSCROLL
	rts	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	

	
	
	;Debugging tools
	include "\SrcAll\monitor.asm"		
	;Basic commands for ASM tasks
	include "\SrcAll\BasicFunctions.asm"	

			
	
;Cartridge Footer	
	org $FFFA
	dw nmihandler			;FFFA - Interrupt handler
	dw ProgramStart			;FFFC - Entry point
	dw irqhandler			;FFFE - IRQ Handler
	
	
	
	
	
	
	