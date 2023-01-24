
z_Regs 		equ $40

z_HL equ z_Regs
z_L  equ z_Regs
z_H  equ z_Regs+1

z_BC equ z_Regs+2
z_C  equ z_Regs+2
z_B  equ z_Regs+3

z_DE equ z_Regs+4
z_E  equ z_Regs+4
z_D  equ z_Regs+5

;Current player pos
PlayerX 	equ $60		;Position of next printed character
PlayerY 	equ PlayerX+1

;Last player pos (For clearing sprite)
PlayerX2 	equ PlayerX+2
PlayerY2 	equ PlayerX+3


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

nmihandler:		;This procuedure runs after each frame (See footer.asm)
	php
	inc vblanked		;Alter Vblank Zero page entry
	plp
irqhandler:	;Do nothing
	rti

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
	
	ldx #4
PaletteAgain
	lda Palette-1,x 
	sta $2007		;PPUDATA
	dex 
	bne PaletteAgain

;Turn ON the screen
;(Sprite enable/back enable/Sprite leftstrip / backleftstrip)
	lda #%00011110 	
	sta $2001		;PPUMASK
	
	lda #$80		;NMI enable (Vblank)
	sta $2000		;PPUCTRL - VPHB SINN

	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	
	lda #<Bitmap	;Source Bitmap Data
	sta z_L
	lda #>Bitmap
	sta z_H

	lda #<(BitmapEnd-Bitmap);Bitmap Data Length
	sta z_C
	lda #>(BitmapEnd-Bitmap)
	sta z_B
	
	lda #<$0800		;Tile 128 (16 bytes per tile)
	sta z_E
	lda #>$0800
	sta z_D
	
	jsr DefineTiles	;Define the tile patterns

	
		
	lda #3			;Start SX
	sta PlayerX
	lda #3			;Start SY
	sta PlayerY

	lda #0			;Fake No Keys on first run		
	sta z_h
	jmp StartDraw	;Force Draw of character first run
	
	
infloop:
	jsr Player_ReadControlsDual
	lda z_h
	beq infloop		;See if no keys are pressed

StartDraw:	
	ldx PlayerX		;Back up X
	stx PlayerX2
	
	ldy PlayerY		;Back up Y
	sty PlayerY2
	
	jsr BlankPlayer	;Remove old player sprite
	
	lda z_h
	and #%00010000	;RLDUSsBA 
	beq JoyNotUp	;Jump if UP not presesd
	dey				;Move Y Up the screen
JoyNotUp:
	lda z_h
	and #%00100000	;RLDUSsBA
	beq JoyNotDown	;Jump if DOWN not presesd
	iny 			;Move Y Down the screen
JoyNotDown:
	lda z_h
	and #%01000000	;RLDUSsBA 
	beq JoyNotLeft	;Move X Left 
	dex
JoyNotLeft:
	lda z_h
	and #%10000000	;RLDUSsBA
	beq JoyNotRight	;Move X Right
	inx
JoyNotRight:

	stx PlayerX		;Update X
	sty PlayerY		;Update Y
	
;X Boundary Check - if we go <0 we will end up back at 255
	cpx #32
	bcs PlayerReset
	
;Y Boundary Check - only need to check 1 byte
	cpy #28
	bcs PlayerReset
	
	jmp PlayerPosYOk ;Not Out of bounds
	
PlayerReset:
	ldx PlayerX2	;Reset Xpos	
	stx PlayerX
	
	ldy PlayerY2	;Reset Ypos
	sty PlayerY
	
PlayerPosYOk:
	jsr DrawPlayer	;Draw Player Sprite
	
	ldx #255
	ldy #100
	jsr PauseXY		;Wait a bit!	
		
	jmp infloop
	

PauseXY:
	dex
	bne PauseXY
	dey 
	bne PauseXY
	rts

		
Palette:  
; 	Color   3   2   1  0
		db $30,$21,$14,$0D

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



BlankPlayer:
	lda #129		;Tile Num (blank)
	jmp DrawSprite
DrawPlayer:
	lda #128		;Tile Num (Smiley)
DrawSprite:
	pha
		stx z_b
		sty z_c
		jsr GetVDPScreenPos	;Calculate Tilemap mempos
	pla
	sta $2007		;PPUDATA - Save Tile selection to Vram
	jmp resetscroll	;Need to reset scroll after writing to VRAM

Bitmap:	;Smiley Sprite
;		00000000 - Bitplane 0
    DB %00111100     ;  0
    DB %01111110     ;  1
    DB %11111111     ;  2
    DB %11111111     ;  3
    DB %11111111     ;  4
    DB %11011011     ;  5
    DB %01100110     ;  6
    DB %00111100     ;  7
;		11111111 - Bitplane 1	
    DB %00000000     ;  1
    DB %00000000     ;  2
    DB %00100100     ;  3
    DB %00000000     ;  4
    DB %00000000     ;  5
    DB %00100100     ;  6
    DB %00011000     ;  7
    DB %00000000     ;  8
;Clear Sprite
	ds 16,0
BitmapEnd:


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
		jsr waitframe		;Wait for Vblank
		sta $2006			;PPUADDR
		lda z_L
		sta $2006			;PPUADDR
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

	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	


NesDisableScreen:			;Turn OFF the screen
;(Sprite enable/back enable/Sprite leftstrip / backleftstrip)
	lda #%00000000	
	sta $2001				;PPUMASK
	;lda #$00				;NMI disable (Vblank)
	sta $2000				;PPUCTRL - VPHB SINN
	rts	
		
NesEnableScreen:			;Turn ON the screenY
;(Sprite enable/back enable/Sprite leftstrip / backleftstrip)
	lda #%00011110 	
	sta $2001				;PPUMASK
	lda #$80				;NMI enable (Vblank)
	sta $2000				;PPUCTRL - VPHB SINN
	rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	

prepareVram:				;Select a destination address
	lda z_d					;MSB - DEST ADDR
	sta $2006				;PPUADDR
	lda z_e					;LSB - Dest ADDR
	sta $2006				;PPUADDR
	rts

ResetScroll:	
	lda #0					;Scroll X
	sta $2005				;PPUSCROLL
	lda #0-8				;Scroll y
	sta $2005				;PPUSCROLL
	rts	

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
		
;Cartridge Footer	
	org $FFFA
	dw nmihandler			;FFFA - Interrupt handler
	dw ProgramStart			;FFFC - Entry point
	dw irqhandler			;FFFE - IRQ Handler
	
	
	
	
	
	
	