
;Mask collision detection routines
CollisionMaskY equ %11111000 
CollisionMaskX equ %11111100

ScreenWidth32 equ 1
ScreenWidth equ 32
ScreenHeight equ 24
ScreenObjWidth equ 128-2
ScreenObjHeight equ 192

ScrWid256 equ 1			;Nes Centering

z_Regs  equ $40
UserRam equ $6000

	include "\SrcAll\BasicMacros.asm"



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
VDP_CT equ z_Regs+18	;Position of next Tile in buffer
VDPBuffer equ $200		;Tilemap Buffer
SpriteBuffer equ VDPBuffer+$100	;SpriteBuffer

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	

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

	lda #%10000000	;Turn on extra ram at $6000-%7FFF
	sta $A001
	
;Define patterns
	loadpair z_hl,Bitmap	;Source Bitmap Data
	loadpair z_bc,(BitmapEnd-Bitmap);Bitmap Data Length
	loadpair z_de,0	;Tile 0 (16 bytes per tile)
	jsr DefineTiles	;Define the tile patterns

	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				
;Clear Game Data
	loadpair z_hl,UserRam	;Start
	loadpair z_bc,$800		;Bytes
	jsr cldir0				;Zero Range

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
ShowTitle:
;Init Game Defaults
	lda #3
	sta lives			;Life count
	lda #0
	sta level  			;Level number
	sta PlayerObject	;Player Sprite
	jsr ChibiSound		;Mute sound

	jsr cls				;Clear Screen

	
;Show Title Screen	

	loadpair z_hl,titlepic	;TitlePicture source
	ldy #0
titlepixnexty:
	ldx #0
titlepixnextx:
	pushpair z_hl
	pushxy
		ldx #0
		lda (z_hl,x)	  ;Sprite number
		beq titlenosprite	
		jsr GetSpriteAddr ;Get Sprite Ram Addr
	pullxy
	pushxy
		jsr showsprite		;Show the sprute
titlenosprite:
	pullxy
	pullpair z_hl
	jsr inchl
	inx
	cpx #ScreenWidth		;Screen Width
	bne titlepixnextx
	iny
	cpy #24					;Screen Height
	bne titlepixnexty
	
	
	
	ldx #$0D
	ldy #$10
	loadpair z_hl,txtFire		;Show Press Fire
	jsr LocateAndPrintString	
	
	ldx #$10
	ldy #$00
	loadpair z_hl,TxtHiScore		
	jsr LocateAndPrintString

	loadpair z_de,HiScore		;Show the highscore
	ldx #4
	jsr BCD_Show
	
	LoadXY $1202
	loadpair z_hl,txtUrl		;Show URL
	jsr LocateAndPrintString	
	
startlevel:
	jsr waitforfire	
	jsr cls
	jsr ResetPlayer			;Center Player
	jsr levelinit			;Set up enemies
	
	
	
infloop:					;Main loop
	ldx #255				;Keypresses
	ldy #255				;Delay
PauseY
	lda #2
	sta z_b					;Delay #2
PauseB
	Pushxy
		jsr Player_ReadControlsDual	;Get Keypresses
	pullxy
	lda z_h
	cmp #255				;Key Pressed?
	beq NoButton
	tax						;Yes - store for later
NoButton:
	dec z_b
	bne PauseB
	dey 
	bne PauseY
	
	
	txa
	pha
		jsr drawui		;Show User Interface
	
		loadpair z_ix,PlayerObject
		jsr BlankSprite	;Remove old player sprite
	pla
	sta z_h
	
	
	lda KeyTimeout	;ignore UDLR during key timeout
	beq ProcessKeys
	dec KeyTimeout
	jmp JoySkip			;skip player input
	
ProcessKeys:
	ldx #0			;Key Timeout
	ldy #O_Yacc
	lda z_h
	and #%00010000	;RLDUSsBA 
	beq JoyNotUp	;Jump if UP not presesd
	jsr DEC_IX_Y
	ldx #5
JoyNotUp:
	lda z_h
	and #%00100000	;RLDUSsBA 
	beq JoyNotDown	;Jump if DOWN not presesd
	jsr INC_IX_Y
	ldx #5
JoyNotDown:
	ldy #O_Xacc
	lda z_h
	and #%01000000	;RLDUSsBA 
	beq JoyNotLeft 	;Jump if LEFT not presesd
	jsr DEC_IX_Y
	ldx #5
JoyNotLeft:
	lda z_h
	and #%10000000	;RLDUSsBA 
	beq JoyNotRight	;Jump if RIGHT not presesd
	jsr INC_IX_Y
	ldx #5
JoyNotRight:
	lda z_h
	and #%00000001	;RLDUSsBA 
	beq JoyNotFire	;Jump if Fire not presesd
	pushX
		jsr PlayerFirebullet	;Fire a bullet
	pullX
JoyNotFire: 
	lda z_h
	and #%00000010	;RLDUSsBA 
	beq JoyNotFire2	;Jump if Fire not presesd
	lda #0
	sta PlayerAccX	;Stop movement
	sta PlayerAccY
JoyNotFire2
	stx KeyTimeout	;Update KeyTimeout
JoySkip: 
	
	jsr drawandmove	;Draw Player Sprite
	
	jmp infloop


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	
cls:
	ldy #0
clsnexty:
	ldx #0
clsnextx:
	pushxy
		lda #0
		sta z_h
		jsr showsprite		;Show the sprute
	pullxy
	inx
	cpx #ScreenWidth		;Screen Width
	bne clsnextx
	iny
	cpy #ScreenHeight		;Screen Height
	bne clsnexty
	rts	

	
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PrintChar:
	sec
	sbc #32				;No char below space
	sta z_as
	pushxy
	pushpair z_hl
	pushpair z_bc
		lda z_as
		sta z_h
		ldx CursorX		;Get XYpos
		ldy CursorY		
		
		jsr showsprite	;Show Character tile
		inc CursorX		;Move along screen
	pullpair z_bc
	pullpair z_hl
	pullxy
	rts

BlankSprite:
	ldy #O_CollProg
	lda (z_IX),y
	cmp #250
	bcc DoBlank
	rts
DoBlank:
	lda #0
	sta z_H
	jmp DrawBoth

GetSpriteAddr:
	clc 
	adc #96				;First 96 sprites are font
	sta z_h
	lda SpriteFrame
	asl					;16 sprites per bank
	asl
	asl
	asl
	clc
	adc z_h
	sta z_h
	rts
	
DoGetSpriteObj:		;Get Settings from Object IX
		ldy #0
		lda (z_ix),y	;Spr
		jsr GetSpriteAddr
DrawBoth:
		ldy #2
		lda (z_ix),y	;object Xpos * 2
		lsr
		lsr
		tax
		
		iny
		lda (z_ix),y	;object Ypos (ignore 3 bits)
		and #%11111000
		lsr
		lsr
		lsr
		tay
		
showsprite:
	lda z_h
	pha
		stx z_b
		sty z_c
		jsr GetVDPScreenPos	;Calculate Tilemap mempos
	pla
	sta VDPBuffer,y	;Save Tile selection to Vram Buffer
	iny
	sty VDP_CT		;INC and save Buffer Pos 
	rts 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

waitframe:
	pha
		lda #$00
		sta vblanked		;Zero Vblanked
waitloop:
		lda vblanked		;Wait for the interrupt to change it
		beq waitloop
	pla
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
Nmihandler:
	php
	pha
	tya
	pha
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
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

	
Bitmap:
	incbin "\ResAll\Yquest\FontNES.raw"
	
	incbin "\ResAll\Yquest\NES_YQuest.RAW"
	incbin "\ResAll\Yquest\NES_YQuest2.RAW"
	incbin "\ResAll\Yquest\NES_YQuest3.RAW"
	incbin "\ResAll\Yquest\NES_YQuest4.RAW"
BitmapEnd
	
Palette:  
; 	Color   3   2   1  0
		db $30,$2B,$13,$0D
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	
	include "\SrcAll\V1_ChibiSound.asm"
	include "\SrcAll\BasicFunctions.asm"
	include "\SrcAll\BCD.asm"
	include "\srcALL\MultiPlatform_ShowDecimal.asm"

	include "YQ_Multiplatform.asm"
	include "YQ_DataDefs.asm"
	include "YQ_RamDefs.asm"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
		
;Cartridge Footer	
	org $FFFA
	dw nmihandler		;FFFA - Interrupt handler
	dw ProgramStart		;FFFC - Entry point
	dw irqhandler		;FFFE - IRQ Handler
	
	
	
	
	
	