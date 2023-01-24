
;Screen size
ScreenWidth40 equ 1
ScreenWidth equ 40
ScreenHeight equ 25
ScreenObjWidth equ 160-2
ScreenObjHeight equ 200-8


;Masks for un-drawable co-ordinates 
collisionmaskX equ %11111110	
collisionmaskY equ %11111000

UserRam equ $3800		;Game Vars

z_Regs equ $20			;Temp Vars

	include "\SrcAll\BasicMacros.asm"

RunLocation equ $0200	;BBC version relocates code.
	ORG RunLocation  ;Actually our code runs at &3000 - but we shift it to here
	
BBCFirstByte:
	SEI			;Stop interrupts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
;Stop the sound chip making a noise!

	;&43 = Data Dir Reg A
	;&40 = I/O Reg B &40
	;&41 = I/O Reg A &41
	
	lda 255		;Set all bits to write
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
		txa 
		asl 	;X * 2
		tax
		
		tya 
		asl    ;Y * 8
		asl
		asl
		tay
		jsr showsprite		;Show the sprute
titlenosprite:
	pullxy
	pullpair z_hl
	jsr inchl				;Next Tile
	inx
	cpx #ScreenWidth		;Screen Width
	bne titlepixnextx
	iny
	cpy #24					;Screen Height
	bne titlepixnexty

	
	
	ldx #$12
	ldy #$02
	loadpair z_hl,txtFire		;Show Press Fire
	jsr LocateAndPrintString
	
	ldx #$18
	ldy #$18
	loadpair z_hl,TxtHiScore		
	jsr LocateAndPrintString
	loadpair z_de,HiScore		;Show the highscore
	ldx #4
	jsr BCD_Show
	
	LoadXY $0018
	loadpair z_hl,txtUrl		;Show URL
	jsr LocateAndPrintString	
	


startlevel:
	jsr waitforfire			;Wait for fire button
	jsr cls					;Clear Screen
	jsr ResetPlayer			;Center Player
	jsr levelinit			;Set up enemies
	
;Main Loop		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		
infloop:					;Main loop
	ldx #255				;Keypresses
	ldy #7					;Delay
PauseY
	Pushxy
		jsr Player_ReadControlsDual	;Get Keypresses
	pullxy
	lda z_h
	cmp #255				;Key Pressed?
	beq NoButton
	tax						;Yes - store for later
NoButton:
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
	and #%00000001	;---FLRUD
	bne JoyNotUp	;Jump if UP not presesd
	jsr DEC_IX_Y
	ldx #5
JoyNotUp:
	lda z_h
	and #%00000010	;---FRLDU
	bne JoyNotDown	;Jump if DOWN not presesd
	jsr INC_IX_Y
	ldx #5
JoyNotDown:
	ldy #O_Xacc
	lda z_h
	and #%00000100	;---FLRUD
	bne JoyNotLeft 	;Jump if LEFT not presesd
	jsr DEC_IX_Y
	ldx #5
JoyNotLeft:
	lda z_h
	and #%00001000	;---FLRUD
	bne JoyNotRight	;Jump if RIGHT not presesd
	jsr INC_IX_Y
	ldx #5
JoyNotRight:
	lda z_h
	and #%00010000	;---FLRUD
	bne JoyNotFire	;Jump if Fire not presesd
	pushX
		jsr PlayerFirebullet	;Fire a bullet
	pullX
JoyNotFire: 
	lda z_h
	and #%00100000	;---FLRUD
	bne JoyNotFire2	;Jump if Fire not presesd
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
	loadpair z_hl,$4180		;Screen Offset $4180
	loadpair z_bc,(80*200)	;Screen Offset $4180
	jmp cldir0				;Clear screen bytes
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PrintChar:				;Print Char in Accumulator
	sec
	sbc #32				;No char below space
	sta z_c
	pushxy
	pushpair z_hl
	pushpair z_bc
		
		loadpair z_hl,Font	;Source Bitmap Data
		
		lda #0
		asl z_c			;16 bytes per char
		rol
		asl z_c
		rol
		asl z_c
		rol
		asl z_c
		rol
		sta z_b
		jsr AddHL_BC

		lda CursorX		;2 bytes per horiz char
		asl
		tax 
		lda CursorY		;8 lines per vert char
		asl
		asl
		asl
		tay
		jsr showsprite
		inc CursorX
		pullpair z_bc
		pullpair z_hl
	pullxy
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BlankSprite:
	ldy #O_CollProg	
	lda (z_IX),y
	cmp #250		;Don't show if Object unused
	bcc DoBlank
	rts
DoBlank:
	loadpair z_hl,Font		;Source Bitmap Data
	jmp DrawBoth

GetSpriteAddr:
	asl 
	asl
	asl
	asl
	sta z_c			;16 bytes per sprite
	
	lda SpriteFrame
	sta z_b	

	loadpair z_hl,Bitmap	;Source Bitmap Data

	jmp AddHL_BC
	
DoGetSpriteObj:		;Get Settings from Object IX
	ldy #O_SprNum
	lda (z_ix),y	;Spr
	jsr GetSpriteAddr
DrawBoth:
	ldy #O_Xpos
	lda (z_ix),y	;object Xpos * 2
	lsr
	tax
	
	iny	;O_Ypos
	
	lda (z_ix),y	;object Ypos (ignore 3 bits)
	and #%11111000		;due to BBC screen layout
	tay
	
showsprite:
	stx z_b
	sty z_c
	jsr GetScreenPos;Get screen pos from XY into Z_DE
	
	ldY #0			;Offset for bytes in this strip
BitmapNextByte:
	lda (z_hl),Y	;Load in a byte from source - offset with Y
	sta (z_de),Y	;Store it in screen ram - offset with Y
	
	inY				;INC the offset
	cpY #8*2		;We draw 8 lines * bitmap width
	bne BitmapNextByte
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;BBC type is odd - the first 8 screen bytes go DOWN... 
;the 9ths goes back to the top 
;Effectively we're filling in 8x8 character blocks in a zigzag pattern

GetScreenPos:
	lda z_b
	pha
	lda z_c
	pha
		lda #0
		sta z_d
		
		txa			;Xpos
		asl
		rol z_d		;2
		asl 
		rol z_d		;4
		asl 
		rol z_d		;8		;8 bytes per X line
		sta z_e
		
		;We have to work in 8 pixel tall strips on the BBC
		tya			;Ypos
		and #%11111000
		lsr			;$04 00
		lsr			;$02 00
		sta z_b		;Multiply Y strip num by $02
		clc
		adc z_d		;Add to D
		sta z_d
		lda #0
		ror z_b		;$01 00		
		ror
		ror z_b		;$00 80
		ror
		adc z_e		;Add to E
		sta z_e
		lda z_b		;Add to D
		adc z_d
		sta z_d
	
		lda z_e
		adc #$80	;Screen Offset $4180
		sta z_e
		lda z_d
		adc #$41	;Screen Offset $4180
		sta z_d
	pla 
	sta z_c
	pla 
	sta z_b
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
waitforfire:
	jsr dorandom				;reseed random numbers
	jsr Player_ReadControlsDual	
	and #%00010000				;Test Fire Button
	bne waitforfire

waitforfireb:
	jsr dorandom				;reseed random numbers

	jsr Player_ReadControlsDual	
	and #%00010000				;Test Fire Button
	beq waitforfireb
	rts
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
Player_ReadControlsDual:
	lda #$F0					;Set port to read (For fire button)
	STA $FE43				;SN76489 - Data Direction
	sta z_as
	
	;lda #%00000000			;Get Channel 0 - Joy 1 LR
	jsr Player_ReadControlsGetData
	lda #%00000001			;Get Channel 1 - Joy 1 UD
	jsr Player_ReadControlsGetData
		
	lda $FE40
	and #%00110000			;Get the fire button 1+2 (PB4 / PB5)
	ora z_as
	eor #%11001111
	sta z_h
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	include "\SrcAll\V1_ChibiSound.asm"
	include "\SrcAll\BasicFunctions.asm"
	include "\SrcAll\BCD.asm"
	include "\srcALL\MultiPlatform_ShowDecimal.asm"

	include "YQ_Multiplatform.asm"
	include "YQ_DataDefs.asm"
	include "YQ_RamDefs.asm"
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
Font:	
	incbin "\ResAll\Yquest\FontBBC.raw"	
	
Bitmap:
	incbin "\ResAll\Yquest\BBC_YQuest.RAW"
	incbin "\ResAll\Yquest\BBC_YQuest2.RAW"
	incbin "\ResAll\Yquest\BBC_YQuest3.RAW"
	incbin "\ResAll\Yquest\BBC_YQuest4.RAW"

	
	
CRTCConfig:
	db $7F		;0 - Horizontal total
	db $50		;1 - Horizontal displayed characters
	db $62		;2 - Horizontal sync position
	db $28		;3 - Horizontal sync width/Vertical sync time
	db $26		;4 - Vertical total
	db $00		;5 - Vertical total adjust
	db $19		;6 - Vertical displayed characters (25)
	db $20		;7 - Vertical sync position
	db $01		;8 - Interlace/Display delay/Cursor delay
	db $07		;9 - Scan lines per character
	db %00110000;10 - Cursor start line and blink type
	db $0		;11 - Cursor end line
	db $08		;12 - Screen start address H (Address /8)
	db $30		;13 - Screen start address L ($4130/8=$0830)
	db 0
	db 0 
	
ULAConfig:	
Palette0:	;Colours
;		SC  SC		-	S=Screen C=Color
	db $07,$17	;0
	db $47,$57	;0
Palette1:
	db $22,$32		;1
	db $62,$72		;1
Palette2:
	db $85,$95			;2
	db $C5,$D5			;2
Palette3:
	db $A0,$B0				;3
	db $E0,$F0				;3

BBCLastByte: db 0

	
;EOR True   Color
;7  (0) 	black
;6  (1) 	red
;5  (2) 	green
;4  (3) 	yellow (green—red)
;3  (4) 	blue
;2  (5) 	magenta (red—blue)
;1  (6) 	cyan (green—blue)
;0  (7) 	white

