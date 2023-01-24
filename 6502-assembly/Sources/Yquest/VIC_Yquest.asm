;Mask collision detection routines
CollisionMaskY equ %11111000 
CollisionMaskX equ %11111100

ScreenWidth20 equ 1
ScreenWidth equ 22
ScreenHeight equ 23
ScreenObjWidth equ 92-4
ScreenObjHeight equ 144-12+48

	include "\SrcAll\BasicMacros.asm"

SprPtn		equ $5F
z_Regs 		equ $60			;Fake Registers
UserRam equ $1000

* = $A000
		dw ProgramStart
		dw ProgramStart
		db $41,$30,$C3,$C2,$CD		;ROM Header
ProgramStart:
;Screen Init
	ldx #16					;We're going to copy 16 registers 
ScreenInitAgain:	
	dex
	lda VicScreenSettings,x	;Get A parameter
	sta $9000,X				;Store to the video registers at $9000
	txa
	bne ScreenInitAgain
	
;Transfer Tiles (Characters)
	loadpair z_hl,Bitmap			;Source Bitmap Data
	loadpair z_bc,(BitmapEnd-Bitmap);Source Length
	loadpair z_de,$1C00				;Custom Char 0
	jsr ldir						;Transfer to Ram 

	
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
	loadpair z_hl,titlepic	;TitlePicture Source
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
		inx					;Offset X+1
		jsr showsprite		;Show the sprite
titlenosprite:
	pullxy
	pullpair z_hl
	jsr inchl				;Next Tile
	inx
	cpx #20					;Screen Width
	bne titlepixnextx
	iny
	cpy #18					;Screen Height
	bne titlepixnexty


	
	
	ldx #$0A+2
	ldy #$00
	loadpair z_hl,txtFire		;Show Press Fire
	jsr LocateAndPrintString
	
	ldx #$00
	ldy #$12
	loadpair z_hl,TxtHiScore		
	jsr LocateAndPrintString
	loadpair z_de,HiScore		;Show the highscore
	ldx #4
	jsr BCD_Show
	
	ldx #$08+2
	ldy #$04
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
	sta z_b
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
	and #%00000001	
	bne JoyNotUp	;Jump if UP not presesd
	jsr DEC_IX_Y
	ldx #5
JoyNotUp:
	lda z_h
	and #%00000010	
	bne JoyNotDown	;Jump if DOWN not presesd
	jsr INC_IX_Y
	ldx #5
JoyNotDown:
	ldy #O_Xacc
	lda z_h
	and #%00000100	
	bne JoyNotLeft 	;Jump if LEFT not presesd
	jsr DEC_IX_Y
	ldx #5
JoyNotLeft:
	lda z_h
	and #%00001000
	bne JoyNotRight	;Jump if RIGHT not presesd
	jsr INC_IX_Y
	ldx #5
JoyNotRight:
	lda z_h
	and #%00010000
	bne JoyNotFire	;Jump if Fire not presesd
	pushX
		jsr PlayerFirebullet	;Fire a bullet
	pullX
JoyNotFire: 
	;lda z_h
	;and #%00100000	
	;bne JoyNotFire2	;Jump if Fire not presesd
	;lda #0
	;sta PlayerAccX	;Stop movement
	;sta PlayerAccY
JoyNotFire2
	stx KeyTimeout	;Update KeyTimeout
JoySkip: 
	
	jsr drawandmove	;Draw Player Sprite
	
	jmp infloop


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	
cls:
	loadpair z_hl,$1E00
	loadpair z_bc,$0200
	lda #128+32				;Space character
	jmp cldir				;Clear screen bytes
	

	
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PrintChar:
	sta z_as
	pushxy
	pushpair z_hl
	pushpair z_bc
		lda #$1E		;Screen starts at $1E00
		sta z_h
		lda CursorX
		sta z_l
		lda #0
		sta z_b
		ifdef ModeVicWide
			lda #28		;ScreenWidth
		else
			lda #22
		endif
		sta z_c
		ldy Cursory
		beq PrintChar_YZero
PrintChar_Addagain:	
		jsr AddHL_BC	;Multiply Y by Screen Width
		dey
		bne PrintChar_Addagain
	PrintChar_YZero:
		lda z_as
		clc
		cmp #64
		bcc PrintChar_Symbols
	PrintChar_Letters:
		sbc #64;
		and #%00011111		;No Lower case!
	PrintChar_Symbols:
		clc 
		adc #128
		ldx #0
		sta (z_hl,x)
		lda #$78			;Colors offset by $7800
		clc
		adc z_h
		sta z_h
		lda #1				;Set Color
		sta (z_hl,x)		;Save Color
			
		inc CursorX			;Move to next Char
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
	lda #128+32				;Space Character
	sta z_H
	jmp DrawBoth

GetSpriteAddr:
	sta SprPtn				;Color offset
	sta z_h
	lda SpriteFrame			;16 characters per bank
	asl
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
		
	sta (z_hl),y		;Transfer Tile to ram
	lda z_h
	clc
	adc #$78			;add Offset to Color Ram ($9600)
	sta z_h
	lda #4				;Color
	
	loadpair z_de,SpritePalette
	ldy SprPtn
	lda (z_de),y
	ldy #0
	sta (z_hl),y		;Set Tile Color
	rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Address= $1E00 + (Ypos * 22) + Xpos	
	
GetVDPScreenPos:	; BC=XYpos	
	lda #$1e			;Screen base is $1E00
	sta z_h					;Colors at $9600 (add $7800 offset)
	
	lda z_b				;Xpos
	sta z_l

	ldy z_c				;Ypos
	beq GetVDPScreenPos_YZero
GetVDPScreenPos_Addagain:	;Repeatedly add screen width (22) Y times 
	clc
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
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	
waitforfire:

	jsr dorandom				;reseed random numbers
	jsr Player_ReadControlsDual	
	and #%00010000	
	bne waitforfire

waitforfireb:
	jsr dorandom				;reseed random numbers

	jsr Player_ReadControlsDual	
	and #%00010000	
	beq waitforfireb
	rts
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


	;R:      3210
	;W:		   CS			C=Clear S=Select key/dir
	
	;Reset the Multitap... following reads will read in 
		;from joysticks 1-5
Player_ReadControlsDual:
	lda #%01111111
	sta $9122	;Set Data Direction of port B to READ (0=read)
	
;	lda #%11000011
;	sta $9113	;Set Data Direction of port A to READ (0=read)
	
	lda $9120	;Port B (R------- Switch)
	sta z_as
	
	lda #255	;Set all buttons to unpressed
	sta z_l
	sta z_h
	
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
	
	;lda #255
	;sta $9122	;Reset port B (for Keyb col scan)
	lda z_h
	rts
	
	include "\SrcAll\V1_ChibiSound.asm"
	include "\SrcAll\BasicFunctions.asm"
	include "\SrcAll\BCD.asm"
	include "\srcALL\MultiPlatform_ShowDecimal.asm"


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	
	include "YQ_Multiplatform.asm"
	include "YQ_DataDefs.asm"
	include "YQ_RamDefs.asm"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


	
	
Bitmap:
	incbin "\ResAll\Yquest\VIC_YQuest.raw"
	incbin "\ResAll\Yquest\VIC_YQuest2.raw"
	incbin "\ResAll\Yquest\VIC_YQuest3.raw"
	incbin "\ResAll\Yquest\VIC_YQuest4.raw"
BitmapEnd

SpritePalette:
	db 2,4,3,2,1,1,1,2
	db 5,4,3,2,3,5,1,4
	


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
VicScreenSettings:
	db $0C		;$9000 - horizontal centering
	db $26		;$9001 - vertical centering
	db $96		;$9002 - set # of columns / 
					;Bit7 = screen base bit ($16 for screen at $1000)
	db $AE		;$9003 - set # of rows
	db $7A		;$9004 - TV raster beam line
	db $FF		;$9005 - bits 0-3 start of character memory /  
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
	
	