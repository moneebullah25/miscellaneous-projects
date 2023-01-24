
;Screen size
ScreenWidth20 equ 1
ScreenWidth equ 20
ScreenHeight equ 17
ScreenObjWidth equ 80-4
ScreenObjHeight equ 144-12

UserRam equ $B000		;Game Vars

z_Regs 		equ $20		;Temp Vars

	include "\SrcAll\BasicMacros.asm"



	org $200-10		;Our program starts at $0200
	db $80,$08,$02,$00,$40,$0A,$42,$53,$39,$33
	
;ScreenInit	-	SUZY chip needs low byte setting first 
					;OR IT WILL WIPE THE HIGH BYTE!
	lda #$9E
	sta $FD00	;TIM0BKUP	 HTIMBKUP Timer 0 backup value
	lda #$18
	sta $FD01	;TIM0CTLA	 HTIMCTL0 Timer 0 static control

	
	lda #$68	;backup value for vertical scan timer (== 102 vertical lines plus 2)
	sta $FD08	;TlM2BKUP	 VTIMBKUP Timer 2 backup value
	lda #$1F
	sta $FD09	;TIM2CTLA	 Timer 2 static control
	
	lda #$29
	sta $FD93	;PBKUP	Magic P count
	
	;Set screen ram pointer to $C000
	lda #$00
	sta $FD94	;DISPADR	Display Address L (Visible)
	sta $FC08	;VIDBAS		Base address of video build buffer L (Sprites)
	
	lda #$C0	
	sta $FD95	;DISPADR	Display Address H (Visible)
	sta $FC09	;VIDBAS		Base address of video build buffer H (Sprites)
	
	lda #$7F
	sta $FC28 ;HSIZOFF	Horizontal size offset
	sta $FC2A ;VSIZOFF	Vertical Size Offeet

	
;Do the palette
	ldx #0
	ldy #0
	loadpair z_hl,Palette
	stz $2121		;CGADD - Colour selection  
PaletteAgain:
		 ;gggrrrrr 
	lda (z_hl),y
	sta $FDB0,x		;CGDATA - Colour data register
		 ;?bbbbbgg 
	iny
	lda (z_hl),y
	sta $FDA0,x		;CGDATA
	iny
	inx
	cpx #16
	bne PaletteAgain


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
		asl 	;X * 4
		asl
		tax
		
		tya 
		asl    ;Y * 8
		asl
		asl
		tay
		jsr showsprite		;Show the sprute
titlenosprite:
	pullxy
	pullpair z_hl			;Next Tile
	jsr inchl
	inx
	cpx #ScreenWidth		;Screen Width
	bne titlepixnextx
	iny
	cpy #17					;Screen Height
	bne titlepixnexty

	
	
	ldx #$0A
	ldy #$00
	loadpair z_hl,txtFire		;Show Press Fire
	jsr LocateAndPrintString
	
	ldx #$04
	ldy #$0C
	loadpair z_hl,TxtHiScore		
	jsr LocateAndPrintString

	loadpair z_de,HiScore		;Show the highscore
	ldx #4
	jsr BCD_Show
	
	ldx #$08
	ldy #$04
	loadpair z_hl,txtUrl		;Show URL
	jsr LocateAndPrintString	
	


startlevel:
	jsr waitforfire
	jsr cls
	jsr ResetPlayer			;Center Player
	jsr levelinit			;Set up enemies
	
	
	
infloop:					;Main loop
	ldx #0					;Keypresses
	ldy #255				;Delay
PauseY
	Pushxy
	jsr Player_ReadControlsDual	;Get Keypresses
	pullxy
	lda z_h
	cmp #0					;Key Pressed?
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
	jmp JoySkip		;skip player input
	
ProcessKeys:
	ldx #0			;Key Timeout
	ldy #O_Yacc
	lda z_h
	and #%10000000	;UDLR12IO
	beq JoyNotUp	;Jump if UP not presesd
	jsr DEC_IX_Y
	ldx #5
JoyNotUp:
	lda z_h
	and #%01000000	;UDLR12IO
	beq JoyNotDown	;Jump if DOWN not presesd
	jsr INC_IX_Y
	ldx #5
JoyNotDown:
	ldy #O_Xacc
	lda z_h
	and #%00100000	;UDLR12IO
	beq JoyNotLeft 	;Jump if LEFT not presesd
	jsr DEC_IX_Y
	ldx #5
JoyNotLeft:
	lda z_h
	and #%00010000	;UDLR12IO
	beq JoyNotRight	;Jump if RIGHT not presesd
	jsr INC_IX_Y
	ldx #5
JoyNotRight:
	lda z_h
	and #%00001000	;UDLR12IO
	beq JoyNotFire	;Jump if Fire not presesd
	pushX
		jsr PlayerFirebullet	;Fire a bullet
	pullX
JoyNotFire: 
	lda z_h
	and #%00000100	;UDLR12IO
	beq JoyNotFire2	;Jump if Fire not presesd
	lda #0
	sta PlayerAccX	;Stop movement
	sta PlayerAccY
JoyNotFire2
	stx KeyTimeout	;Update KeyTimeout
JoySkip: 
	
	jsr drawandmove	;Draw Player Sprite
	
	ldx #255		;Delay Loop
	lda #0
DelayB
	clc
	sbc #1
	bne DelayB
	dex
	bne DelayB
	
	jmp infloop


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	
cls:
	loadpair z_hl,$C000
	loadpair z_bc, $1FE0
	jmp cldir0				;Clear screen bytes

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PrintChar:
	sec
	sbc #32				;No char below space
	sta z_c
	pushxy
	pushpair z_hl
	pushpair z_bc
		loadpair z_hl,Font		;Source Bitmap Data
		
		lda #0
		asl z_c			;32 bytes per char
		rol
		asl z_c
		rol
		asl z_c
		rol
		asl z_c
		rol
		asl z_c
		rol
		sta z_b
		jsr AddHL_BC

		lda CursorX		;4 bytes per horiz char
		asl
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

BlankSprite:
	ldy #O_CollProg
	lda (z_IX),y
	cmp #250
	bcc DoBlank
	rts
DoBlank:
	loadpair z_hl,Font		;Source Bitmap Data
	jmp DrawBoth

GetSpriteAddr:
	pha
		lda #0
		sta z_b
	pla
	asl 
	rol z_b
	asl
	rol z_b
	asl
	rol z_b
	asl
	rol z_b
	asl
	rol z_b
	sta z_c				;32 bytes per sprite
	
	loadpair z_hl,Bitmap	;Source Bitmap Data
	jsr AddHL_BC
	
	lda #0
	sta z_c
	lda SpriteFrame		;512 bytes per bank 
	asl
	sta z_b	
	jmp AddHL_BC
	
	
DoGetSpriteObj:		;Get Settings from Object IX
	ldy #O_SprNum
	lda (z_ix),y	;Spr
	jsr GetSpriteAddr
DrawBoth:
	ldy #O_Xpos
	lda (z_ix),y	;object Xpos 
	tax
	
	iny	;O_Ypos
	lda (z_ix),y	;object Ypos (ignore 3 bits)
	and #%11111110
	tay
		
showsprite:
	jsr GetScreenPos;Get screen pos from XY into Z_DE
	ldx #0
BitmapNextLine:
	phy
		ldY #0
BitmapNextByte:
		lda (z_hl),Y	;Copy a byte from the source 
		sta (z_de),Y	;to the destination
			
		inY
		cpY #4			;Repeat for next byte of line
		bne BitmapNextByte
		
		clc
		tya
		adc z_l			;ADD Y to Z_HL to move source 
		sta z_l
		lda z_h
		adc #0
		sta z_h
			
		clc
		lda z_e
		adc #$50		;ADD 50 to Z_DE to move Destination
		sta z_e
		lda z_d
		adc #0
		sta z_d
	ply
	inx 
	cpx #1
	beq LnxSkipLine
	cpx #5
	beq LnxSkipLine
	cpx #8			;Check if we've done all the lines
	bne BitmapNextLine		;Repeat until we have
	rts
LnxSkipLine:
	AddPair z_Hl,4	;Skip 4 bytes (8 pixes) on unwanted lines
	inx 
	jmp BitmapNextLine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
GetScreenPos:
	clc
	tya
	lsr
	sta z_c
	lsr
	clc
	adc z_c
	tay
	
	lda #$00		;Reset z_C
	sta z_c
	
	tya 			;Move Y into top byte 	= YYYYYYYY 00000000
	lsr
	ror z_c
	lsr 
	ror z_c			;Shift Right Twice      = 00YYYYYY YY000000
	
	sta z_d			;Store High byte in total	
	lda z_c			
	sta z_e			;Store Low byte in total
	
	lda z_d			;Shift Right Twice      = 0000YYYY YYYY0000
	lsr
	ror z_c
	lsr 
	ror z_c
	
	clc				;Add High byte to total
	adc z_d
	adc #$C0		;Screen base at &C0000
	sta z_d

	
	lda z_c			;Add Low byte to total
	adc z_e
	sta z_e
	
	lda z_d			;Add any carry to the high byte
	adc #0
	sta z_d
	
	clc				;Add the X pos 
	txa 
	adc z_e 
	sta z_e
	
	lda z_d			;Add any carry to the high byte
	adc #0
	sta z_d
	rts
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
Player_ReadControlsDual:
	lda $FCB0		;JOYSTICK	Read Joystick and Switches	
	sta z_h			;UDLR12IO
	rts

waitforfire:
	jsr dorandom				;reseed random numbers
	jsr Player_ReadControlsDual ;UDLR12IO
	and #%00001000
	bne waitforfire

waitforfireb:
	jsr dorandom				;reseed random numbers
	jsr Player_ReadControlsDual	;UDLR12IO
	and #%00001000
	beq waitforfireb
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
	incbin "\ResALL\Yquest\MSX2_Font.raw"
	
Bitmap:
	incbin "\ResAll\Yquest\LNX_Yquest.RAW"
	incbin "\ResAll\Yquest\LNX_Yquest2.RAW"
	incbin "\ResAll\Yquest\LNX_Yquest3.RAW"
	incbin "\ResAll\Yquest\LNX_Yquest4.RAW"
	
	

Palette:
	dw $0000; ;0  -GBR
	dw $0F01; ;1  -GBR
	dw $0555; ;2  -GBR
	dw $0AAA; ;3  -GBR
	dw $0FFF; ;4  -GBR
	dw $0862; ;5  -GBR
	dw $0D33; ;6  -GBR
	dw $033E; ;7  -GBR
	dw $076E; ;8  -GBR
	dw $0A5E; ;9  -GBR
	dw $0F4F; ;10  -GBR
	dw $02AA; ;11  -GBR
	dw $00FF; ;12  -GBR
	dw $03D0; ;13  -GBR
	dw $06B3; ;14  -GBR
	dw $0DF0; ;15  -GBR
