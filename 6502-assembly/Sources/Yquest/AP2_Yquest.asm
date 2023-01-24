
;Screen size
ScreenWidth40 equ 1
ScreenWidth equ 40
ScreenHeight equ 24
ScreenObjWidth equ 160-2
ScreenObjHeight equ 200-8

;Masks for un-drawable co-ordinates 
collisionmaskX equ %11111100
collisionmaskY equ %11111000

z_Regs 		equ $40		;Temp Vars

UserRam equ $6000		;Game Vars

	include "\SrcAll\BasicMacros.asm"



	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	

	ORG $0C00	;Program Start
ProgramStart:	
	sei 		;Disable interrupts
	
	lda $C050 	;TXTCLR:   Display Graphics
	lda $C052 	;MIXCLR:   Display Full Screen
	lda $c057 	;HIRES:    Display HiRes Graphics
	lda $C055 	;TXTPAGE2: If 80STORE Off: Display Page 2
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
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
	pushxy		;X * 1
		tya 
		asl    	;Y * 8
		asl
		asl
		tay
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

	
	
	ldx #$12
	ldy #$02
	loadpair z_hl,txtFire		;Show Press Fire
	jsr LocateAndPrintString

	ldx #$18
	ldy #$17
	loadpair z_hl,TxtHiScore		
	jsr LocateAndPrintString
	loadpair z_de,HiScore		;Show the highscore
	ldx #4
	jsr BCD_Show
	
	LoadXY $0017
	loadpair z_hl,txtUrl		;Show URL
	jsr LocateAndPrintString	
	


startlevel:
	jsr waitforfire
	jsr cls
	jsr ResetPlayer			;Center Player
	jsr levelinit			;Set up enemies
	
	
	
infloop:				;Main loop
	ldx #0				;Keypresses
	ldy #16				;Delay
PauseY
	Pushxy
		jsr Player_ReadControlsDual	;Get Keypresses
	pullxy
	lda z_h
	cmp #0				;Key Pressed?
	beq NoButton
	tax					;Yes - store for later
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
	beq JoyNotUp	;Jump if UP not presesd
	jsr DEC_IX_Y
	ldx #5
JoyNotUp:
	lda z_h
	and #%00000010	;---FRLDU
	beq JoyNotDown	;Jump if DOWN not presesd
	jsr INC_IX_Y
	ldx #5
JoyNotDown:
	ldy #O_Xacc
	lda z_h
	and #%00000100	;---FLRUD
	beq JoyNotLeft 	;Jump if LEFT not presesd
	jsr DEC_IX_Y
	ldx #5
JoyNotLeft:
	lda z_h
	and #%00001000	;---FLRUD
	beq JoyNotRight	;Jump if RIGHT not presesd
	jsr INC_IX_Y
	ldx #5
JoyNotRight:
	lda z_h
	and #%00010000	;---FLRUD
	beq JoyNotFire	;Jump if Fire not presesd
	pushX
		jsr PlayerFirebullet	;Fire a bullet
	pullX
JoyNotFire: ;No Fire 2 :-(
	;lda z_h
	;and #%00100000	;---FLRUD
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
	loadpair z_hl,$4000		;Screen Offset $4000
	loadpair z_bc,$2000		
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
		asl z_c			;8 bytes per char
		rol
		asl z_c
		rol
		asl z_c
		rol
		sta z_b
		jsr AddHL_BC

		lda CursorX		;1 byte per char
		tax 
		lda CursorY		;8 lines per vert char
		asl
		asl
		asl
		tay
		jsr showsprite
		inc CursorX		;Move across 1 char
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
	loadpair z_hl,Font ;Source Bitmap Data
	jmp DrawBoth

GetSpriteAddr:
	asl 
	asl
	asl
	sta z_c			;8 bytes per sprite
	
	lda #0
	sta z_b

	loadpair z_hl,bitmap ;Source Bitmap Data
	jsr AddHL_BC
	
	lda #0
	sta z_c
	lda SpriteFrame	;128 bytes per bank
	lsr 
	ror z_c
	sta z_b
	jmp AddHL_BC
	

DoGetSpriteObj:		;Get Settings from Object IX
	ldy #O_SprNum
	lda (z_ix),y	;Spr
	jsr GetSpriteAddr
DrawBoth:
	ldy #O_Xpos
	lda (z_ix),y	;object Xpos
	lsr
	lsr
	tax
	
	iny	;O_Ypos
	lda (z_ix),y	;object Ypos (ignore 3 bits)
	and #%11111000
	tay
		
showsprite:
	stx z_b
	sty z_c
	jsr GetScreenPos
	
	ldY #0
BitmapNextLine:
	ldx #0
	lda (z_hl),Y 	;Read byte from source
	sta (z_de,X) 	;Write to screen
			
	lda z_d			;move mempos down a line
	clc
	adc #$04	;add $0400 to the line number 
	sta z_d		;  (only works within an 8 line block)

	iny
	cpy #8
	bne BitmapNextLine;need a recalc every 8 lines
	rts
		

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	

;BBC type is odd - the first 8 screen bytes go DOWN... 
;the 9ths goes back to the top 
;Effectively we're filling in 8x8 character blocks in a zigzag pattern

	
	;Screen layout is split in 3 parts according to Y line
	;AABBBCCC - AA*$0028  BBB*$0080  CCC*$0400
GetScreenPos:
	lda #0
	sta z_e
	tya 				;--BBB---	;Multiply by $0080
	and #%00111000
	lsr
	lsr
	lsr					
	lsr					;Shift 1 bit right 
	ror z_e
	adc #$40			;Screen base
	sta z_d
	tya					;AA------		;multiply by $0028
	rol 				;Get 1st A from AA------ 
	bcc GetScreenPos_SecondThird
GetScreenPos_ThirdThird:
	lda z_e
	clc
	adc #$50			;3/3 = Add $0050 to address
	jmp GetScreenPos_ThirdDone
GetScreenPos_SecondThird:
	rol 				;Get 2nd A from AA------ 
	bcc GetScreenPos_FirstThird
	lda z_e
	clc
	adc #$28			;3/2 = Add $0028 to address
GetScreenPos_ThirdDone:	
	sta z_e
GetScreenPos_FirstThird:;1/3 = Add nothing to addreess
	tya 			;-----CCC	;Multiply by 4
	and #%00000111
	asl
	asl
	adc z_d
	sta z_d
	
	txa				;Process X
	clc
	adc z_e			;Add X to calculated address
	sta z_e
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
;Apple Joysticks are annoying!
;they are analog... we have to strobe the port 
;then read from the X and Y ports, and count up until the top bit changes
;this is a 'timer'...using just 1 bit (the top one) 
;it effectively returns an 'analog' value from about 0-100
	
Player_ReadControlsDual:	;---FRLDU
	lda $C061			;Fire 1
	and #%10000000
	rol	;Move in the fire button
	rol z_h
	
	lda $C070			;Strobe Joypads
	ldy #0
	ldx #0 
Joy_ReadAgain:
	pha
	pla					;delay
Joy_gotPDL1:			;Jump backhere when we get X
Joy_ChkPDl0:
	lda	$C064 			;<--SM ***   Y
JoySelfModAA_Plus2:
	bpl Joy_gotPDL0		;Have we got Y?
	nop
	iny	
	lda $C065			;<--SM ***   X
JoySelfModB_Plus2:
	bmi Joy_nogots		;Have we got X?
	bpl Joy_gotPDL1
Joy_nogots:
	inx
	jmp Joy_ChkPdl0
Joy_gotPDL0:			;We've Got Tpos - just waiting for X
	lda  $C065			;<--SM ***   X
JoySelfModBB_Plus2:
	bmi Joy_Nogots
	
	tya
	jsr JoyConvertAnalog;Convert Y
	txa
						;Convert X
	jsr JoyConvertAnalog;Convert Y
	lda z_h
	rts
	
JoyConvertAnalog:	;covert analog from 0-100 into L/R or U/D
	cmp #$66
	bcs Joy_Rbit
	cmp #$33
	bcc Joy_Lbit
	clc 
	bcc Joy_Cbit
Joy_Rbit:
	sec 
Joy_Cbit:
	rol z_h
	clc
	rol z_h
	rts
	
Joy_Lbit:
	clc
	rol z_h
	sec 
	rol z_h
	rts
	
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
waitforfire:

	jsr dorandom				;reseed random numbers
	jsr Player_ReadControlsDual	;km get joystick... returns ---frldu
	and #%00010000
	bne waitforfire

waitforfireb:
	jsr dorandom				;reseed random numbers

	jsr Player_ReadControlsDual	;km get joystick... returns ---frldu
	and #%00010000
	beq waitforfireb
	rts
	
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
Font:	
	incbin "\ResAll\Yquest\FontAP2.Raw"
	
Bitmap:
	incbin "\ResAll\Yquest\AP2_YQuest.raw"
	incbin "\ResAll\Yquest\AP2_YQuest2.raw"
	incbin "\ResAll\Yquest\AP2_YQuest3.raw"
	incbin "\ResAll\Yquest\AP2_YQuest4.raw"



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	
	include "\SrcAll\V1_ChibiSound.asm"
	include "\SrcAll\BasicFunctions.asm"
	include "\SrcAll\BCD.asm"
	include "\srcALL\MultiPlatform_ShowDecimal.asm"

	
	include "YQ_Multiplatform.asm"
	include "YQ_DataDefs.asm"
	include "YQ_RamDefs.asm"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
