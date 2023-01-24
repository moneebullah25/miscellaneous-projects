
;Screen Size
ScreenWidth40 equ 1
ScreenWidth equ 40
ScreenHeight equ 25
ScreenObjWidth equ 160-2
ScreenObjHeight equ 200-8

;Masks for un-drawable co-ordinates 
collisionmaskX equ %11111110
collisionmaskY equ %11111000

FourColor equ 1
	 
z_Regs equ $20		;Temp Vars
SprPtn equ $1F		;Sprite Color pointer
UserRam equ $5000	;Game Vars

	include "\SrcAll\BasicMacros.asm"

;Init Routine
*=$0801
	db $0E,$08,$0A,$00,$9E,$20,$28,$32,$30,$36,$34,$29,$00,$00,$00  
*=$0810	;Start at $0810

	;	  LXMSHVVV - L=Cur Line X=extended BG M=mode 
				;(Txt/Bmp) S=screen on H=height V=Vert scroll
	lda #%00111011	;turn on graphics mode
	sta $D011
	
	;     ---MWHHH - M=Multicolor W=scr width H=horiz scroll
	ifdef FourColor
		lda #%11011000  ;1=Multicolor 4 coor 
	else
		lda #%11001000  ;0=standard 2 color 
	endif
	sta $D016

	;     SSSSTTT- - T=Text/Bmp screen address S=Screen (color) address
	lda #%00011000  ;T=1 Screen at $2000 					
	sta $D018			;(Other bits have no function in bitmap mode)
	
	lda $DD00
	and #%11111100
	ora #%00000010	;Screen base at $4000 range
	sta $DD00
	
	;	  ----CCCC
	lda #%00000000
	sta $D021		;Background color (only bits #0-#3).	
	lda #%00001011
	sta $D020		;Border 
		
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
	jsr waitforfire		
	jsr cls
	jsr ResetPlayer			;Center Player
	jsr levelinit			;Set up enemies
	
	
	
infloop:					;Main loop
	ldx #255				;Keypresses
	ldy #4					;Delay
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
	loadpair z_hl,$6000		
	loadpair z_bc,$2000		
	jmp cldir0			;Clear Screen
	
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PrintChar:
	sec
	sbc #32				;No char below space
	sta z_c
	pushxy
	pushpair z_hl
	pushpair z_bc
		lda #16*2
		sta SprPtn		;Store Sprite Color for later
		
		loadpair z_hl,Font	;Source Bitmap Data
		lda #0
		
		asl z_c			;16 bytes per char
		rol
		asl z_c
		rol
		asl z_c
		rol
		sta z_b
		jsr AddHL_BC

		lda CursorX		;1 byte per horiz char
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
	asl 
	sta SprPtn	;Store Sprite Color offset for later
	asl
	asl
	sta z_c			;8 bytes per sprite
	
	lda #0
	sta z_b
	
	loadpair z_hl,Bitmap	;Source Bitmap Data
	jsr AddHL_BC
	
	lda SpriteFrame		;128 bytes per bank
	sta z_b	
	lda #0
	lsr z_b
	ror
	sta z_c
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
	txa
	pha
	tya
	pha
		stx z_b
		sty z_c
		jsr GetScreenPos ;Get screen pos from XY into Z_DE

BitmapNextLine:
		ldY #0			 ;Offset for bytes in this strip
BitmapNextByte:
		lda (z_hl),Y	 ;Load in a byte from source offset with Y
		sta (z_de),Y	 ;Store it in screen ram - offset with Y
		inY				 ;INC the offset
		cpY #8			 ;We draw 8 lines * bitmap width
		bne BitmapNextByte
	pla
	tay
	pla
	tax
	
;Fill Color Data
	stx z_b
	sty z_c
	jsr GetColMemPos	;Get color pos from XY into Z_DE
	ldy SprPtn
	loadpair z_hl,SpritePalette
	
	ldx #0
	ifdef FourColor
		lda (z_hl),y	;Color
		sta (z_de,x)	;%22221111 Color 1,2
		iny 
		lda (z_hl),y	;Color
		sta (z_bc,x)	;%----3333 Color 3 
	else
		lda #$40		;Color
		sta (z_de,x)	;%11110000 Color 1,2
	endif		
	rts
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	

;Screen base is $4000 - to move us away from program code
	
;Address= (X * 8) + (Top5BitsOfY * 40) + $2000+$4000
GetScreenPos:
	lda #0
	sta z_b
	sta z_d
	txa				;Multiple X by 8
	asl				;-------- XXXXXXXX
	rol z_d
	asl
	rol z_d
	asl
	rol z_d			;-----XXX XXXXX---
	sta z_e

;40 bytes per Yline =00000000 00101000
	tya
	and #%11111000	;00000000 YYYYYyyy
	asl
	rol z_b
	asl
	rol z_b
	asl				;00000000 00101000
	rol z_b			;00000YYY YYyyy000
	tax 
		adc z_e		;Add part to total L
		sta z_e
		lda z_b		;Add part to total H
		adc z_d
		sta z_d
	txa 
	asl
	rol z_b
	asl				;00000000 00101000
	rol z_b			;000YYYYY yyy00000
	
	adc z_e			;Add part to total L
	sta z_e
	lda z_b			;Add part to total H
	adc z_d
	adc #$60		;Screen Base $4000+$2000
	sta z_d
	rts
	
	
;Color Ram data at $D800 & $400
;Address = $0400+ Y Strip * 40 + Xpos

GetColMemPos:
	lda #0
	sta z_d
	txa
	sta z_e			;Xpos
;40 bytes per Yline =00000000 00101000
	tya				;Need to multiply by 40 (%00101000)
	and #%11111000	;One color per 8x8 square
	tay 
		clc
		adc z_e		;Add Ypos part to Xpos 
		sta z_e		;Save %00-01000 part
	tya 
	asl
	rol z_d	
	asl				;00000000 00101000
	rol z_d			;000YYYYY yyy00000
	clc
	adc z_e			;Add Ypos part to total
	sta z_e
	sta z_c
	
	lda z_d
	adc #$44+0		;Color Offset $0400+$4000
	sta z_d
	
	adc #$D8-$44	;Color Offset $D800
	sta z_b
	rts				;z_DE = $4400-47FF byte  
					;z_BC = $D800-DBFF byte (for 4 color)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Player_ReadControlsDual:	
	;lda $DC00			;Read in Joystick 1
	lda $DC01			;Read in Joystick 2
	sta z_h			;---FRLDU
	rts
		
waitforfire:
	jsr dorandom				;reseed random numbers
	jsr Player_ReadControlsDual	;---FRLDU
	and #%00010000
	bne waitforfire

waitforfireb:
	jsr dorandom				;reseed random numbers

	jsr Player_ReadControlsDual	;---FRLDU
	and #%00010000
	beq waitforfireb
	rts
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
Font:	
	incbin "\ResAll\Yquest\FontC64.raw"
	
Bitmap:
	incbin "\ResAll\Yquest\C64_YQuest.raw"
	incbin "\ResAll\Yquest\C64_YQuest2.raw"
	incbin "\ResAll\Yquest\C64_YQuest3.raw"
	incbin "\ResAll\Yquest\C64_YQuest4.raw"


	
SpritePalette:
	db $A2,$0F		;0
	db $24,$0A		;1
	db $E6,$03		;2
	db $A2,$07		;3
	db $CF,$01		;4
	db $A2,$0F		;5
	db $CB,$0C		;6
	db $82,$08		;7
	db $B5,$0D		;8
	db $24,$0A		;9
	db $6E,$03		;10
	db $56,$0D		;11
	db $3E,$01		;12
	db $D5,$01		;13
	db $42,$0C		;14
	db $4B,$04		;15
	
	db $BC,$01		;Font
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	
	include "\SrcAll\V1_ChibiSound.asm"
	include "\SrcAll\BasicFunctions.asm"
	include "\SrcAll\BCD.asm"
	include "\srcALL\MultiPlatform_ShowDecimal.asm"


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	include "YQ_DataDefs.asm"
	include "YQ_Multiplatform.asm"
	
	include "YQ_RamDefs.asm"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	