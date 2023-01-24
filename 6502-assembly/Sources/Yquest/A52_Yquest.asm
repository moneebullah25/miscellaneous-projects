
;Screen size
ScreenWidth40 equ 1
ScreenWidth equ 40
ScreenHeight equ 25
ScreenObjWidth equ 160-2
ScreenObjHeight equ 200-8

;Masks for un-drawable co-ordinates 
collisionmaskX equ %11111110
collisionmaskY equ %11111000

FourColor equ 1

z_Regs 	equ $20		;Temp Vars

UserRam equ $1000	;Game Data

	include "\SrcAll\BasicMacros.asm"

	


	ifdef BuildA80		;Atari 800 settings
GTIA equ $D000			;GTIA address
PIA  equ $D300			;PIA address
POKEY equ $D200
	org $A000     	  	;Start of cartridge area
	else				;Atari 5200 settings
GTIA  equ $C000			;GTIA address
POKEY equ $E800			;POKEY address
	org $4000       	;Start of cartridge area	
	endif
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	INIT

ProgramStart:        
	sei             ;Disable interrupts
	
    ldx #$00		;Zero GTIA area and Zero Page
    txa
ClearLoop    
    sta $00,x   	;Clear zero page
    sta GTIA,x      ;Clear GTIA
    dex
    bne ClearLoop
		
	lda #<DisplayList
	sta $D402 		;DLISTL - Display list lo
	lda #>DisplayList
	sta $D403 		;DLISTH - Display list hi
	
	lda #%00100010   	
	sta $D400 		;DMACTL - DMA Control (screen on)

	ifdef FourColor
		lda #$98      	;Set color PF1 (foreground) (CYAN)
		sta GTIA+ $17 	;COLPF1 equ 
		
		lda #$0F       	;Set color PF2 (background) (White)
		sta GTIA+ $18	;COLPF2 

		lda #$C8        ;Set color PF0 (Green)
		sta GTIA+ $16
		
		lda #$00       ;Set color PF0 (Black)
		sta GTIA+ $1A
	else
		lda #$0F
		sta GTIA+ $17 	;COLPF1 equ 		
		
		lda #$00        ;2 color mode only 
		sta  GTIA+ $16		;uses the brightness of color1
		sta  GTIA+ $18
		sta  GTIA+ $1A	
	endif

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
	lda #<titlepic
	sta z_l
	lda #>titlepic
	sta z_h
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
	pushxy 		;X * 1
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
	ldy #255				;Delay
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
	
	ldx #255
DelayB
	dex
	bne DelayB
	
	jmp infloop


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	
cls:
	loadpair z_hl,$2060
	loadpair z_bc, $1F00
	jmp cldir0				;Clear screen bytes

	
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PrintChar:
	sec
	sbc #32				;No char below space
	sta z_c
	pushxy
	pushpair z_hl
	pushpair z_bc
		loadpair z_hl,Font	;Source Bitmap Data
		lda #0
		
		asl z_c			;8 bytes per char
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
	asl
	asl
	sta z_c			;8 bytes per sprite
	
	lda #0
	sta z_b
	
	loadpair z_hl,Bitmap		;Source Bitmap Data
	jsr AddHL_BC
	
	lda SpriteFrame
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
	stx z_b
	sty z_c

	jsr GetScreenPos	;Calculate Memory address of XY 
	ldY #0			;Line Count
BitmapNextLine:
	ldx #0
	lda (z_hl),Y 	;Copy a byte from the source 
	sta (z_de,x)	;to the destination
		
	;Move down a line (40 Bytes / $0028)
	addpair z_de, $0028
	
	iny
	cpy #8			;Height (8 bytes)
	bne BitmapNextLine		
	rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	
	
	
GetScreenPos:
	txa				;Store X pos in E
	sta z_e			
	tya 			;Get Ypos - store in B
	sta z_b			;%YYYYYYYY 00000000		
	
	lda #0
	lsr z_b			;Shift 3 Bits
	ror 			;%0YYYYYYY Y0000000
	lsr z_b
	ror 			;%00YYYYYY YY000000
	lsr z_b
	ror 			;%000YYYYY YYY00000 = Y*32
	tax	
		clc
		adc z_e		;Update Low Byte
		sta z_e
		
		lda z_b 		;Update High Byte
		adc #0
		sta z_d
	txa
	lsr z_b			;Shift 2 bits
	ror 			;%0000YYYY YYYY0000
	lsr z_b
	ror 			;%00000YYY YYYYY000 = Y*8 
	adc z_e			;Add to Update Low Byte
	sta z_e
	
	lda z_b			;Add to Update High Byte
	adc z_d
	sta z_d	

	clc
	addpair z_de,$2060 ;Add Screen Base ($2060)
	rts


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
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
	
	;We Return ---FRLDU in z_h for Player 0, and z_L for Player 1

	ifdef BuildA80	
Player_ReadControlsDual:
		lda PIA+$0	;22221111 - RLDU in player controls
		and #%00001111	;Bottom Nibble is Player 1 Joystick
		ora #%11100000
		sta z_h

		lda GTIA+$10	;$D010 - TRIG0 - joystick trigger 0
		clc
		rol
		rol
		rol
		rol
		ora z_h			;Joystick 1 Done
		sta z_h			
		ifdef UseDualJoy 			
			lda GTIA+$11	;$D011 - TRIG1 - joystick trigger 1
			ror 
			php
				lda PIA+$0	;22221111 - RLDU in player controls
				and #%11110000	;Top Nibble is Player 2 Joystick
			plp
			ror
			ror
			ror
			ror
			ora #%11100000
			sta z_l			;Joystick 2 Done
		else
			lda #255		;Disable Joystick 2
		endif
		sta z_l
		lda z_h
	rts
	endif
	
	
	ifdef BuildA52		;Atari 5200 doesn't have PIA 
Player_ReadControlsDual:
	lda GTIA+$10		;$C010 - TRIG0 - joystick trigger 0
	sta z_as
	
	lda pokey+0			;$E800 - POT0 - game paddle 0
	jsr Player_ReadControlsProcessAnalog
	
	lda pokey+1			;$E801 - POT1 - game paddle 1
	jsr Player_ReadControlsProcessAnalog
	
	lda #%11100000
	ora z_as
	sta z_h
	
	ifdef UseDualJoy 	
		lda GTIA+$11	;$C011 - TRIG1 - joystick trigger 1
		sta z_as
		
		lda pokey+2		;$E802 - POT2 - game paddle 2
		jsr Player_ReadControlsProcessAnalog
		
		lda pokey+3		;$E803 - POT3 - game paddle 3
		jsr Player_ReadControlsProcessAnalog
		
		lda #%11100000
		ora z_as
	else
		lda #255		;Disable Joystick 2
	endif
	sta z_l
	lda z_h
	rts
	
	;Convert Analog to Digital
Player_ReadControlsProcessAnalog:
	cmp #255-64
	bcs Player_ReadControlsProcessHigh
	cmp #64
	bcc Player_ReadControlsProcessLow
	sec
	bcs Player_ReadControlsProcessB

Player_ReadControlsProcessHigh:		;B/R
	clc
Player_ReadControlsProcessB:
	rol z_as
	sec
	rol z_as
	rts
Player_ReadControlsProcessLow:		;T/L
	sec
	rol z_as
	clc
	rol z_as
	rts
	endif
	
	

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

	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		
Bitmap:
	incbin "\ResAll\Yquest\C64_YQuest.raw"
	incbin "\ResAll\Yquest\C64_YQuest2.raw"
	incbin "\ResAll\Yquest\C64_YQuest3.raw"
	incbin "\ResAll\Yquest\C64_YQuest4.raw"
Font:	
	incbin "\ResAll\Yquest\FontC64.raw"

		
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	Display List
  
    org $bF20
	ifdef FourColor	
Smode Equ $0E	;E=4 color.... F=2 color
	else
Smode Equ $0F	;E=4 color.... F=2 color
	endif
DisplayList:				;Display list data
	db $70,$70,$70;$70 7= 8 blank lines 0= blank lines

		db $40+Smode,$60,$20	;Strange start ($2060) to safely step over the boundary
		
		db	         Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		db	   Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		db	   Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		db	   Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		db	   Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		
		db	   Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		db	   Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		db	   Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		db	   Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		db	   Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		
		db $40+Smode,$00,$30	;Have to manually step over the 4k boundary ($3000)
		db	         Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		db	   Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		db	   Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		db	   Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		db	   Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		
		db	   Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		db	   Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		db	   Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		db	   Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		db	   Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
	db $41					;Loop
	dw DisplayList
		

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	Rom Footer
	org $bffd
	db $FF         ;Disable Atari Logo
	dw ProgramStart;program Start
		
		
		
		