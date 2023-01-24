
sppage equ $0100

z_Regs 	equ $20			;Temp Vars

UserRam equ $200		;Game Vars

ScreenWidth32 equ 1
ScreenWidth equ 280		;Screen is 280x192 pixels
ScreenHeight equ 192

Color4 equ 1			;Apple II only has one color (on/off)
Color3 equ 1
Color2 equ 1
Color1 equ 1

	include "\SrcAll\BasicMacros.asm"

	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
	ORG $0C00	;Program Start
	sei 		;Disable interrupts
	
	lda $C050 	;TXTCLR:   Display Graphics
	lda $C052 	;MIXCLR:   Display Full Screen
	lda $c057 	;HIRES:    Display HiRes Graphics
	lda $C055 	;TXTPAGE2: If 80STORE Off: Display Page 2
	
	jsr cls
	
;Fill our Div 7 lookup table	
	
	loadpair z_hl,DivTable
	ldy #0
	lda #0				;Whole Units
DivTableAgain:
	ldx #0				;Remainder
DivTableAgain2:	
	sta (z_hl),y		;Whole part
	pha
		jsr IncHl
		txa
		sta (z_hl),y	;Remainder
		jsr IncHl
	pla
	inx
	cpx #7				;We're dividing by 7
	bne DivTableAgain2
	clc
	adc #1
	cmp #40				;7*40=280 (Screen Width)
	bne DivTableAgain
	
	
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	loadpair z_hl,userram	;Clear Game Ram
	loadpair z_bc,256
	jsr CLDIR0
		
	jsr MainMenu			;Main Menu
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


infloop:					;main loop
			
	lda tick				;Update Tick
	clc
	adc #1
	and #%00000001
	sta tick

	loadpair z_bc,30		;slow down delay

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
	bne pausebc				;See if BC=0

startdraw:
	lda keytimeout			;See if Keytimeout is set
	bne joyskip

	lda #1
	sta boost
	
processkeys:
	loadpair z_ix,playerxacc ;point ix to player accelerations 

	lda z_d
	and #%00000100			;---FLRUD - L
	beq joynotleft
	
	dec playerdirection

	jsr setplayerdirection

	lda #1
	sta keytimeout

joynotleft:
	lda z_d
	and #%00001000			;---FLRUD - R
	beq joynotright
	
	inc playerdirection
	jsr setplayerdirection
	
	lda #1					;ignore keypresses		
	sta keytimeout

joynotright:

	lda z_d
	and #%00010000			;---FLRUD - Fire
	beq joynotfire

	lda boostpower			;check if boost power remains
	beq joynotfire

	lda #0
	sta boost
joynotfire:

joyskip:
	jsr handleplayer		;draw and update player
	jsr handlecpu			;draw and update cpu
	jmp infloop


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
cls:
	loadpair z_hl,$4000		;Screen Offset $4000
	loadpair z_bc,$2000		
	jmp cldir0				;Clear screen bytes

	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
;(ab,c) = (x,y)		

pset:			;x=ab y=c color=d
	tax
	lda z_d
	pha
		txa
		jsr GetPixelPos	;Get DE memory address 
							;+ H=BG Mask L=Pixel Mask
		ldx #0
	pla 
	beq Pset0			;Check color
	lda (z_de,x)
	and z_l				;Keep background
	ora z_h				;Set pixel
	sta (z_de,x)
	rts
Pset0:
	lda (z_de,x)
	and z_l				;Keep background
	sta (z_de,x)
	rts


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

point:
	jsr GetPixelPos	;Get DE memory address 
							;+ H=BG Mask L=Pixel Mask
	ldx #0
	lda (z_de,x)	;Get screen byte
	and z_h			;Mask pixel we're looking at
	
	beq Point0		;Return 0
	lda #1			;Return 1
Point0:
	rts
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	
;Return Screen Address DE  for X,Y pos AB,C
GetPixelPos:		
	sta z_h		;Xpos in HL
	lda z_b
	sta z_l
	
	lda z_c		;Ypos in A
	pha
	
;Calc pixel via LUT (byte = xpos /7)
		clc
		rol z_l					;2 Result bytes per Entry
		rol z_h
		addpair z_hl,DivTable
		ldy #0
		lda (z_hl),y			;Whole Result (Byte)
		pha
			iny 
			lda (z_hl),y		;Remainder (pixels)
			tay
		pla
		sta z_l
		
;Calc pixel via division (byte = xpos /7)
		; lda #7
		; jsr Div16			;Divide Xpos by 7
		; tay
		
;Used by both	
		lda pixelbitlookup,y	;Get Pixel Mask
		sta z_d					;Foreground pixel
		eor #255
		sta z_e					;Background mask
	pla
	tay	
	
	pushpair z_de
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
		tya 				;AA------		;multiply by $0028
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
		tya  			;-----CCC	;Multiply by 4
		and #%00000111
		asl
		asl
		adc z_d
		sta z_d
		
		lda z_l			;Process X
		clc
		adc z_e			;Add X to calculated address
		sta z_e		
	pullpair z_hl
	rts	

	;Position of each pixel in a byte
pixelbitlookup:
	db %00000001,%00000010,%00000100,%00001000,%00010000,%00100000,%01000000

DivTable:
	ds 280*2	;Table for Dividing By 7

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
	pha					;delay
	pla				
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
	
