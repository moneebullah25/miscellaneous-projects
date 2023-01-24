	include "\SrcAll\BasicMacros.asm"

FourColor equ 1
	 
z_Regs 		equ $20
sppage equ $0100

UserRam equ $1000		;Game Data

ScreenWidth20 equ 1
ScreenWidth equ 160		;160x192 screen
ScreenHeight equ 192

Color4 equ 1			;Only 3 colors
Color3 equ 3
Color2 equ 2
Color1 equ 1


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

		lda #$68        ;Set color PF0 (Purple)
		sta GTIA+ $16
		
		lda #$00       ;Set color PF0 (Black)
		sta GTIA+ $1A
	else
		lda #$0F
		sta GTIA+ $17 	;COLPF1 equ 		
		
		lda #$00        ;2 color mode only uses the brightness of color1
		sta  GTIA+ $16
		sta  GTIA+ $18
		sta  GTIA+ $1A	
	endif


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	loadpair z_hl,userram	;Clear Game Ram
	loadpair z_bc,256
	jsr CLDIR0
		
	jsr MainMenu			;Main Menu
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


infloop:					;main loop
	lda tick
	clc
	adc #1
	and #%00000001			;Update Tick
	sta tick

	loadpair z_bc,400		;slow down delay

	lda boost
	bne boostoff
	loadpair z_bc,100		;boost - no delay
boostoff:					;(compensate for font draw)
	
	lda #255
	sta z_d					;key buffer
pausebc:
	pushpair z_bc
		pushpair z_de

			jsr Player_ReadControlsDual	
		pullpair z_de

		lda z_h
		cmp #255			;Key Pressed?
		beq pausenokey

		sta z_d
		jmp keysdown		;store any pressed joystick buttons

pausenokey:
		lda #0
		sta keytimeout		;released - nuke key, and relese keypress
	
		lda #255
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

	lda #1					;Turn boost off
	sta boost
	
processkeys:
	loadpair z_ix,playerxacc ;point ix to player accelerations 

	lda z_d
	and #%00000100			;---FLRUD L
	bne joynotleft
	
	dec playerdirection
	jsr setplayerdirection

	lda #1
	sta keytimeout

joynotleft:
	lda z_d
	and #%00001000			;---FLRUD R
	bne joynotright
	
	inc playerdirection
	jsr setplayerdirection
	
	lda #1					;ignore keypresses		
	sta keytimeout

joynotright:
	lda z_d
	and #%00010000			;---FLRUD	- Fire
	bne joynotfire

	lda boostpower			;check if boost power remains
	beq joynotfire

	lda #0					;Turn boost on
	sta boost
joynotfire:
joyskip:
	jsr handleplayer		;draw and update player
	jsr handlecpu			;draw and update cpu
	jmp infloop

	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
cls:
	loadpair z_hl,$2060
	loadpair z_bc, $1F00
	jmp cldir0				;Clear screen bytes


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		
pset:			;x=ab y=c color=d
	lda z_b
	cmp #250
	bcc PsetOK
	rts
PsetOK:
	lda z_d
	pha
		jsr getpixelmask ;get hl=address 
						  ;e=background mask d=pixel mask
	pla

	jsr getcolormasknum
	and z_e				;mask pixel color

	sta z_e
	ldx #0
	lda (z_hl,x)
	and z_d				;keep background pixels

	ora z_e				;Set pixel color
	sta (z_hl,x)	
	rts


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		
point:			;x=ab y=c color=d

	pushpair z_bc
		jsr getpixelmask
		ldx #0
		lda (z_hl,x)
		and z_e		;mask pixel color
		tax
	pullpair z_bc
	txa
	jsr bytetocolormask	;fill all pixels with same color

	loadpairsafe z_hl,colorlookup	;look up color 
	ldy #0
pointagain:
	
	cmp (z_hl),y	;Find the color
	beq pointdone
	iny
	jmp pointagain
pointdone:
	tya				;return color
	rts


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
;Get a byte with all the pixels the same color

getcolormasknum:	
	and #%00000011		;4 colors
	tay
	loadpairsafe z_bc,colorlookup	;Get filled mask
	lda (z_bc),y
	rts


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
;returns hl=mempos... d=mask to keep background - e= pixel to select	

getpixelmask:				;(b,c) = (x,y)		
	lda z_b
	pha
		lsr z_b				;4 pixels per byte
		lsr z_b			
		jsr getscreenpos	;get screen pos in HL
	pla
	and #%00000011
	tay
	loadpair z_bc,pixelbitlookup
	lda (z_bc),y
	sta z_e					;Pixel mask
	eor #255
	sta z_d					;Background mask
	rts
	
	align 2
pixelbitlookup:		;pixel positions
	db %11000000,%00110000,%00001100,%00000011
	
colorlookup:		;Byte filled with each color
	db %00000000,%01010101,%10101010,%11111111
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	
	;40 bytes per line = * %00000000 00101000
	;We shift our Ypos 3 to the right, add it, then another 2, eg
	
	;%00000000 00101000	=40 (32+8)
	;%YYYYYYYY 00000000
	;%000YYYYY YYY00000	= Y*32
	;%00000YYY YYYYY000 = Y*8
GetScreenPos:
	lda z_b	
	sta z_l			;Store X pos in L
		
	
	lda #0			;%YYYYYYYY 00000000		
	
	lsr z_c			;Shift 3 Bits
	ror 			;%0YYYYYYY Y0000000
	lsr z_c
	ror 			;%00YYYYYY YY000000
	lsr z_c
	ror 			;%000YYYYY YYY00000 = Y*32
	tax	
		clc
		adc z_l		;Update Low Byte
		sta z_l
		
		lda #$20	;Add Screen Base ($2060)
		
		adc z_c		;Update High Byte
		sta z_h
	txa
	lsr z_c			;Shift 2 bits
	ror 			;%0000YYYY YYYY0000
	lsr z_c
	ror 			;%00000YYY YYYYY000 = Y*8 
		
	adc z_l			;Add to Update Low Byte
	bcc GetScreenPosHok
	
	inc z_h			;L overflowed, so update H
	clc
GetScreenPosHok:

	adc #$60		;Add Screen Base ($2060)
	sta z_l
	
	lda z_c			;Add to Update High Byte
	adc z_h
	sta z_h	

	rts
	
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;fill whole byte with color - eg %00000001 to %00001111
bytetocolormask:
;a=color pixel... b=pixel pos (xpos)

	sta z_c
	lda z_b
	
	and #%00000011		;4 pixels per byte
	beq bytetocolormasknoshift
	tax
bytetocolormaskleftshift:
	asl z_c
	asl z_c
	dex
	bne bytetocolormaskleftshift
bytetocolormasknoshift:
	lda z_c
	ror
	ror
	ora z_c
	ror
	ror	
	ora z_c
	ror
	ror
	ora z_c
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
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
  
		
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
		
		
		
		