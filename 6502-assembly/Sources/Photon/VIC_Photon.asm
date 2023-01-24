;screen base and char base at $1000
; this means first 240 bytes of chars are used by the screen!

;We're using Double Height mode - so each char is 16 bytes

	include "\SrcAll\BasicMacros.asm"

sppage equ $0100

z_Regs equ $20


UserRam equ $200		;Game Vars

ScreenWidth20 equ 1
ScreenWidth equ 160		;Screeb size (20*12)
ScreenHeight equ 192

Color4 equ 7			;Color attribs
Color3 equ 5
Color2 equ 4
Color1 equ 3


	org $A000
	dw ProgramStart
	dw ProgramStart
	db $41,$30,$C3,$C2,$CD		;ROM Header
ProgramStart:

;Screen Init

;We set Bit 0 of reg $9003 to 1 to enable 16 pixel tall mode
;We set $9005 to CC - this defines both Screen and Char base as $1000
;First Char we use is 16 at $1100
 
	ldx #16					;We're going to copy 16 registers 
ScreenInitAgain:	
	dex
	lda VicScreenSettings,x	;Get A parameter
	sta $9000,X				;Store to the video registers at $9000
	txa
	bne ScreenInitAgain

;Fill The entire screen with 16 line tiles
;Total screen size 160*192	
	lda #1
	sta z_d				;Color
	
	lda #0				;Start SX
	sta z_b
	
	lda #0				;Start SY
	sta z_c
	
	ldx #20	;Width in tiles
	ldy #12			;Height in tiles (20*12)
	lda #16				;TileStart (First tile it 16)
	jsr FillAreaWithTiles	;Draw the tiles to screen


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	;start=ix,iy... dest hl,de=yoffset 
	loadpair z_hl,userram
	loadpair z_bc,256
	jsr CLDIR0
		
	jsr MainMenu
		

infloop:					;main loop
	lda tick
	clc
	adc #1
	and #%00000001
	sta tick

	loadpair z_bc,300		;slow down delay

	lda boost

	bne boostoff
	loadpair z_bc,100		;boost - no delay 
								;(compensate for font draw)

boostoff:
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
	lda keytimeout

	bne joyskip

	lda #1
	sta boost
	
processkeys:
	loadpair z_ix,playerxacc	;point ix to player accelerations 

	lda z_d
	and #%00000100			;---FLRUD - L
	bne joynotleft
	
	dec playerdirection
	jsr setplayerdirection

	lda #1
	sta keytimeout

joynotleft:
	lda z_d
	and #%00001000			;---FLRUD - R
	bne joynotright
	
	inc playerdirection
	jsr setplayerdirection
	
	lda #1						;ignore keypresses		
	sta keytimeout

joynotright:
	lda z_d
	and #%00010000			;---FLRUD - Fire
	bne joynotfire

	lda boostpower			;check if boost power remains
	beq joynotfire

	lda #0
	sta boost
joynotfire:

joyskip:
	jsr handleplayer		;draw and update player
	jsr handlecpu			;draw and update cpu
	jmp infloop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
FillAreaWithTiles:
	sta z_e			;Backup Tile number
	tya
	pha
						;Calculate screen ram location of tile
		jsr GetVDPScreenPos	
	pla
	tay
FillAreaWithTiles_Yagain:
	tya
	pha
	txa
	pha
		lda z_e			;Tilenum
		ldy #0
FillAreaWithTiles_Xagain:
		sta (z_hl),y	;Transfer Tile to ram
		pha
			lda z_h		;Back up z_H
			pha
				clc
				adc #$84	;Offset to Color Ram
				sta z_h
				lda z_d
				sta (z_hl),y ;Set Tile Color
			pla
			sta z_h		;Restore z_H
			iny
		pla
		clc
		adc #1			;Increase tile number
		dex				;Decrease X counter
		bne FillAreaWithTiles_Xagain
		sta z_e			;Back up Tilenum for next loop
		inc z_c
	pla
	tax
	pla
	tay 
	lda z_l
	adc #20			;Move Down (20 bytes per line)
	sta z_l
	lda z_h
	adc #0			;Add Carry
	sta z_h
	dey				;Decrease Y counter
	bne FillAreaWithTiles_Yagain
	rts
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


	
pset:			;x=ab y=c color=d
	lda z_b
	cmp #250
	bcc PsetOK			;Check we're not drawing offscreen.
	rts
PsetOK:
	lda z_d
		pha
		pushpair z_bc
			loadpair z_hl,PixelMask		
			lda z_b
			and #%00000111
			tay
			lda (z_hl),y	;Get Pixel pos in byte
			pha	
				jsr CalcVramAddr ;Get Vram Address in HL
			pla
			sta z_b			;Pixel Mask
			eor #255
			sta z_c			;Background Mask
			ldy #0
			lda (z_hl),y
			and z_c			;Keep background pixels
			sta z_c
			lda z_d	
			beq PsetSkip1	;Color 0?
			lda z_c
			ora z_b			;Or in foreground pixels
			sta z_c
PsetSkip1:
			lda z_c
			sta (z_hl),y	;Store new pixel
		pullpair z_bc
	;Do background color	
		lda z_b
		and #%11111000		;Convert X to Tile pos
		ror
		ror
		ror
		sta z_b
		
		lda z_c
		and #%11110000		;Convert Y to Tile pos
		ror
		ror
		ror
		ror
		sta z_c
		jsr GetVDPScreenPos	;Get Character Mempos
		
		lda z_h
		clc
		adc #$84	;Offset to Color Ram ($9400)
		sta z_h
		ldy #0
	pla
	sta (z_hl),y
	rts
	
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	align 4
PixelMask:
	db %10000000,%01000000,%00100000,%00010000
	db %00001000,%00000100,%00000010,%00000001

;Get HL address of byte containing BC pixel
CalcVramAddr: 	
	pushpair z_de
		lda #0
		sta z_d
		
		lda z_b			;Xpos
		and #%11111000	;8 pixels per tile	
		asl
		rol z_d
		sta z_l
		
		lda z_d
		adc #$11		;Tile Def Base at $1000 
		sta z_h				;First Tile=16 ($1100)
		
		lda z_c			;Ypos
		and #%00001111	;16 bytes per tile
		adc z_l
		sta z_l
		
		lda #0
		sta z_d
		
		lda z_c
		and #%11110000	;16 bytes per tile 
		asl 			;20 tiles per row
		rol z_d
		asl 
		rol z_d			;*20*16
		sta z_e
		jsr addhl_de
		asl z_e
		rol z_d
		asl z_e
		rol z_d
		jsr addhl_de	
	pullpair z_de
	rts
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
point:		;x=ab y=c 
	loadpair z_hl,PixelMask
	lda z_b
	and #%00000111	
	tay
	lda (z_hl),y	;Get the mask for the pixel we want
	pha
		jsr CalcVramAddr	;Get byte containing our pixel
	pla
	ldy #0
	and (z_hl),y	;Mask our pixel
	beq Point0
	lda #1			;Pixel colored
Point0:
	rts	
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	
GetVDPScreenPos:	; BC=XYpos	
	pushpair z_de
		lda #$10			;Screen base is $1000
		sta z_h
		
		lda z_b				;Xpos
		sta z_l

		lda #0
		sta z_d
		
		lda z_c				;Ypos
		sta z_e
		
		asl z_e
		rol z_d
		asl z_e
		rol z_d
		
		jsr addhl_de
		asl z_e
		rol z_d
		asl z_e
		rol z_d
		
		jsr addhl_de
	pullpair z_de
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
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
cls:
	loadpair z_bc,(20*192)
	loadpair z_hl,$1100		;Tile 0 in VIC Custom Characters
	jmp Cldir0
	 

	;LLLL options
	   ; 0000   ROM   8000  32768
	   ; 0001         8400  33792
	   ; 0010         8800  34816
	   ; 0011         8C00  35840
	   ; 1000   RAM   0000  0000
	   ; 1001         xxxx
	   ; 1010         xxxx  unavail.
	   ; 1011         xxxx
	   ; 1100         1000  4096
	   ; 1101         1400  5120
	   ; 1110         1800  6144
	   ; 1111         1C00  7168		<---
	   
	   
	
VicScreenSettings:
	db $0E		;$9000 - horizontal centering
	db $26		;$9001 - vertical centering
	db $14		;$9002 - set # of columns / 
					;Bit7 = screen base bit ($16 for screen at $1000)
	db %10011001		;$9003 - set # of rows + DoubleSize (Bit 0)
	db $7A		;$9004 - TV raster beam line
	db $CC		;$9005 - bits 0-3 start of character memory /  
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
	db $6+8 	;$900F - Screen and border color register
	
	



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
	