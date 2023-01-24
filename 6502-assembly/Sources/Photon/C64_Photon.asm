	include "\SrcAll\BasicMacros.asm"

FourColor equ 1


	 
z_Regs 		equ $20
sppage equ $0100
UserRam equ $5000		;Game Vars

ScreenWidth20 equ 1		;Screen Size Settings (4 color mode)
ScreenWidth equ 160
ScreenHeight equ 200

Color4 equ 1			;The c64 has only 4 colors per square
Color3 equ 3
Color2 equ 2
Color1 equ 1


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
	lda #%00000000
	sta $D020		;Border 

		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
		
	loadpair z_hl,userram	;Clear the game ram
	loadpair z_bc,256
	jsr CLDIR0
		
	jsr MainMenu			;Show main menu
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

infloop:					;main loop

	lda tick				;Update Tick
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

		sta keytimeout		;released - nuke key, 
								;and relese keypress
	
		lda #255
		sta z_d
	
keysdown:
	pullpair z_bc
	jsr decbc

	lda z_b
	ora z_c
	bne pausebc				;See if BC>0

startdraw:
	lda keytimeout			;See if Keys have been released
	bne joyskip

	lda #1
	sta boost				;Turn off boost
	
processkeys:
	loadpair z_ix,playerxacc ;point ix to player accelerations 

	lda z_d
	and #%00000100			;---FLRUD - L
	bne joynotleft
	
	dec playerdirection		;Move Left
	jsr setplayerdirection

	lda #1
	sta keytimeout

joynotleft:
	lda z_d
	and #%00001000			;---FLRUD - R
	bne joynotright
	
	inc playerdirection		;Move Right
	jsr setplayerdirection
	
	lda #1					;ignore keypresses		
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
	jsr handleplayer	;draw and update player
	jsr handlecpu		;draw and update cpu
	jmp infloop


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
pset:					;x=ab y=c color=d
	lda z_b
	cmp #250			;Check we're not trying to draw offscreen
	bcc PsetOK
	rts
PsetOK:
	lda z_d				;Color to draw
	pha
		lda #0
		jsr getpixelmask ;get hl=address d=background mask 
							;e=pixel mask
	pla
	jsr getcolormasknum	;Get Color A byte
	and z_e				;mask pixel color
	sta z_e
	
	ldx #0
	lda (z_hl,x)		;Get Current Byte
	and z_d				;Keep background pixels
	ora z_e				;Update pixel to change
	sta (z_hl,x)		;Save New Byte
	rts	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
;(ab,c) = (x,y)
getpixelmask:	 ;returns hl=mempos... d=mask to keep background
	tax				;e= pixel to select
	lda z_b	
	pha
		txa
		lsr
		ror z_b				;4 pixels per byte
		lsr
		ror z_b				;4 pixels per byte
		
		ldx z_b
		ldy z_c
		jsr getscreenpos	;Calc Vram address in HL from BC pos
	pla
	and #%00000011			;4 pixels per byte
	tay
	loadpair z_bc,pixelbitlookup	;Get mask for pixel
	lda (z_bc),y
	sta z_e					;Pixel mask
	eor #255
	sta z_d					;Background Mask
	rts
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Return a byte with all pixels color A

getcolormasknum:
	and #%00000011		;Color number 0-3
	tay
	loadpairsafe z_bc,colorlookup
	lda (z_bc),y		
	rts
	
	align 2
pixelbitlookup:
	db %11000000,%00110000,%00001100,%00000011
colorlookup:
	db %00000000,%01010101,%10101010,%11111111

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;(ab,c) = (x,y)
point:					;Get a color from a pixel
	lda #0
	pushpairsafe z_bc
		jsr getpixelmask
		ldx #0
		lda (z_hl,x)
		and z_e			;mask pixel color
	pullpairsafe z_bc

	jsr bytetocolormask	;fill all pixels with same color

	loadpairsafe z_hl,colorlookup	;look up color 
	ldy #0
pointagain:
	cmp (z_hl),y		;See if color matches LUT
	beq pointdone
	iny
	jmp pointagain
pointdone:
	tya					;Found color
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


	
;fill whole byte with color - eg %00000001 to %00001111
bytetocolormask:;a=color pixel... b=pixel pos (xpos)
	sta z_c
	lda z_b
	and #%00000011		;Get Color
	beq bytetocolormasknoshift
	tax
bytetocolormaskleftshift:
	asl z_c
	asl z_c
	dex
	bne bytetocolormaskleftshift
bytetocolormasknoshift:
	lda z_c				;Fill All Pixels
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
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
cls:
	loadpair z_hl,$6000		
	loadpair z_bc,$2000		
	jsr cldir0			;Clear Screen
	
	loadpair z_hl,$4400		
	loadpair z_bc,$0400		
	lda #$34			;Cyan / Magenta
	jsr cldir			;Clear Colors 1
	
	loadpair z_hl,$D800		
	loadpair z_bc,$03E7
	lda #$05			;Green
	jmp cldir			;Clear Colors 2
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	

;Screen base is $4000 - to move us away from program code
	
;Address= (X * 8) + (Top5BitsOfY * 40) + $2000+$4000
GetScreenPos:
	lda #0
	sta z_b
	sta z_h
	txa				;Multiple X by 8
	asl				;-------- XXXXXXXX
	rol z_h
	asl
	rol z_h
	asl
	rol z_h			;-----XXX XXXXX---
	sta z_l

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
		adc z_l		;Add part to total L
		sta z_l
		lda z_b		;Add part to total H
		adc z_h
		sta z_h
	txa 
	asl
	rol z_b
	asl				;00000000 00101000
	rol z_b			;000YYYYY yyy00000
	
	adc z_l			;Add part to total L
	sta z_l
	lda z_b			;Add part to total H
	adc z_h
	adc #$60		;Screen Base $4000+$2000
	sta z_h
	
	tya
	and #%00000111	;00000000 YYYYYyyy
	clc
	adc z_l
	sta z_l	
	
	
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
	