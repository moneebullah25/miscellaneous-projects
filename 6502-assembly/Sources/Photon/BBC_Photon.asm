	include "\SrcAll\BasicMacros.asm"
	
	
z_Regs 		equ $20
sppage equ $0100

UserRam equ $3800		;Game Vars

ScreenWidth40 equ 1		;Screen Size Settings
ScreenWidth equ 320
ScreenHeight equ 192

Color4 equ 1			;The BBC has only 3 colors
Color3 equ 3
Color2 equ 2
Color1 equ 1

RunLocation equ $0200

	ORG RunLocation  ;Actually our code runs at $3000 - but we shift it to here
BBCFirstByte:
	SEI			;Stop interrupts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
;Stop the sound chip making a noise!

	;$43 = Data Dir Reg A
	;$40 = I/O Reg B $40
	;$41 = I/O Reg A $41
	
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

	loadpair z_hl,userram	;Clear Game Ram
	loadpair z_bc,256
	jsr CLDIR0
		
	jsr MainMenu			;Main Menu
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Main loop

infloop:
	lda tick			;Update Tick
	clc
	adc #1
	and #%00000001
	sta tick

	loadpair z_bc,5		;slow down delay

	lda boost

	bne boostoff
	loadpair z_bc,1		;boost - no delay 
						;(compensate for font draw)
boostoff:

	lda #255
	sta z_d				;key buffer	
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
	lda keytimeout				;See if Keytimeout is set
	bne joyskip	

	lda #1
	sta boost					;Boost Off
	
processkeys:
	loadpair z_ix,playerxacc;point ix to player accelerations 

	lda z_d
	and #%00000100	;---FLRUD - L
	bne joynotleft
	
	dec playerdirection			;Move Left
	jsr setplayerdirection

	lda #1
	sta keytimeout

joynotleft:
	lda z_d
	and #%00001000	;---FLRUD - R
	bne joynotright
	
	inc playerdirection			;Move Right
	jsr setplayerdirection
	
	lda #1						;ignore keypresses		
	sta keytimeout

joynotright:
	lda z_d
	and #%00010000	;---FLRUD	-;	Fire
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
	loadpair z_hl,$4180		;Screen Offset $4180
	loadpair z_bc,(80*200)	;Screen Offset $4180
	jmp cldir0				;Clear screen bytes
	

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
	lda #$F0				;Set port to read (For fire button)
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
	

;(ab,c) = (x,y)		

pset:			;x=a+z_b y=z_c color=z_d
	tax
	lda z_d
	pha
		txa
		jsr getpixelmask;get hl=address 
	pla					;e=background mask d=pixel mask

	jsr getcolormasknum
	and z_e				;mask pixel color

	sta z_e
	ldx #0
	lda (z_hl,x)
	and z_d				;keep background pixels
	ora z_e
	sta (z_hl,x)
	rts


	
;(ab,c) = (x,y)		
getpixelmask:		 ;returns hl=mempos...
	tax				;d=mask to keep background - e= pixel to select
	lda z_b
	pha
		txa
		lsr
		ror z_b			;2 pixels per byte in mode 0
		lsr
		ror z_b			;2 pixels per byte in mode 0
		jsr getscreenpos
	pla
	and #%00000011
	tay
	loadpair z_bc,pixelbitlookup
	lda (z_bc),y
	sta z_e
	eor #255
	sta z_d
	rts
	
;(ab,c) = (x,y)		
getcolormasknum:	;Get a byte with all pixels the same color
	and #%00000011
	tay
	loadpairsafe z_bc,colorlookup
	lda (z_bc),y
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	
;BBC type is odd - the first 8 screen bytes go DOWN... 
;the 9ths goes back to the top 
;Effectively we're filling in 8x8 character blocks in a zigzag pattern

GetScreenPos:
		lda #0
		sta z_h
		
		lda z_b		;Xpos
		asl
		rol z_h		;2
		asl 
		rol z_h		;4
		asl 
		rol z_h		;8		;8 bytes per X line
		sta z_l
		
		;We have to work in 8 pixel tall strips on the BBC
		lda z_c		;Ypos
		and #%11111000
		lsr			;$04 00
		lsr			;$02 00
		sta z_b		;Multiply Y strip num by $02				
		clc
		adc z_h		;Add to D
		sta z_h
		lda #0
		ror z_b		;$01 00		
		ror
		ror z_b		;$00 80
		ror
		adc z_l		;Add to E
		sta z_l
		lda z_b		;Add to D
		adc z_h
		sta z_h
	
		lda z_c		;Bottom 3 bits of Ypos
		and #%00000111
		adc z_l
		adc #$80	;Screen Offset $4180
		sta z_l
		lda z_h
		adc #$41	;Screen Offset $4180
		sta z_h
	rts
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

point:	;x=a+z_b y=z_c color=z_d
	pushpairsafe z_bc
		jsr getpixelmask
		ldx #0
		lda (z_hl,x)
		and z_e		;mask pixel color
	pullpairsafe z_bc

	jsr bytetocolormask	;fill all pixels with same color

	loadpairsafe z_hl,colorlookup	;look up color 
	ldy #0
pointagain:
	cmp (z_hl),y		;See if this byte matches screen one
	beq pointdone
	iny
	jmp pointagain
pointdone:
	tya					;Yes? Then Y=Color number
	rts
	

;fill whole byte with color - eg %00000001 to %00001111
bytetocolormask:		;a=color pixel... b=pixel pos (xpos)
	sta z_c
	lda z_b
	and #%00000011
	beq bytetocolormasknoshift
	tax
bytetocolormaskleftshift:
	asl z_c
	dex
	bne bytetocolormaskleftshift
bytetocolormasknoshift:
	lda z_c
	ror
	ora z_c
	ror	
	ora z_c
	ror
	ora z_c
	rts
	
	align 2
pixelbitlookup:
	db %10001000,%01000100,%00100010,%00010001
colorlookup:
	db %00000000,%11110000,%00001111,%11111111

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
	
	
;scr=$3000 when 256x192
;Set screen height to 25, and screen offset so we're using $3
	
CRTCConfig:
	db $7F		;0 - Horizontal total
	db $50		;1 - Horizontal displayed characters
	db $62		;2 - Horizontal sync position
	db $28		;3 - Horizontal sync width/Vertical sync time
	db $26		;4 - Vertical total
	db $00		;5 - Vertical total adjust
	db $18		;6 - Vertical displayed characters (25)
	db $22		;7 - Vertical sync position
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
	db $81,$91			;2
	db $C1,$D1			;2
Palette3:
	db $A5,$B5				;3
	db $E5,$F5				;3
	
;EOR True   Color
;7  (0) 	black
;6  (1) 	red
;5  (2) 	green
;4  (3) 	yellow (green—red)
;3  (4) 	blue
;2  (5) 	magenta (red—blue)
;1  (6) 	cyan (green—blue)
;0  (7) 	white

	
BBCLastByte: db 0







