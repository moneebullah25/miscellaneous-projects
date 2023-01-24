	include "\SrcAll\BasicMacros.asm"
	
	
z_Regs 		equ $20
sppage equ $0100

UserRam equ $3800		;Game Vars

ScreenWidth20 equ 1		;Screen Size Settings
ScreenWidth equ 160
ScreenHeight equ 102

Color4 equ 4			;16 colors on the Lynx...Woo!
Color3 equ 3
Color2 equ 2
Color1 equ 1



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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	loadpair z_hl,userram
	loadpair z_bc,256
	jsr CLDIR0
		
	jsr MainMenu
		

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	
infloop:
	lda tick				;Update Tick
	clc
	adc #1
	and #%00000001
	sta tick

	loadpair z_bc,1000		;slow down delay

	lda boost
	bne boostoff
	loadpair z_bc,500		;boost - no delay 
								;(compensate for font draw)
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
		sta z_d				;Clear key
keysdown:
	pullpair z_bc
	jsr decbc

	lda z_b
	ora z_c
	bne pausebc				;see if BC=0

startdraw:
	lda keytimeout			;See if Key Timeout is set
	bne joyskip

	lda #1
	sta boost
	
processkeys:
	loadpair z_ix,playerxacc ;point ix to player accelerations 

	lda z_d
	and #%00100000			;UDLR12IO - L
	beq joynotleft
	
	dec playerdirection

	jsr setplayerdirection

	lda #1
	sta keytimeout

joynotleft:
	lda z_d
	and #%00010000			;UDLR12IO - R
	beq joynotright
	
	inc playerdirection
	jsr setplayerdirection
	
	lda #1						;ignore keypresses
	sta keytimeout

joynotright:
	lda z_d
	and #%00001000			;UDLR12IO - Fire
	beq joynotfire

	lda boostpower				;check if boost power remains
	beq joynotfire

	lda #0
	sta boost
joynotfire:
joyskip:
	jsr handleplayer			;draw and update player
	jsr handlecpu				;draw and update cpu
	jmp infloop

	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(b,c) = (x,y)

pset:	;x=ab y=c color=d
	lda z_b
	cmp #250
	bcc PsetOK			;See if drawing offscreen
	rts
PsetOK:
	lda #0
	tax
	lda z_d				;Color number
	pha
		txa
		jsr getpixelmask;get hl=address e=background mask
	pla						; d=pixel mask

	jsr getcolormasknum	;Get color byte 
	and z_e				;mask pixel color
	sta z_e
		
	ldx #0
	lda (z_hl,x)		;Get Current byte
	and z_d				;keep background pixels
	ora z_e				;Set pixel
	sta (z_hl,x)		;Save to screen
	rts



getpixelmask:;returns hl=mempos... 
	lda z_b		;d=mask to keep background e= pixel to select
	pha
		lsr z_b			;2 pixels per byte in mode 0
		jsr getscreenpos
	pla
	and #%00000001
	tay
	loadpair z_bc,pixelbitlookup
	lda (z_bc),y
	sta z_e
	eor #255
	sta z_d
	rts
	
	
getcolormasknum:
	and #%0001111	;Color number
	tay
	loadpairsafe z_bc,colorlookup
	lda (z_bc),y
	rts

	align 2
pixelbitlookup:		;1 nibble per pixel
	db $F0,$0F
colorlookup:		;Color filled bytes
	db $00,$11,$22,$33,$44,$55,$66,$77,$88,$99,$AA,$BB,$CC,$DD,$EE,$FF


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(b,c) = (x,y)


point:
	pushpair z_bc
		jsr getpixelmask
		ldx #0
		lda (z_hl,x)
		and z_e		;mask pixel color
		tax
	pullpair z_bc
	lda z_b
	and #%000001	;See if we're looking at top nible
	bne bytetocolormasknoshift
	txa
	lsr 			;Shift Right one Nibble
	lsr 
	lsr 
	lsr 
	rts
bytetocolormasknoshift:
	txa				;Color from screen
	rts


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		
	

	;Y= $50 bytes per Yline = 00000000 01010000
	;Move Y into top byte 	= YYYYYYYY 00000000
	;Shift Right Twice      = 00YYYYYY YY000000
	;Shift Right Twice      = 0000YYYY YYYY0000
	
GetScreenPos:
	lda #0		
	lsr z_c			;Y is top byte 		= YYYYYYYY 00000000
	ror 
	lsr z_c
	ror 			;Shift Right Twice      = 00YYYYYY YY000000
	tax
		adc z_b
		sta z_l		;Store Low byte in total
	
		lda z_c
		adc #$C0	;Screen base at &C0000
		sta z_h		;Store High byte in total	
	txa
	lsr z_c			;Shift Right Twice      = 0000YYYY YYYY0000
	ror 
	lsr z_c
	ror 
	adc z_l
	sta z_l
	
	lda z_c			;Add High byte to total
	adc z_h
	sta z_h
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

	
cls:
	loadpair z_hl,$C000
	loadpair z_bc, $1FE0
	jmp cldir0				;Clear screen bytes

	

Palette:
	dw $0000; ;0  -GBR
	dw $0FF0; ;2  -GBR
	dw $00FF; ;1  -GBR
	dw $0F00; ;3  -GBR
	dw $0F0F; ;4  -GBR
	
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
