	include "\SrcALL\V1_Header.asm"		;Cartridge/Program header - platform specific
	include "\SrcAll\BasicMacros.asm"		;Basic macros for ASM tasks

	SEI						;Stop interrupts
	jsr ScreenInit			;Init the graphics screen
	jsr Cls					;Clear the screen
	
	lda #0
	sta z_iyl
Again:
	ldx #0
	ldy #0			;Relocate to top of screen
	jsr Locate
	
	loadpair  z_hl,TitleLine		;Show Title line
	jsr PrintString
	jsr Newline

	loadpair z_hl,CRTCParams	;Parameters
	ldx #0
ShowCRTCAgain:
	lda (z_hl,x)
	jsr printchar		;Show +- button
	jsr incHL
	lda (z_hl,x)
	jsr printchar
	jsr inchl
	jsr PrintSpace

	lda (z_hl,x)			;Show reg name
	sta z_e
	jsr inchl
	lda (z_hl,x)			;Show reg name
	sta z_d
	jsr inchl
	pushpair z_hl
		pushpair z_de
		pullpair z_hl
		jsr PrintString
	pullpair z_hl
	
	lda (z_hl,x)			
	sta z_as
	pushpair z_hl
		lda z_as
		jsr ShowDecimal 	; Show Reg Num
	pullpair z_hl
	jsr inchl
	jsr PrintSpace
	lda (z_hl,x)			
	sta z_as
	pushpair z_hl
		lda z_as
		jsr showDecimal	;Show Reg value in DEC
	pullpair z_hl
	jsr PrintSpace
	lda (z_hl,x)			
	jsr showHex			;Show Reg value in HEX
	jsr inchl

	jsr NewLine
	lda (z_hl,x)			
	cmp #255					;Repeat until 255
	beq ShowCRTCDone
	jmp ShowCRTCAgain
ShowCRTCDone:
	
	jsr Newline

	loadpair z_hl,Extra
	jsr PrintString		;Safemode string
	
	jsr waitchar
	sta z_ixl
	cmp #' '
	bne NoSafe
	lda z_iyl		;Toggle safe mode
	eor #255
	sta z_iyl
 NoSafe:
	
	loadpair z_hl,CRTCParams
SetCRTCAgain:
	lda #0
	sta z_ixh
	ldx #0
	lda (z_hl,x)		;See if Down button was pressed
	jsr incHL
	cmp z_ixl	
	bne NotLower
	dec z_ixh
NotLower:

	lda (z_hl,x)		;See if up button was pressed
	jsr inchl
	cmp z_ixl
	bne NotHigher
	inc z_ixh
NotHigher:

	jsr inchl
	jsr inchl
	lda (z_hl,x)		;Get Reg Num
	sta z_c
	jsr inchl
	lda (z_hl,x)		;Get Reg Val
	clc
	adc z_ixh			;Alter Reg Val
	sta (z_hl,x)
	jsr inchl
	jsr SetCRTC	;C=RegNum A=RegVal 
	lda (z_hl,x)
	cmp #255			;Repeat if not 255
	bne SetCRTCAgain

	lda z_iyl		;IYL=0 means safe mode is off
	
	bne DoSafe
	
	jmp Again		
DoSafe:
	loadpair z_bc,$3000		;Wait a while
 PauseAgain:
	jsr decbc
	lda z_b
 	ora z_c
	bne PauseAgain

	loadpair z_hl,SafeParams	;Reset Safe parameters
	ldx #0
SafeParamsAgain:
	lda (z_hl,x)
	sta z_c
	jsr inchl
	lda (z_hl,x)
	jsr inchl
	jsr SetCRTC	;C=RegNum A=RegVal 
	lda (z_hl,x)
	cmp #255
	bne SafeParamsAgain

	 jmp Again
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SetCRTC:
	pha
		lda z_c		;$FE00=CRTC Reg select
		sta $FE00
	pla
	sta $FE01		;$FE01=CRTC Val select
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CRTCParams:			;CRTC Settings 

	db '12'			;-+ Buttons
		dw Htotal	;Label address
		db $00,$7F	;Reg,Val
	db 'QW'
		dw HDisp
		db $01,$50
	db 'AS'
		dw HSyncPos
		db $02,$62
	db 'ZX'
		dw HVSyncWidth
		db $03,$28
	db '45'
		dw VTotal
		db $04,$26
	db 'RT'
		dw VAdjust
		db $05,0
	db 'FG'
		dw VDisp
		db $06,$18
	db 'VB'
		dw VSync
		db $07,$22
	db '78'
		dw IntSkw
		db $08,1
	db 'UI'
		dw MaxAddr
		db $09,7
	db 'JK'
		dw CursorS
		db $A,%00110000
	db 'M,'
		dw CursorE
		db $B,$0
	db '90'
		dw AddrH
		db $C,$08
	db 'OP'
		dw AddrL
		db $D,$30
	db 255			;End of list

SafeParams:
		db $00,$7F	;Reg,Val
		db $01,$50
		db $02,$62
		db $03,$28
		db $04,$26
		db $05,0
		db $06,$18
		db $07,$22
		db $08,1
		db $09,7
		db $0A,%00110000	;10 - Cursor start line and blink type
		db $0B,0			;11 - Cursor end line
		db $0C,$08
		db $0D,$30

	db 255			;End of list

	
					;Label strings
Htotal: 	db "Htotal......",255
HDisp: 		db "HDisp.......",255
HSyncPos: 	db "HSyncPos....",255
HVSyncWidth:db "HVSyncWidth.",255
VTotal:		db "VTotal......",255
VAdjust:	db "VAdjust.....",255
VDisp:		db "VDisp.......",255
VSync:		db "VSync.......",255
IntSkw:		db "IntSkw......",255
MaxAddr:	db "MaxAddr.....",255
CursorS:	db "Cursor S....",255
CursorE:	db "Cursor E....",255
AddrH:		db "AddrH.......",255
AddrL:		db "AddrL.......",255

				;Extra Lines
TitleLine:	db "-+ Name        Reg Val $VL",255
Extra:		db "spc=SafeMode ",255

HelloWorld:
	db "Hello World",255
	
	include "\SrcAll\monitor.asm"			;Debugging tools
	include "\SrcAll\BasicFunctions.asm"	;Basic commands for ASM tasks
	
Bitmapfont:									;Chibiakumas bitmap font
	ifndef BuildVIC
		incbin "\ResALL\Font96.FNT"			;Not used by the VIC due to memory limitations
	endif
	

WaitChar:
		
		jsr GetChar
		cmp #255 
		beq WaitChar
		
        rts

		include "\SrcBBC\BBC_V1_KeyboardDriver.asm"
		
		
GetChar:
	jsr KeyboardScanner_Read
	
	lda #<KeyboardScanner_KeyPresses
	sta z_L
	lda #>KeyboardScanner_KeyPresses
	sta z_H
	
	lda #<HardwareKeyMap
	sta z_E
	lda #>HardwareKeyMap
	sta z_D
	ldy #0
GetCharLineAgain:	
	ldx #8
GetCharBitAgain:	
	lda (z_HL),y
	rol 
	bcc GetCharFound
	sta (z_HL),y
	
	jsr IncDE
	dex
	bne GetCharBitAgain
	;jsr IncHL

	iny
	tya
	cmp #8
	bne GetCharLineAgain
	lda #255
	rts
GetCharFound:
	ldy #0
	lda (z_DE),y	
	rts
	
	
showdecimal:
drawtext_decimal:
	pha
		lda #<$640a
		sta z_l
		lda #>$640a
		sta z_h
	pla
	sta z_b
	cmp z_h
	bcs decthreedigit
	jsr printspace
	cmp z_l
	bcs skipdigit100
	jsr printspace
	jmp skipdigit10
decthreedigit:
	jsr drawtextdecimalsub
skipdigit100:;                                      	ld h,l
	pha
		lda z_l
		sta z_h
	pla
	jsr drawtextdecimalsub
drawtextforcezerocall_plus3:
skipdigit10:
	lda z_b
drawtext_charsprite48:;                                      	add 48
	clc
	adc #48
drawtext_charspriteprotectbc:;                                      	jp printchar; draw char
	sta z_as
	pushpair z_hl
	pushpair z_bc
		lda z_as
		jsr printchar
	pullpair z_bc
	pullpair z_hl
	rts
	
PrintSpace:
	lda #' '
	jmp drawtext_charspriteprotectbc
drawtextdecimalsub:
	lda z_b
	pha
		lda #0
		sta z_c
	pla
drawtext_decimalsubagain:;                                      	cp h
	cmp z_h
	bcc drawtext_decimallessthan
	inc z_c
	sec
	sbc z_h
	jmp drawtext_decimalsubagain
drawtext_decimallessthan:;                                      	ld b,a
	sta z_b
	lda z_c

	jmp drawtext_charsprite48



KeyboardScanner_KeyPresses
        db 16

	

	include "\SrcALL\V1_Functions.asm"		;Basic text to screen functions
	include "\SrcAll\V1_BitmapMemory.asm"	;Bitmap functions for Bitmap screen systems
	include "\SrcAll\V1_VdpMemory.asm"		;VRAM functions for Tilemap Systems
	include "\SrcALL\V1_Palette.asm"		;Palette functions
	include "\SrcALL\V1_Footer.asm"			;Footer for systems that need it
	
	