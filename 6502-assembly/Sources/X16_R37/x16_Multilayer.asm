z_Regs equ $20
	include "\srcAll\BasicMacros.asm"

ZPADDR EQU $50			;TEMP VALUE TO KEEP TRACK OF ON/OFF STATE OF LAYER 2

	macro PushPair,ra	;Push a pair onto the stack (eg PushPair z_HL)
		lda \ra			
		pha				;Push lower Zpage entry
		lda \ra+1
		pha
	endm				;Push higher Zpage entry
	
	macro PullPair,ra	;Pull a pair onto the stack (eg PullPair z_HL)
		pla
		sta \ra+1		;Pull lower Zpage entry
		pla
		sta \ra			;Pull higher Zpage entry
	endm


SPpage equ $0100

	
V_L  equ $9f20			;VERA_ADDR_LO  LLLLLLLL	20 Bit address L
V_M  equ $9f21			;VERA_ADDR_MID MMMMMMMM	20 Bit address M
V_H  equ $9f22			;VERA_ADDR_HI  IIIIHHHH	20 Bit address H  I=Increment
V_D1 equ $9f23			;VERA_DATA1    DDDDDDDD	Data Port 1

VERA_CTRL equ $9F25 ;$9F25 R------A   R=Reset A=Address (port 1 / 2)

;NEW VERA CONTROL REGISTERS (X16 EMU R37)
DC_VIDEO		EQU $9F29		;VERA DC_VIDEO REGISTER (BITS=FS10-CMM	F=CURRENT FIELD, S=SPRITES ENABLE, 1=LAYER1 ENABLE, 0=LAYER0 ENABLE, C=CHROMA DISABLE, MM=OUTPUT MODE (01=VGA) )
DC_HSCALE		EQU $9F2A
DC_VSCALE		EQU $9F2B
L0_CONFIG		EQU $9F2D		;LAYER 0 CONFIG (BITS=HHWWTBCC		H=MAP HEIGHT W=MAP WIDTH T=T256C B=BITMAP MODE (1=BITMAP MODE, 0=TILE MODE), C=COLOR DEPTH (0=1BPP, 1=2BPP, 2=4BPP, 3=8BPP))
L0_MAPBASE		EQU $9F2E
L0_TILEBASE		EQU $9F2F
L0_HSCROLL_L	EQU $9F30
L0_HSCROLL_H	EQU $9F31		;USED IN BITMAP MODE , H-SCROLL (11:8) IS USED TO SPECIFY PALLET OFFSET FOR THE BITMAP
L0_VSCROLL_L	EQU $9F32
L0_VSCROLL_H	EQU $9F33

L1_CONFIG		EQU $9F34		;LAYER 1 CONFIG (BITS=HHWWTBCC		H=MAP HEIGHT W=MAP WIDTH T=T256C B=BITMAP MODE (1=BITMAP MODE, 0=TILE MODE), C=COLOR DEPTH (0=1BPP, 1=2BPP, 2=4BPP, 3=8BPP))
L1_MAPBASE		EQU $9F35
L1_TILEBASE		EQU $9F36
L1_HSCROLL_L	EQU $9F37
L1_HSCROLL_H	EQU $9F38		;USED IN BITMAP MODE , H-SCROLL (11:8) IS USED TO SPECIFY PALLET OFFSET FOR THE BITMAP
L1_VSCROLL_L	EQU $9F39
L1_VSCROLL_H	EQU $9F3A

*=$0801
        BYTE    $0E, $08, $0A, $00, $9E, $20, $28,  $32, $30, $36, $34, $29, $00, $00, $00  ;Start at $0810
*=$0810
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;Screen Setup
	
	     ; ; ;R------A		R=Reset A=Address (port 1 / 2)
	; ; lda #%00000000
	; ; sta VERA_CTRL  		;$9F25 - set ADDR1 active
	
		 ;R-----DA			R=RESET, D=DCSEL, A=ADDRSEL
	LDA #%00000000
	STA VERA_CTRL
	
	; ; SetVeraAddr $1F0000 ;$F0000 address with inc DC_VIDEO 
	
		 ; ; ;F----COO		F=current field (RO) 
	; ; lda #%00000001		;C=Chroma disable O=Out mode (1=vga)	 
	; ; sta V_D1 			;$40040	DC_VIDEO 
	
		 ;FS10-CMM		F=CURRENT FIELD, S=SPRITES ENABLE, 1=LAYER1 ENABLE, 0=LAYER0 ENABLE, C=CHROMA DISABLE, MM=OUTPUT MODE (01=VGA)
	LDA #%01110001
	STA DC_VIDEO

	; ; lda #64				; Hscale - 128=Normal 64=2x
	; ; sta V_D1 			;$40041	DC_HSCALE
	
	; ; lda #64				; Vscale - 128=Normal 64=2x
	; ; sta V_D1 			;$40042	DC_VSCALE
	LDA #64
	STA DC_HSCALE
	LDA #64
	STA DC_VSCALE
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;Palette Setup
	
	; ; lda #$1F
	; ; ldy #$10
	; ; ldx #0
	; ; jsr SetVeraHML		;$40200	Color 0 Green/Blue
	lda #$11
	ldy #$FA
	ldx #0
	jsr SetVeraHML		;$40200	Color 0 Green/Blue

	ldx #2				;2 bytes x 256 entries ($GB $-R )
	ldy #0
	lda #<palette
	sta z_l
	lda #>palette
	sta z_h
	
PaletteLoop:	
	lda (z_hl),y		
	sta V_D1		
	iny
	bne PaletteLoop
	inc z_h
	dex
	bne PaletteLoop
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;Layer Setup
	
	; ; lda #$1F
	; ; ldy #$20
	; ; ldx #$00
	; ; jsr SetVeraHML		;$0F 20 00	L1_CTRL0 (Autoinc $01)
	
		 ; ; ;MMM----E M=Mode E=Enable
	; ; lda #%11000001
	; ; sta V_D1		;$40000	L1_CTRL0		MMM----E	(M=Mode E=Enable)
	; ; stz V_D1		;$40001	L1_CTRL1		--HWhhww	H=tile Height / W=tile Width / m=map height / w=map width
	; ; stz V_D1		;$40002	L1_MAP_BASE_L 	LLLLLLLL	Map Base (9:2)
	; ; stz V_D1		;$40003	L1_MAP_BASE_H	HHHHHHHH	Map Base (17:10)
	; ; stz V_D1		;$40004	L1_TILE_BASE_L 	LLLLLLLL	Tile Base (9:2)
	; ; stz V_D1		;$40005	L1_TILE_BASE_H  HHHHHHHH	Tile Base (17:10)
	; ; stz V_D1		;$40006	L1_HSCROLL_L	LLLLLLLL	Hscroll (7:0)
	; ; stz V_D1		;$40007	L1_HSCROLL_H 	----HHHH	Vscroll (11:8)
	; ; stz V_D1		;$40008	L1_VSCROLL_L  	LLLLLLLL	Hscroll (7:0)
	; ; stz V_D1		;$40009	L1_VSCROLL_H 	----HHHH	Vscroll (11:8)
	
	     ;HHWWTBCC		H=MAP HEIGHT W=MAP WIDTH T=T256C B=BITMAP MODE (1=BITMAP MODE, 0=TILE MODE), C=COLOR DEPTH (0=1BPP, 1=2BPP, 2=4BPP, 3=8BPP)
	LDA #%00000110
	STA L0_CONFIG
	
	STZ L0_MAPBASE
	STZ L0_TILEBASE
	STZ L0_HSCROLL_L
	STZ L0_HSCROLL_H
	STZ L0_VSCROLL_L
	STZ L0_VSCROLL_H
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;Layer 2 Setup
	
	; ; lda #$1F
	; ; ldy #$30
	; ; ldx #$00
	; ; jsr SetVeraHML		;$04 00 10	L1_CTRL0 (Autoinc $01)
	
		 ; ; ;MMM----E M=Mode E=Enable (mode 6=4bpp)
	; ; lda #%11000001
	; ; sta V_D1		;$40000	L1_CTRL0		MMM----E	(M=Mode E=Enable)
	; ; stz V_D1		;$40001	L1_CTRL1		--HWhhww	H=tile Height / W=tile Width / m=map height / w=map width
	; ; stz V_D1		;$40002	L1_MAP_BASE_L 	LLLLLLLL	Map Base (9:2)
	; ; stz V_D1		;$40003	L1_MAP_BASE_H	HHHHHHHH	Map Base (17:10)
	
	; ; ;Layer at $00A000-$013600
	; ; lda #$00
	; ; sta V_D1		;$40004	L1_TILE_BASE_L 	LLLLLLLL	Tile Base (9:2)
	; ; lda #%00101000
	; ; sta V_D1		;$40005	L1_TILE_BASE_H  HHHHHHHH	Tile Base (17:10)
	
	; ; stz V_D1		;$40006	L1_HSCROLL_L	LLLLLLLL	Hscroll (7:0)
	; ; stz V_D1		;$40007	L1_HSCROLL_H 	----HHHH	Vscroll (11:8)
	; ; stz V_D1		;$40008	L1_VSCROLL_L  	LLLLLLLL	Hscroll (7:0)
	; ; stz V_D1		;$40009	L1_VSCROLL_H 	----HHHH	Vscroll (11:8)
		
	     ;HHWWTBCC		H=MAP HEIGHT W=MAP WIDTH T=T256C B=BITMAP MODE (1=BITMAP MODE, 0=TILE MODE), C=COLOR DEPTH (0=1BPP, 1=2BPP, 2=4BPP, 3=8BPP)
	LDA #%00000110
	STA L1_CONFIG
	
	STZ L1_MAPBASE
	
		 ;BBBBBBHW		B=TILE BASE ADDRESS, H=Tile Height (0:8pixels, 1:16 pixels), W=Tile Width
	LDA #%01010000
	STA L1_TILEBASE
	
	STZ L1_HSCROLL_L
	STZ L1_HSCROLL_H
	STZ L1_VSCROLL_L
	STZ L1_VSCROLL_H

	
	; ; ;MMM----E M=Mode E=Enable
	; ; lda #%11100000
	; ; sta V_D1		;Disable Layer 2
	
	
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;Test Pattern Loop

	lda #$10			
	ldy #$00
	ldx #$00
	jsr SetVeraHML		;Bitmap ram $-0 00 00 (Autoinc $01)
	

	;160x240=$9600
	ldy #$96+1
	lda #$00+1
TestPatternLoop:	
	stz V_D1			;Write test byte to screen
	sec
	sbc #1
	bne TestPatternLoop
	dey
	bne TestPatternLoop
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		
	;Clear Layer 2
	lda #$10
	ldy #$A0
	ldx #$00
	jsr SetVeraHML		;Bitmap ram $-0 00 00 (Autoinc $01)
	

	ldy #$96+1		;160x240=$9600
	lda #$00+1
TestPatternLoop2:	
	stz V_D1			;Write test byte to screen
	sec
	sbc #1
	bne TestPatternLoop2
	dey
	bne TestPatternLoop2
	
	
	lda #>SpriteTest	;Address of bitmap in RAM
	sta z_h
	lda #<SpriteTest	; (48x48 Chibiko BMP)
	sta z_l
	
	lda #$10			;TopLeft of Layer 2
	ldy #$A0
	ldx #$00
	jsr SetVeraHML		;Bitmap ram $-0 A0 00 (Autoinc $01)
	
	
	
	ldY #48				;Height of bitmap 
BmpAgainY:
	ldx #24				;Width of Bitmap (24 bytes)
BmpAgainX:
	lda (z_hl)			;Read a byte
	sta v_D1			;Copy a byte
	jsr incHL			;increase z_hl	
	dex 
	bne BmpAgainX		;Loop for rest of line
	phy
		ldy #>(160-24)
		ldx #<(160-24)
		jsr AddVHML_YX	;Move down screen line (160 byte)
	ply
	dey
	bne BmpAgainY		;Loop for next line
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	lda #<MyText
	sta z_L
	lda #>MyText
	sta z_H
	jsr PrintString
	jsr newline
	jsr PrintString
	
	jsr newline
	jsr monitor		;Dump Registers to screen
	
    jsr MemDump
    word $CEED      ;Address
    byte $10          ;Lines
	
	LDA #1
	STA ZPADDR			;SET ZPADDR TO 1 TO STORE THAT LAYER 2 IS ON.
	
	jsr longpause
	
	; ; lda #$0F			;L2_BM_PAL_OFFS
	; ; ldy #$30
	; ; ldx #$17
	; ; jsr SetVeraHML		;Bitmap ram $-0 00 00 (Autoinc $01)


  	    ; ----BBBB		;(In mode 567) Bmp palette offset
	lda #%00000001	
	; ; sta v_d1			;Change 4bpp pixel to palete $1x
	sta L1_HSCROLL_H
	
	jsr longpause			; (+16 to color)

	
	; ; lda #$0F			;L2_CTRL0
	; ; ldy #$30
	; ; ldx #$10
	; ; jsr SetVeraHML		;Bitmap ram $-0 00 00 
							;(Autoinc $01)
ToggleFlash:
	; ; lda v_d1
	; ; ;	  MMM----E		(M=Mode E=Enable)
	; ; eor #%00000001)		;Toggle layer on/off
	; ; sta v_d1			
	
	; ENABLING / DISABLING OF LAYER 2 HAS TO BE DONE THROUGH DC_VIDEO NOW:
	LDA ZPADDR
	CMP #0
	beq ToggleFlashOn
		 ;FS10-CMM		F=CURRENT FIELD, S=SPRITES ENABLE, 1=LAYER1 ENABLE, 0=LAYER0 ENABLE, C=CHROMA DISABLE, MM=OUTPUT MODE (01=VGA)
	LDA #%01010001
	STA DC_VIDEO
	stz ZPADDR			;STORE THAT LAYER 2 IS NOW OFF
	bra ToggleFlashEnd
ToggleFlashOn:
		 ;FS10-CMM		F=CURRENT FIELD, S=SPRITES ENABLE, 1=LAYER1 ENABLE, 0=LAYER0 ENABLE, C=CHROMA DISABLE, MM=OUTPUT MODE (01=VGA)
	LDA #%01110001
	STA DC_VIDEO
	LDA #1
	STA ZPADDR			;STORE THAT LAYER 2 IS NOW ON
ToggleFlashEnd:
	
	jsr dopause
	jmp ToggleFlash		;Repeat
	
	
LongPause:
	jsr dopause
	jsr dopause
	jsr dopause	
	jsr dopause
	jsr dopause
	jsr dopause
	jsr dopause
	jsr dopause
	jsr dopause
	jsr dopause
	rts	
	
dopause:
	ldx #255
dopauseX:	
	ldy #255
dopauseY:
	nop 
	nop 
	nop 
	nop 
	nop 
	nop 
	nop 
	nop 
	
	dey
	bne dopauseY
	dex 
	bne dopauseX
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		
		
SetVeraHML:		;Select Memory address %----AAAA YYYYYYYYY XXXXXXXX
				;Set Inc to            %AAAA----
						
		sta V_H	
		sty V_M
		stx V_L
	rts


AddVHML_YX		;add 24 bit %00000000 YYYYYYYYY XXXXXXXX to V_HML
		lda #0
AddVHML_AYX		;add 24 bit %AAAAAAAA YYYYYYYYY XXXXXXXX to V_HML
		pha
			clc
			txa
			adc V_L
			sta V_L
			tya
			adc V_M
			sta V_M
		pla
		adc V_H
		sta V_H
		rts	



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		
		
CursorX: db 0
CursorY: db 0

NewLine:		
		stz CursorX
		inc CursorY
	rts
	
PrintChar:
	sec
	sbc #32					;Our charactrer has no space
	sta z_as
	
	phx						;Backup the registers we use
	phy
	lda z_h
	pha
	lda z_l
	pha
		lda z_as			;8 bytes per character in our font
		stz z_H
		asl
		rol	z_H
		asl 
		rol	z_H
		asl 
		rol	z_H
		
		adc #<BitmapFont	;Add the address of our font
		sta z_L
		lda z_H
		adc #>BitmapFont
		sta z_H	
	
	
		stz z_d				;For the 24 bits we'll use z_d  z_e and A
			
		lda CursorY	 ;CursorY* 160*8 = Cursor Y * %00000000 00000101 00000000
		;asl					
		;rol z_d
		sta z_e				;Shift one bit left and store
		
		asl
		rol z_d
		asl
		rol z_d				;Shift 2 bits left
		
		clc
		adc z_e				;Add carry to M byte
		tay
		
		lda z_d				
		adc #0				;Add carry to H byte
		sta z_d
		
		stz z_e
		lda CursorX	;CursorX* 4 = Cursor Y * %00000000 00000000 00000100
		
		;asl
		;rol z_e
		asl
		rol z_e
		asl
		rol z_e				;Shift 3 bits left
		
		tax					;X contains L byte 
		
		tya 				;Add carry to M byte
		adc z_e				
		tay					;Y contains M byte
		
		lda z_d				;Add carry to H byte & set AutoInc
		adc #$10			;A contains H byte  
		
		jsr SetVeraHML		;Set Screen Address
		
		
		ldy #0						;Line in Font Char
CharLineAgain:
		phy
			lda (z_hl),y
			sta z_as
			ldx #4				;4 Bytes (2xpixels) Per Line
CharPixelAgain:				
			lda #0
			rol z_as			;Pop a bit off our font byte
			rol
			rol
			rol
			rol
			rol z_as			;Pop a bit off our font byte
			rol
			sta V_D1			;Send to screen ram
			
			dex 
			bne CharPixelAgain	;Next Pixel
			
			ldy #>(160-4)
			ldx #<(160-4)
			jsr AddVHML_YX			;Move down screen line (160 byte)
		ply
		iny							;Move down a line in our font
		tya
		cmp #8
		bne CharLineAgain			;8 lines per character
		
		inc CursorX
		lda CursorX
		cmp #40						;40 Characters per screen line
		bne NoNewLine
		jsr NewLine
NoNewLine:	
	pla								;Restore the registers
	sta z_l
	pla
	sta z_h	
	ply
	plx
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SpriteTest:
	incbin "\ResAll\Sprites\RawMSX.RAW"
	
		
BitmapFont:
	incbin "\resall\Font96.FNT"

MyText
    db "Hello worlds!",  255
	
	include "\SrcAll\monitor.asm"
	include "\SrcAll\BasicFunctions.asm"
	
	
palette:
	;	-RGB
	dw $0008	;0
	dw $0FF0	;1
	dw $00FF	;2
	dw $0F00	;3
	dw $0001	;4
	dw $0002	;5
	dw $0003	;6
	dw $0004	;7
	dw $0000	;8	
	dw $0000	;9
	dw $0000	;10
	dw $0000	;11
	dw $0000	;12
	dw $0000	;13
	dw $0000	;14
	dw $0000	;15
	dw $0000	;16
	dw $0808
	dw $00FF
	dw $0FFF
	dw $0008
	dw $0009
	dw $000A
	dw $000B
	dw $000C
	dw $000D
	dw $000E
	dw $000F
	dw $0010
	dw $0020
	dw $0030
	dw $0040
	dw $0050
	dw $0060
	dw $0070
	dw $0080
	dw $0090
	dw $00A0
	dw $00B0
	dw $00C0
	dw $00D0
	dw $00E0
	dw $00F0
	dw $0100
	dw $0200
	dw $0300
	dw $0400
	dw $0500
	dw $0600
	dw $0700
	dw $0800
	dw $0900
	dw $0A00
	dw $0B00
	dw $0C00
	dw $0D00
	dw $0E00
	dw $0F00
	dw $0111
	dw $0222
	dw $0333
	dw $0444
	dw $0555
	dw $0666
	dw $0777
	dw $0888
	dw $0999
	dw $0AAA
	dw $0BBB
	dw $0CCC
	dw $0DDD
	dw $0EEE
	dw $0FFF
	
	
