;FourColor equ 1

;Current player pos
PlayerX 	equ $60	
PlayerY 	equ PlayerX+1

;Last player pos (For clearing sprite)
PlayerX2 	equ PlayerX+2
PlayerY2 	equ PlayerX+3

	 
z_Regs 		equ $20

z_HL equ z_Regs
z_L  equ z_Regs
z_H  equ z_Regs+1

z_BC equ z_Regs+2
z_C  equ z_Regs+2
z_B  equ z_Regs+3

z_DE equ z_Regs+4
z_E  equ z_Regs+4
z_D  equ z_Regs+5


	


	ifdef BuildA80		;Atari 800 settings
GTIA equ $D000			;GTIA address
PIA  equ $D300			;PIA address
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

	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	Your Code Goes Here		


	
	
	lda #3			;Start SX
	sta PlayerX
	lda #3			;Start SY
	sta PlayerY

	lda #0			;Fake No Keys on first run		
	sta z_h
	jmp StartDraw	;Force Draw of character first run
	
	
infloop:
	jsr Player_ReadControlsDual
	lda z_h
	beq infloop		;See if no keys are pressed
	pha
StartDraw:	
		ldx PlayerX		;Back up X
		stx PlayerX2
		
		ldy PlayerY		;Back up Y
		sty PlayerY2
		
		jsr BlankPlayer	;Remove old player sprite
		
		ldx PlayerX		;Back up X
		ldy PlayerY		;Back up Y
	pla
	sta z_h
	and #%00000001	;---FRLDU
	bne JoyNotUp	;Jump if UP not presesd
	tya
	sec
	sbc #8
	tay
JoyNotUp:
	lda z_h
	and #%00000010	;---FRLDU
	bne JoyNotDown	;Jump if DOWN not presesd
	tya
	clc
	adc #8
	tay
JoyNotDown:
	lda z_h
	and #%00000100	;---FRLDU
	bne JoyNotLeft	;Move X Left 
	dex
JoyNotLeft:
	lda z_h
	and #%00001000	;---FRLDU
	bne JoyNotRight	;Move X Right
	inx
JoyNotRight:

	stx PlayerX		;Update X
	sty PlayerY		;Update Y
	
;X Boundary Check - if we go <0 we will end up back at 255
	cpx #40
	bcs PlayerReset
	
;Y Boundary Check - only need to check 1 byte
	cpy #200-8
	bcs PlayerReset
	
	jmp PlayerPosYOk ;Not Out of bounds
	
PlayerReset:
	ldx PlayerX2	;Reset Xpos	
	stx PlayerX
	
	ldy PlayerY2	;Reset Ypos
	sty PlayerY
	
PlayerPosYOk:
	jsr DrawPlayer	;Draw Player Sprite
	
	ldx #255
	ldy #100
	jsr PauseXY		;Wait a bit!	
		
	jmp infloop
	
BlankPlayer:
	lda #<BitmapBlank		;Source Bitmap Data
	sta z_L
	lda #>BitmapBlank
	sta z_H
	jmp DrawSprite
	
DrawPlayer:
	lda #<Bitmap		;Source Bitmap Data
	sta z_L
	lda #>Bitmap
	sta z_H
	
DrawSprite:
	jsr GetScreenPos	;Calculate Memory address of XY 
	ldY #0			;Line Count
BitmapNextLine:
	ldx #0
	lda (z_hl),Y 	;Copy a byte from the source 
	sta (z_de,x)	;to the destination
		
	clc
	lda z_e
	adc #$28		;Move down a line (40 Bytes / $0028)
	sta z_e
	lda z_d
	adc #$00			
	sta z_d		
	
	iny
	cpy #8			;Height (8 bytes)
	bne BitmapNextLine		
	rts
	
Bitmap:					;Smiley
	ifdef FourColor
		DB %00010100     ;  0
        DB %01010101     ;  1
        DB %01110111     ;  2
        DB %01010101     ;  3
        DB %01010101     ;  4
        DB %01100110     ;  5
        DB %01011001     ;  6
        DB %00010100     ;  7
	else
		DB %00111100     ;  0
        DB %01111110     ;  1
        DB %11011011     ;  2
        DB %11111111     ;  3
        DB %11111111     ;  4
        DB %11011011     ;  5
        DB %01100110     ;  6
        DB %00111100     ;  7
	endif	

BitmapBlank:
        DS 8,0			;Blank Sprite
		
	
	
	;40 bytes per line = * %00000000 00101000
	;We shift our Ypos 3 to the right, add it, then another 2, eg
	
	;%00000000 00101000	=40 (32+8)
	;%YYYYYYYY 00000000
	;%000YYYYY YYY00000	= Y*32
	;%00000YYY YYYYY000 = Y*8
GetScreenPos:
	txa	
	sta z_e			;Store X pos in E
	
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
	lda z_e
	adc #$60
	sta z_e
	lda z_d			;Add Screen Base ($2060)
	adc #$20
	sta z_d		
	
	rts
PauseXY:
	dex
	bne PauseXY
	dey 
	bne PauseXY
	rts
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	Atari 800 Joystick Routine

	ifdef BuildA80	
Player_ReadControlsDual:
		lda PIA+$0	;22221111 - RLDU in player controls
		and #%00001111	;Bottom Nibble is Player 1 Joystick
		sta z_h
	rts
	
	else
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	Atari 5200 - Atari 5200 doesn't have PIA 

Player_ReadControlsDual:
	lda pokey+0			;$E800 - POT0 - game paddle 0
	jsr Player_ReadControlsProcessAnalog
	
	lda pokey+1			;$E801 - POT1 - game paddle 1
	jsr Player_ReadControlsProcessAnalog

	lda z_h
	and #%00001111	;Bottom Nibble is Player 1 Joystick
	sta z_h
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
	rol z_h
	sec
	rol z_h
	rts
Player_ReadControlsProcessLow:		;T/L
	sec
	rol z_h
	clc
	rol z_h
	rts
	endif
		


		
		
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	Display List
  
    org $b000
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
;	Rom Header
        org $bffd
        db $FF         ;Disable Atari Logo
        dw ProgramStart;program Start
		
		
		