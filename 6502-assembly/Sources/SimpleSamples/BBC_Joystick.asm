
;Current player pos
PlayerX 	equ $60		
PlayerY 	equ PlayerX+1

;Last player pos (For resetting sprite)
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


RunLocation equ $0200

	ORG RunLocation  ;Actually our code runs at &3000 - but we shift it to here
BBCFirstByte:
	SEI			;Stop interrupts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
;Stop the sound chip making a noise!

	;&43 = Data Dir Reg A
	;&40 = I/O Reg B &40
	;&41 = I/O Reg A &41
	
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
	
	
	
	
	
	lda #$41	;Clear $4180-$8080
	sta z_h
	lda #$80
	sta z_l
	
	ldx #$3F	;Clear $3F00 bytes
	ldy #$00
	lda #0
FillZeros:	
	sta (z_hl),y
	dey
	bne FillZeros
	inc z_h
	dex 
	bne FillZeros
	
	
	
	
	
	lda #4			;Start SX
	sta PlayerX
	lda #80			;Start SY
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
	and #%00000001	;---FLRUD
	beq JoyNotUp	;Jump if UP not presesd
	tya
	sec				;Move Y Up the screen
	sbc #8
	tay
JoyNotUp:
	lda z_h
	and #%00000010	;---FRLDU
	beq JoyNotDown	;Jump if DOWN not presesd
	tya
	clc	 			;Move Y Down the screen
	adc #8
	tay
JoyNotDown:
	lda z_h
	and #%00000100	;---FLRUD
	beq JoyNotLeft	;Move X Left 
	dex
JoyNotLeft:
	lda z_h
	and #%00001000	;---FLRUD
	beq JoyNotRight	;Move X Right
	inx
JoyNotRight:
	
	stx PlayerX		;Update X
	sty PlayerY		;Update Y
	
;X Boundary Check - if we go <0 we will end up back at 255
	cpx #80-1
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
	
	
PauseXY:
	dex
	bne PauseXY
	dey 
	bne PauseXY
	rts

	
	
	
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
	stx z_b
	sty z_c
	jsr GetScreenPos;Get screen pos from XY into Z_DE
	
	ldY #0			;Offset for bytes in this strip
BitmapNextByte:
	lda (z_hl),Y	;Load in a byte from source - offset with Y
	sta (z_de),Y	;Store it in screen ram - offset with Y
	
	inY				;INC the offset
	cpY #8*2		;We draw 8 lines * bitmap width
	bne BitmapNextByte
	rts

Bitmap:
        DB %00000011     ;  0	;Vline 1 (left)
        DB %00000111     ;  1
        DB %00101111     ;  2
        DB %00001111     ;  3
        DB %00001111     ;  4
        DB %00101101     ;  5
        DB %00010110     ;  6
        DB %00000011     ;  7
        DB %00001100     ;  8	;Vline 2 (Right)
        DB %00001110     ;  9
        DB %01001111     ; 10
        DB %00001111     ; 11
        DB %00001111     ; 12
        DB %01001011     ; 13
        DB %10000110     ; 14
        DB %00001100     ; 15
BitmapBlank:	
		ds 16,0
	
	
	

;BBC type is odd - the first 8 screen bytes go DOWN... 
;the 9ths goes back to the top 
;Effectively we're filling in 8x8 character blocks in a zigzag pattern

GetScreenPos:
	lda z_b
	pha
	lda z_c
	pha
		lda #0
		sta z_d
		
		txa			;Xpos
		asl
		rol z_d		;2
		asl 
		rol z_d		;4
		asl 
		rol z_d		;8		;8 bytes per X line
		sta z_e
		
		;We have to work in 8 pixel tall strips on the BBC
		tya			;Ypos
		and #%11111000
		lsr			;$04 00
		lsr			;$02 00
		sta z_b		;Multiply Y strip num by $02				
		clc
		adc z_d		;Add to D
		sta z_d
		lda #0
		ror z_b		;$01 00		
		ror
		ror z_b		;$00 80
		ror
		adc z_e		;Add to E
		sta z_e
		lda z_b		;Add to D
		adc z_d
		sta z_d
	
		lda z_e
		adc #$80	;Screen Offset $4180
		sta z_e
		lda z_d
		adc #$41	;Screen Offset $4180
		sta z_d
	pla 
	sta z_c
	pla 
	sta z_b
	rts

	


Player_ReadControlsDual:;----LRUD
	lda #$F0				;Set port to read (For fire button)
	STA $FE43				;SN76489 - Data Direction
	sta z_h
	
	;lda #%00000000			;Get Channel 0 - Joy 1 LR
	jsr Player_ReadControlsGetData
	lda #%00000001			;Get Channel 1 - Joy 1 UD
	jsr Player_ReadControlsGetData
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
	rol z_h
	clc
	rol z_h
	rts
Player_ReadControlsDualHigh:	;U/L
	clc
	rol z_h
	sec
	rol z_h
	rts

	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
;scr=$3000 when 256x192
;Set screen height to 25, and screen offset so we're using &3
	
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
	db $A0,$B0				;3
	db $E0,$F0				;3
	
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