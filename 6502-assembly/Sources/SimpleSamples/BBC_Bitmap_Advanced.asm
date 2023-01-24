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
	



	lda #<Bitmap		;Source Bitmap Data
	sta z_L
	lda #>Bitmap
	sta z_H

	ldx #10
	ldy #10
	jsr GetScreenPos	;Get screen pos from XY into Z_DE

	ldx #6				;Height in strips
NexBitmapNextStrip:
	lda z_d
	pha
	lda z_e
	pha
BitmapNextLine:
		ldY #0			;Offset for bytes in this strip
BitmapNextByte:
		lda (z_hl),Y	;Load in a byte from source - offset with Y
		sta (z_de),Y	;Store it in screen ram - offset with Y
		
		inY				;INC the offset
		cpY #12 *8 		;We draw 8 lines * bitmap width (12 bytes)
		bne BitmapNextByte
		clc
		tya				;Add Y to HL
		adc z_l
		sta z_l
		lda z_h
		adc #0
		sta z_h				
	pla ;z_e
	clc
	adc #$80	;Move down one strip (+$0280)
	sta z_e
	pla ;z_d
	adc #$02
	sta z_d	
	dex			;NO of Y-strips in Bitmap 8 rows per strip
	bne NexBitmapNextStrip
	

	jmp *


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
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	

Bitmap:
	incbin "\ResALL\Sprites\RawBBC.RAW"
BitmapEnd:
	
	
	
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