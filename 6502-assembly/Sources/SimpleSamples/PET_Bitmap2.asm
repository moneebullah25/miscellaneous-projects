z_HL equ $20
	
z_L equ $20
z_H equ $21

; 10 SYS (1025)
*=$0401
        BYTE    $0E, $04, $0A, $00, $9E, $20, $28,  $31, $30, $34, $30, $29, $00, $00, $00

	sei

	lda #14
	sta $E84C	;Enable Graphics on later PETs (12=Gra 14=Lower)

	
	lda #0
	sta z_L		;load Zeropage z_HL with $8000
	lda #$80
	sta z_H
	
	ldy #0
	lda #0
CharAgain:	
	
	jsr PrintChar
	
	iny
	cpy #16
	beq NewLine
NewLineD:	
	clc
	adc #1
	bne CharAgain

infloop:
	jmp infloop

NewLine:
	pha
		lda #32
		ldy #24
NewLineA:		
		jsr PrintChar
		dey
		bne NewLineA
		ldy #0
	pla

	jmp NewLineD
	
	
PrintChar:	;Upper+LowerCase Font
	pha
		ldx #0
		Sta (z_HL,x)
		clc
		lda z_l
		adc #1
		sta z_l
		lda z_h
		adc #0
		sta z_h
	pla
	rts
		
	
HelloWorld:				;Test string to show
	db "Hello World",255
	
	
	
	