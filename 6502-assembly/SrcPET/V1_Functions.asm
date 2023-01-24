Locate:	
	sty Cursor_Y
	
	ifdef ScreenWidth32
		pha
			tax
			clc
			adc #4
			sta Cursor_X
		pla
	else
		stx Cursor_X
	endif
	
	
	rts
PrintChar:
	sta z_as
	
	PushAll
	lda z_h
	pha
	lda z_l
	pha
	
		lda #$80		;Screen starts at $8000
		sta z_h
		lda Cursor_X
		sta z_l
		
		lda #0
		sta z_b
		ifdef ModePet80
			lda #80
		else
			lda #40			;40 chars per line
		endif
		sta z_c

		ldy Cursor_y
		beq PrintChar_YZero
PrintChar_Addagain:	
		jsr AddHL_BC
		dey
		bne PrintChar_Addagain
PrintChar_YZero:
	lda z_as
	
	clc
	cmp #64
	bcs PrintChar_Letters
;	sbc #32
	jmp PrintChar_Symbols
PrintChar_Letters:
	sbc #64;
	and #%00011111
PrintChar_Symbols:
;	clc 
;	adc #128
	ldx #0
	sta (z_hl,x)

	inc Cursor_X
	lda Cursor_X
	ifdef ModePet80
		cmp #80
	else
		cmp #40
	endif
	bne PrintChar_NotNextLine
	jsr NewLine
PrintChar_NotNextLine:	
	
	pla
	sta z_l
	pla
	sta z_h	
	PullAll
	rts
NewLine:
	lda #0
	sta Cursor_X	
	inc Cursor_Y
	rts

	
Cls:
		lda #' '
ClsAlt:
		sta z_d
		ldx #0
		ldy #0
		jsr Locate
		ifdef ModePet80
			lda #$08
		else
			lda #$04
		endif
		sta z_b
		lda #$00
		sta z_c
ClsAgain:		
		PushPair z_bc
			lda z_d
			jsr PrintChar
		PullPair z_bc
		jsr DecBC
		lda z_b
		ora z_c
		bne ClsAgain
		
		ldx #0
		ldy #0
		jsr Locate
        rts