;Fraction 16... Return between values 0-15

		;16 steps between HL and DE
		
		;A=0 means 100% of HL
		;A=15 means almost 100% of DE

tween16hlde:	;HL= a/16 of de, 16-a/16 of hl  hl----a----de
	beq tween16hlde0
	pha
		pushpairsafe z_hl
				pha
					jsr ex_dehlQuick
				pla
				jsr fraction16	;Fraction of DE
			jsr ex_dehlQuick
		pullpair z_hl
	pla							;First result in DE
	jsr neg		;Negate A
	clc							;Amount of HL to use
	adc #16
	jsr fraction16				;Fraction of HL
	jmp addhl_de				;Result in HL

	
ex_dehlQuick:
	ldx z_d
	lda z_h
	sta z_d
	stx z_h

	ldx z_e
	lda z_l
	sta z_e
	stx z_l
tween16hlde0:
	rts
	

	ifndef neg	
neg:
	eor #255
	clc
	adc #1
	rts
	endif

; ex_dehl:
	; pha
	; txa
	; pha
		; jsr ex_dehlQuick
	; pla
	; tax
	; pla
	; rts
	
	
fraction16_ZeroHL:		;a=0
	ldx #0
	stx z_h
	stx z_l
	rts 
	
fraction16:		;return hl=hl* a/16 (devide by 16, mutlt by a
	beq fraction16_ZeroHL ;return if 0
	
	cmp #16
	bcc fraction16lt16	;Return if >16
	rts 
fraction16lt16:		;Anything else is calculated by Shifting
	sta z_as
	pushpair z_de
		lda z_h
		sta z_d
		lda z_l
		sta z_e
		jsr fraction16_ZeroHL	;Buildup HL=0
		
		lda z_as		;Test bit 3
fraction16_Again:		
		lsr z_d			;DE=DE /2
		ror z_e
		
		bit Bit3
		beq fraction16_NoAdd
		tax
		jsr AddHL_DE	;tested bit=1 so add DE to buildup HL
		txa
fraction16_NoAdd:
		asl 			;Shift bits left 1
		and #%00001111
		bne fraction16_Again
	pullpair z_de
	rts
	
	