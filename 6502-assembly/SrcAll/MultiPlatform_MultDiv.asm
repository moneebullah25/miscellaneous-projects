
  

DivInfinite:
	loadpair z_hl,$FFFF
	 rts
	 
	 
Div8ZeroL:			;Div H0 / A
	tax
		lda #0
		sta z_l
	txa
	
Div8:
Div16:				;Divide HL/A ... Remainder in A
	beq DivInfinite	
Div8NoZeroCheck:		
	sta z_e
	lda z_h
	
	ldx z_l
	stx z_as
	
	ldx #0
	stx z_h
	stx z_l
			
	jsr DoDivPart	
	pha
		lda z_as
	
		jsr DoDivPart	
		ldx z_l
		sta z_l
	pla
	sta z_h
	txa
    rts
	
DoDivPart:
	ldx #8;ld b,8
	clc
Div16_Loop1:
	rol ;rla
	tay
		rol z_L
		rol z_H
		bcs Div16_FlipCarry
		sec		
Div16_FlipCarry
		lda z_l		;Subract E from L
		sbc z_E
		sta z_l
		bcs Div16_NoAdd1	;NoCarry
		dec z_h		;Subtract D from H (with any carry)
		bcs Div16_NoAdd1 ;jr nc,Div16_NoAdd1
		clc
		lda z_l			;Add E to L
		adc z_e
		sta z_l
		bcc Div16_NoAdd1
			
		inc z_h
		clc
Div16_NoAdd1:
	tya
	dex 
	bne Div16_Loop1
    rol 
	rts
	