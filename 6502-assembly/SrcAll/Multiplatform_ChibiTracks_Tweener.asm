
tweentone:	;a= %ooooffff	o=offset -8 to +7 f=fraction 0-15
	pha
		lsr
		lsr
		lsr
		lsr
		bit Bit3				;%----SNNN
		beq TweenNotNegative
		ora #%11110000			;Sign extend nibble
TweenNotNegative:
		clc
		adc z_e
		sta z_e
		pushpair z_de
			jsr getchibitone	;e=note (bit 1-6=note  bit0=flat)
			jsr ex_dehlQuick
		pullpair z_de
	pla
	and #%00001111
	beq tweentone_onlyOne
	pha
		pushpair z_hl
			inc z_e
			jsr getchibitone	;e=note (bit 1-6=note  bit0=flat)
		pullpair z_hl
	pla
	jsr tween16hlde
tweentone_onlyOne:	
	jmp ex_dehlQuick			;result in de
	
	
	

GetChibiTone:	;E=Note (Bit 1-6=Note  Bit0=Flat) 
				;Returns DE Frequency  
				;E-F-G-A-B-C-D-... Octive 0-6

	loadpair z_hl,chibiOctave
	ldy #0
	tya			;a=0
	sta z_d
	
	lda z_e
	pha
		and #%11111110	;Clear Sharp/Flat
		sta z_e
		jsr addhl_de

		lda (z_hl),y	;First note
		sta z_e

		iny
		lda (z_hl),y
		sta z_d
	pla
	
	and #%00000001		;Sharp/Flat?
	beq chibitonegotnote	;No? we're done!
	
	lsr z_d				;Halve first value
	ror z_e

	iny			
	lda (z_hl),y
	pha
		iny
		lda (z_hl),y	;Get second value
		sta z_h
	pla
	lsr z_h				;Halve second value
	ror 
	sta z_l
	jsr addhl_de		;add two halves
	jmp ex_dehlQuick
chibitonegotnote:		;Result in DE
	rts
	
	