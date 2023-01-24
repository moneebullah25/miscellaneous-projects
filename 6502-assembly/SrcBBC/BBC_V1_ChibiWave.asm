; Z_HL = Source Data / z_DE = Length / X=Speed / A=Bitrate (1/2/3 = 1/2/4 bit)
ChibiWave:
		sta z_c
		stx z_b
		
		lda #8						;1 Bit settings
		ldx #>(Do1BitWav-1)
		ldy #<(Do1BitWav-1)
		dec z_c
		beq ChibiWaveBitSet		
		lda #4						;2 Bit settings
		ldx #>(Do2BitWav-1)
		ldy #<(Do2BitWav-1)
		dec z_c
		beq ChibiWaveBitSet
		lda #2						;4 Bit settings
		ldx #>(Do4BitWav-1)
		ldy #<(Do4BitWav-1)
 ChibiWaveBitSet:
		; ;Selfmod the settings in for the settings
		; ld (PlayWaveCallSelfMod_Plus2-2),hl
	; pop hl
	stx z_iyh
	sty z_iyl
	sta z_c

	;lda #%10011111			;Set Volume %1cc1VVVV
	;sta $FE41
	;lda #%10111111			;Set Volume %1cc1VVVV
	;sta $FE41
	;lda #%11111111			;Set Volume %1cc1VVVV
	;sta $FE41
	
	;We're using Channel 2 for our speech
	
	lda #%11000000			; Low Tone L
	sta $FE41
	lda #%00000000			; Low Tone H
	sta $FE41

	; Sending a value
	ldy #0
Waveagain:
	lda (z_hl),Y			;Load in sample
	sta z_ixh				;Store read in byte 
	
	iny
	bne WaveagainYok
	inc z_h					;INC top byte of address
WaveagainYok:
	
	lda z_c					;Samples per byte
	sta z_ixl

WaveNextBit:
	lda #0					;Shift first bit into D
	rol z_ixh
	rol 

	jsr CallIY				;Call correct wave function 
	ldx z_b
Wavedelay:					;Wait for next sample
	dex
	bne Wavedelay
	
	dec z_ixl
	bne WaveNextBit			;Process any remaining bits
	

	lda z_E					;Dec DE - repeat if not done.
	beq ChibiWaveDecDE_D
ChibiWaveDecDE_E:	
	DEC z_E
	jmp Waveagain
ChibiWaveDecDE_D:	
	DEC z_D
	bne ChibiWaveDecDE_E
	rts
	
CallIY:
	tax
	lda z_iyh
	pha
	lda z_iyl
	pha
	txa
	rts
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
Do4BitWav:
	rol z_ixh							;Shift 4 bits in
	rol 
	rol z_ixh
	rol 
	rol z_ixh
	rol 
Do1BitWavc:
	ora #%11010000			;Set Volume %1101VVVV
	sta $FE41
	rts
Do2BitWav:
	rol z_ixh							;Shift 2 bits in
	rol 
	jmp Do1BitWavb
Do1BitWav:
	asl							;Move bits into correct pos
Do1BitWavb:
	asl
	asl
	jmp Do1BitWavc
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
