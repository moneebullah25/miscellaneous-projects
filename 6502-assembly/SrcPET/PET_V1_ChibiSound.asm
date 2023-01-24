		
		
ChibiSound:	;NVPPPPPP - N=Noise V=Volume P=Pitch

;Note the PET can't really do Volume or Noise.

	pha
		lda #16		;16= sound on ... 0=off
		sta $E84B
	pla
	beq silent
	tax	
	and #%00111111
	rol 
	rol
	ora #%00000011
	sta $E848
	lda #15		;octive (15/51/85)
	sta $E84A
ChibiSoundFinish:
	rts
silent:				
		lda #0		;16= sound on ... 0=off
		sta $E84B
	rts

	
	
	