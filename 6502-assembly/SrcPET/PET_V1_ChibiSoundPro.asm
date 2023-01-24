
;Singlechannel Ignore all but one channel (May give better result?)


ChibiOctave:
	;   E     F     G      A     B     C     D   
	dw $0000,$0000,$0000,$0000,$0000,$0000,$0000  ;0
	;   41.2  43.6   49    55   61.7  65.4  73.4
	ifndef SmsTranspose
		dw $0000,$0000,$0000,$0000,$0000,$0000,$0000  ;1
		 ;   82.4  87.3  98    110   123    130   146
		dw $0000,$0000,$0000,$0000,$0000,$0000,$0000 ;2
	endif
	;	164   174    196   220 	246   261   293
	dw $0000,$0000,$0000,$0000,$0240,$0c60,$1d60;3
	;   329   349   392   440   493   523    587
	dw $2D30,$3400,$41c0,$4d50,$5750,$5c30,$64d0 ;4
	;    659  698   783   880   987   1046  1174
	dw $6c90,$7000,$7680,$7c00,$8300,$8c00,$9de0 ;5
	;   1318  1396  1567  1760  1975  2093  2349
	dw $ad50,$b410,$c160,$ccc0,$d750,$dc00,$e4b0;6
	dw $E800,$EC00,$F000,$F400,$F800,$Fc00,$FFFF ;EXTRA
  ifdef SmsTranspose
		dw $FF00,$FF00,$FF00,$FF00,$FF00,$FF00,$FF00 ;EXTRA
		dw $FF00,$FF00,$FF00,$FF00,$FF00,$FF00,$FF00 ;EXTRA
  endif


chibisoundpro_init:
	ldx #0
	lda #0
chibisoundpro_Zero:	
	sta ChannelCache,x
	inx
	cpx #16
	bne chibisoundpro_Zero
	rts	   
	
;H=Volume (0-255) 
;L=Channel Num (0-127 unused channels will wrap around) / Top Bit=Noise
;DE=Pitch (0-65535)
		
		
;Note the PET can't really do Volume or Noise.		
		
chibisoundpro_Set:		
	lda z_L
	;and #%10000000
	;bne SkipChannel			;Skip Noise ?
	
	lda z_L
	and #%00000011
	asl
	asl
	tax
	lda z_h
	sta ChannelCache,x
	inx
	lda z_l
	sta ChannelCache,x
	inx
	lda z_d
	sta ChannelCache,x
	inx
	lda z_e
	sta ChannelCache,x
SkipChannel:
	rts
	
	
chibisoundpro_Update:	
	ldx #0
	stx z_h
chibisoundpro_CheckNext:
	lda ChannelCache,x	;Virtual Channel cache volume
	inx
	cmp z_h		
	bcc chibisoundpro_NotHigher
	beq chibisoundpro_NotHigher
	
	sta z_h				;Volume of this virtual channels
							;Is higher
	lda ChannelCache,x
	sta z_l				;Load in other settings
	inx		
	lda ChannelCache,x
	sta z_d
	inx
	lda ChannelCache,x
	sta z_e
	jmp chibisoundpro_Done
chibisoundpro_NotHigher
	inx					;Skip thsi virtual channel
	inx
chibisoundpro_Done:
	inx
	ifndef Singlechannel
		cpx #16
		bcc chibisoundpro_CheckNext	 ;4 channels * 4 bytes
	endif
	
	
	lda z_h
	and #%11110000
	beq chibisoundpro_Silent
		lda #16		;16= sound on ... 0=off
chibisoundpro_Silent:
	sta $E84B		;Channel ON/Off
		
	ldx #15			;Values $0000-$7FFF = Octave 0/1
	
	lda z_d
	and #%10000000	
	beq chibisoundpro_SetOctave
	
	ldx #85			;Values $8000-$FFFF = Octave 2/3
chibisoundpro_SetOctave:
	stx $E84A		;Octave (15/51/85)
	
	lda z_d
	and #%01111111
	eor #%01111111
	sta z_d
	lsr 			;half of D - convert 0-128 to 0-192
		
	clc
	adc z_d 		;Frequency (64-255)
	adc #64
	sta $E848		;Note
	
	rts

ChannelCache equ chibisoundram+128	;Cache for 4 sound channels
	


;Address $E84B will turn the sound on or off... we write #16 to turn it on, #0 to turn it off.
;Address $E84A can be used to select the Octave with value 15/51/85
;Address $E848 can be used to select the note, a value of 64-255 should be passed.	
	
;Note 		octave=15				 octave=51				octave=85
;Freq	Octave 0	Octave 1	Octave 1	Octave 2	Octave 2	Octave 3
;B		251			125			251			125			251			125
;C		238			118			238			118			238			118
;C#		224			110			224			110			224			110
;D		210			104			210			104			210			104
;D#		199			99			199			99			199			99
;E		188			93			188			93			188			93
;F		177			88			177			88			177			88
;F#		168			83			168			83			168			83
;G		158			78			158			78			158			78
;G#		149			74			149			74			149			74
;A		140			69			140			69			140			69
;A#		133			65			133			65			133			65




;Alternative octave table (created with freq anaz, rather than calculated)


; ChibiOctave:
	; ;   E     F     G      A     B     C     D   
	; dw $0000,$0000,$0000,$0000,$0000,$0000,$0000  ;0
	; ;   41.2  43.6   49    55   61.7  65.4  73.4
	; ifndef SmsTranspose
		; dw $0000,$0000,$0000,$0000,$0000,$0000,$0000  ;1
		; ;   82.4  87.3  98    110   123    130   146
		; dw $0000,$0000,$0000,$0000,$0000,$0000,$0000 ;2
	; endif
	; ;	164   174    196   220 	246   261   293
	; dw $0000,$0000,$0000,$0000,$0500,$0E00,$2000	;3

	; ;   329   349   392   440   493   523    587
	; dw $2F00,$3600,$4200,$4E00,$5800,$5D00,$6600	;4
		
	; ;    659  698   783   880   987   1046  1174
	; dw $6D00,$7100,$7700,$7D00,$8500,$8E00,$A000	;5

	; ;   1318  1396  1567  1760  1975  2093  2349
	; dw $AF00,$B600,$C200,$CE00,$D800,$DD00,$E600	;6
	; dw $ED00,$F100,$F700,$FD00,$FE00,$FF00,$FFFF ;EXTRA
  ; ifdef SmsTranspose
		; dw $FF00,$FF00,$FF00,$FF00,$FF00,$FF00,$FF00 ;EXTRA
		; dw $FF00,$FF00,$FF00,$FF00,$FF00,$FF00,$FF00 ;EXTRA
  ; endif
