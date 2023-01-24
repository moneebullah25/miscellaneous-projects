
ChibiOctave:
	;   E     F     G      A     B     C     D   
	dw $0020,$0030,$0040,$0080,$00C0,$0100,$0140 ;0
	;   41.2  43.6   49    55   61.7  65.4  73.4
	dw $56b4,$6025,$715f,$8135,$8e6e,$951b,$a0fd ;1
	;   82.4  87.3  98    110   123    130   146
	dw $aace,$afe8,$b8d9,$c088,$c73d,$ca6f,$d050 ;2
	;	164   174    196   220 	246   261   293
	dw $d56c,$d838,$dc65,$e06c,$e39c,$e541,$e805 ;3
	;   329   349   392   440   493   523    587
	dw $eaa0,$ebe5,$ee16,$F000,$f1b8,$f28c,$f40a ;4
	;    659  698   783   880   987   1046  1174
	dw $f557,$f5e9,$f6fb,$f800,$F8E0,$F940,$F9F0 ;5
	;   1318  1396  1567  1760  1975  2093  2349
	dw $fa9e,$fac0,$fb6d,$FBE0,$FC50,$FC93,$FD00 ;6
	dw $FD60,$FDC0,$FE00,$FE40,$FE80,$FEC0,$FFFF ;EXTRA
	   
	   
	   
;Init sound levels, and Set up the wave samples
chibisoundpro_init:
	lda #255				;Volume to max
	sta $0801				;Main Amplitude Level %LLLLLRRRRR
	
	ldx #5					;Init channels 0-5
chibisoundpro_init2:
	phx
		stx	$0800			;Channel Select %-----CCC
		
		lda #255
		sta $0805			;Channel L/R Volume %LLLLRRRR
		
		stz $0804			;Channel On/Write %ED-VVVVV
		
		;Define the wave data
		
			lda #%00011111	;Wave data
ChibiSoundMoreWaves:
			ldx #16			;Write wave bytes 
ChibiSoundMoreBytes:		
				sta $0806	;Waveform Data (5 bit)
			dex 
			bne ChibiSoundMoreBytes
		eor #%00011111		;Flip all 5 bits of the wave
		beq ChibiSoundMoreWaves		;Do twice, all 1, then all 0
	plx
	dex
	bpl chibisoundpro_init2
	rts
    


;H=Volume (0-255) 
;L=Channel Num (0-127 unused channels will wrap around) / Top Bit=Noise
;DE=Pitch (0-65535)

chibisoundpro_Set:		
	lda z_l				;Channel %-CCCCCCC
	and #%00000011		;Cnannel number 0-3 for tones
	tay						;(4 for noise)
	
	
	lda z_l
	and #%10000000		;Test noise bit %N-------
	beq ChibiSoundPro_NotNoise
	
	;Silence the tone
	sty $0800			;Select channel
	stz $0804			;Channel On  / Volume %ED-VVVVV
	
	sta ChannelNoise,y	;Mark noise as on (Nonzero value)
	
	ldy #4				;All nose on Channel 4
	bra	chibisoundpro_SetChannel
ChibiSoundPro_NotNoise:	


	lda ChannelNoise,y	;Check if noise was on before
	beq chibisoundpro_SetChannel	;Noise still off
	
	;Silence the noise
	lda #4				;All nose on Channel 4
	sta $0800			;Channel Select %-----CCC
	
	lda #0
	sta ChannelNoise,y	;Mark noise as off
	
	stz $0804			;Channel On  / Volume %ED-VVVVV
	stz $0807			;Enable noise… Noise freq %E--NNNNN
	
	
chibisoundpro_SetChannel:		
	sty $0800			;Channel Select %-----CCC
	
	lda z_h				;Volume %VVVVVVVV
	lsr 
	lsr
	beq ChibisoundProDisableChannel		
	sec					;Enable bit
	ror
ChibisoundProDisableChannel:
	sta $0804			;Channel On  / Volume %ED-VVVVV
	
	
	;Set Noise Frequency
	lda z_l				;%N-------
	rol
	php					;Get top bit
		lda z_d			;Frequency high byte
		lsr
		lsr
	plp
	ror
	sta $0807			;Enable noise… Noise freq %E--NNNNN
	
	
	;Set Tone Frequency
	lda z_d
	lsr 
	ror z_e
	lsr 
	ror z_e
	lsr 
	ror z_e
	lsr 
	ror z_e
	
	eor #$0F			;Flip top 4 bits
	sta $0803			;Frequency H %----HHHH
	
	lda z_e
	eor #255			;Flip bottom 8 bits
	sta $0802			;Frequency L %LLLLLLLL
	
chibisoundpro_update:
	rts
	

channelnoise equ chibisoundram+64	;Current noise state


