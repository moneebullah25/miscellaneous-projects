

ChibiOctave:
	dw $0020,$0030,$0040,$0080,$00C0,$0100,$0140 ;0
	dw $0020,$0030,$0040,$0080,$00C0,$0100,$0140 ;0
	;   E     F     G      A     B     C     D   
	;   41.2  43.6   49    55   61.7  65.4  73.4
	dw $027A,$029f,$02fd,$0343,$03b6,$03e6,$0450 ;1
	;   82.4  87.3  98    110   123    130   146
	dw $0536,$05C2,$0684,$0753,$0835,$08ac,$09b6 ;2
	;	164   174    196   220 	246   261   293
	dw $0aeb,$0ba5,$0d10,$0ead,$106c,$1168,$138d;3
	;   329   349   392   440   493   523    587
	dw $15e7,$1740,$1a13,$1d53,$20d8,$22ca,$2711 ;4
	;    659  698   783   880   987   1046  1174
	dw $2bda,$2e6f,$3422,$3a8d,$41ab,$45a3,$4e12 ;5
	;   1318  1396  1567  1760  1975  2093  2349
	dw $57b8,$5cf1,$6846,$74fe,$836e,$8b47,$9c35;6
	dw $a000,$b000,$c000,$d000,$e000,$F000,$FFFF ;EXTRA


;H=Volume (0-255) 
;L=Channel Num (0-127 unused channels will wrap around) / Top Bit=Noise
;DE=Pitch (0-65535)

chibisoundpro_Set:		
	lda #$D4		;Reg base = $D4xx
	sta z_b
	
	lda z_l
	and #%00000011	;Only 3 actual channels
	tay
	lda ChannelMempos,y	;Get Memory address for channel
	sta z_c			;$D400,$D407,$D40E
	
	
	lda z_l
	and #%10000000
	beq chibisoundpro_NoNoise
	asl z_e			;Noise frequencies are totally out
	rol z_d			;of wack to tone ones!
	asl z_e
	rol z_d
chibisoundpro_NoNoise:	

	ldy #0			;Point to first channel register.
	
	lda z_e
	sta (z_bc),y	;$D400	Voice #1 frequency L	LLLLLLLL
	iny
	lda z_d
	sta (z_bc),y	;$D401	Voice #1 frequency H	HHHHHHHH
	iny				;Higher values=higher pitch
	
	
	lda #%00000000
	sta (z_bc),y	;$D402 LLLLLLLL 	Voice #1 pulse width L
	iny
	lda #%00001111
	sta (z_bc),y	;$D403 ----HHHH		Voice #1 pulse width H
	iny
	
	
	lda z_h
	beq ChibisoundMute	;Volume 0 = Disable channel
	
	
	lda z_l
	and #%10000000	;Noise Bit
	bne ChibiSound_NoiseDone
		; NPST-RSG
	lda #%01000000	;Set Pulse
ChibiSound_NoiseDone:
	ora #%00000001
	sta (z_bc),y	;$D404 NPST-RSG	Voice #1 control register 
	iny				;Noise / Pulse / Sawtooth / Triangle / 	
					;- test / Ring mod / Sync /Gate	


	lda #0
	sta (z_bc),y	;$D405 AAAADDDD	Voice #1 Attack and Decay length 
	iny				;Attack / Decay
	
	
	lda z_h
	and #%11000000	;Low volume is very low, so only use two bits
	ora #%00110000	;of H
	sta (z_bc),y	;$D406	Voice #1 Sustain volume and Release 
					;length.	VVVVRRRR	Sustain Vol / Release
chibisoundpro_Update:			
	rts
	
ChibisoundMute:
	sta (z_bc),y	;$D404 NPST-RSG	Voice #1 control register
	ldy #6			;Noise / Pulse / Sawtooth / Triangle / 
					;- test / Ring mod / Sync /Gate
	sta (z_bc),y	;$D406	Voice #1 Sustain volume and Release length.
					;VVVVRRRR	Sustain Vol / Release
	rts

	
chibisoundpro_Init:			
	lda #%00001111	;MHBLVVVV
	sta $D418		;Mute3 / Highpass / Bandpass / Lowpass / 
					;Volume (0=silent)
	lda #0
	sta $D416		;HHHHHHHH	Cut off frequency
	sta $D415		;-----LLL	Cut off frequency 
	sta $D417		;RRRREVVV	R=Resonance (0=off) / 
	rts						   ;External / V= Voice 3-1


ChannelMempos: db $00,$07,$0E,$07	;Channel 1,2,3,2

