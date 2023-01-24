chibiOctave:
		;   E     F     G      A     B     C     D   
	dw $0000,$0000,$0000,$0000,$0000,$0000,$0000 ;0
	;   41.2  43.6   49    55   61.7  65.4  73.4
	ifndef SmsTranspose
	dw $0000,$0000,$0000,$0000,$01e0,$10e0,$2be0 ;1
	endif
	;   82.4  87.3  98    110   123    130   146
	dw $4270,$4d30,$6100,$72a0,$81d0,$8920,$9660 ;2
	;	164   174    196   220 	246   261   293
	dw $a210,$a990,$b1a0,$ba60,$c040,$c590,$cc10;3
	;   329   349   392   440   493   523    587
	dw $D1F0,$d4a0,$d980,$de20,$e1f0,$e3e0,$e720 ;4
	;    659  698   783   880   987   1046  1174
	dw $EA00,$eb60,$ede0,$F000,$F200,$F2D0,$F480 ;5
	;   1318  1396  1567  1760  1975  2093  2349
	dw $F610,$F6B0,$F7F0,$F910,$FA00,$FA70,$FB40 ;6
	dw $FC40,$FD40,$FE00,$FE80,$FF00,$FF80,$FFFF ;EXTRA
	   

chibisoundpro_init:			;Setup sound hardware
	ldy #127
	lda #0
chibisoundpro_Clear:
	sta chibisoundram,y		;Clear our sound cache
	dey
	bpl chibisoundpro_Clear	;Clear 128 bytes
	
	lda #16
	;sta ToneLength
	;rts
	
;special function to set tone lenth (16=default)
chibisoundpro_setbeeperlength:
	sta ToneLength
	rts

;h=volume (0-255) 
;l=channel num (0-127 unused channels will wrap around) / top bit=noise
;de=pitch (0-65535)

	
chibisoundpro_set:
	lda z_l
	and #%00000011			;Channel number
	asl
	asl						;4 bytes per entry
	tax
	lda z_h
	sta ChannelCache,x		;Store data in cache
	inx
	lda z_l
	sta ChannelCache,x
	inx
	lda z_d
	sta ChannelCache,x
	inx
	lda z_e
	sta ChannelCache,x
	rts
	

chibisoundpro_Update:	
	ldx #0
	stx z_h
chibisoundpro_CheckNext:
	lda ChannelCache,x		;Virtual Channel cache volume
	inx
	cmp z_h		
	bcc chibisoundpro_NotHigher
	beq chibisoundpro_NotHigher
	
	sta z_h					;Volume of this virtual channels
							;Is higher
	lda ChannelCache,x
	sta z_l					;ChannelNum/Noise
	inx		
	lda ChannelCache,x
	eor #255
	sta z_b					;Pitch H
	inx
	lda ChannelCache,x
	eor #255
	sta z_c					;Pitch L
	jmp chibisoundpro_Done
chibisoundpro_NotHigher
	inx						;Skip this virtual channel
	inx
chibisoundpro_Done:
	inx
	ifndef chibisoundpro_singlechannel
		cpx #16
		bcc chibisoundpro_CheckNext	 ;4 channels * 4 bytes
	endif

	
	lda z_h
	and #%11110000			;Muted?
	beq chibisoundpro_Silent

	
	
	lda z_l
	and #%10000000			;Noise state
	tax						;0=off
	
	inc NoisePos			;Noise source
	ldy NoisePos	
	
	
	lda #0					;00000000 HHHHHHHH LLLLLLLL
	rol z_c				
	rol z_b
	rol 					;0000000H HHHHHHHL LLLLLLL0
	rol z_c
	rol z_b
	rol 					;000000HH HHHHHHLL LLLLLL00
	
	sta z_d					;000000HH 
	
	lda z_b
	sta z_e					;HHHHHHLL
		
	inc z_d					;Fix Counters
	inc z_e
	
	lda ToneLength
	sta z_h					;Tone length (Lower=Less Cpu)
	
	;Lets start the tone!
ChibiSoundPro_ToneLoop:	
	txa						;Randomize beep?
	iny
	and ProgramStart,y		;Random data for noise effect from our code
	bne NoiseSkip
	lda $C030				;Reading C030 'beeps' the speaker
NoiseSkip:

	lda z_d					;H Loop
	sta z_b
	lda z_E					;L Loop
	sta z_C

	lda z_h					;Time left to play this tone
	sec
	sbc z_b					;- Pitch of this tone
	sta z_h
	bcs ChibiSoundPro_NoOverFlow
	rts						;<0 - Finish tone
ChibiSoundPro_NoOverFlow:

	jsr chibisoundpro_Pause	;Delay until next sound (pitch from BC)
	
	jmp ChibiSoundPro_ToneLoop
	

chibisoundpro_Silent:		;Mimic a 'tone pause'
	ifdef chibisoundpro_FastSilent	
;Don't pause for silence (eg if running in interrupt handler)
		rts					
	else
		lda ToneLength		;Tone length (Lower=Less Cpu)
		sta z_b
		lda #255
		sta z_c
	endif
	
chibisoundpro_Pause:
	dec z_c
	bne chibisoundpro_Pause	;loop between beeps
	dec z_b
	bne chibisoundpro_Pause
	rts

ChannelCache equ chibisoundram+64;first 64 bytes reserved for tracker

NoisePos equ ChannelCache+16	;0-255 offset for randomization

ToneLength equ ChannelCache+17	;normal Length of tones


