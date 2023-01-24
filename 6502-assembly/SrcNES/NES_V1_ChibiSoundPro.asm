
ChibiOctave:
	;   E     F     G      A     B     C     D   
	dw $0080,$0100,$0180,$0200,$0280,$0300,$0380 ;0
	;   41.2  43.6   49    55   61.7  65.4  73.4
	ifndef SmsTranspose
		dw $0400,$0500,$06000,$0700,$0800,$02C0,$0300 ;1
	endif
	;   82.4  87.3  98    110   123    130   146
	dw $0340,$0380,$03C0,$03AA,$1eb8,$2AB8,$4188 ;2
	;	164   174    196   220 	246   261   293
	dw $5788,$6088,$71F8,$817c,$8E9C,$95AC,$A0AC;3
	;   329   349   392   440   493   523    587
	dw $ABAC,$AFAC,$B8AC,$C0AC,$C7AC,$CAAC,$D0AC ;4
	;    659  698   783   880   987   1046  1174
	dw $D5AC,$D808,$DC63,$E06C,$E3C4,$E55c,$E84c ;5
	;   1318  1396  1567  1760  1975  2093  2349
	dw $EAF4,$ec1c,$EE40,$F0c3,$F1FC,$F2C4,$F438;6
	dw $F400,$F600,$F800,$FACC,$FC00,$FE00,$FFFF ;EXTRA
	   

chibisoundpro_init:
	lda #0
	sta channelmixer			;Turn off channels by default
	rts
	
;H=Volume (0-255) 
;L=Channel Num (0-127 unused channels will wrap around) / Top Bit=Noise
;DE=Pitch (0-65535)

chibisoundpro_Set:		
	lda z_l						;Channel number %-CCCCCCC
	and #%00000011				;wrap every 4 channels
	tay							;Set Y to channel lookup
	
	lda #$40
	sta z_b						;Sound Regs are at $40xx
	
	lda channelmap,y
	sta z_c						;Get Channel address $40xx
	
	lda channelmask,y
	tax							;Channel Mask in X for $4015
	
	
	lda z_l						;Noise %N-------
	and #%10000000
	beq ChibiSoundPro_NotNoise	;Nose Bit Set?
	
;chibisoundpro_noise
	sta ChannelNoise,y			;Mark noise as on (A!=0)
	
	jsr SilenceChannel			;Silence the tone
	
	lda #$0C
	sta z_c						;$400C = Noise Channel
	
	ldx #%00001000				;%DF-54321 for $4015
	jsr NesSetVolume			;Set Channel X Volume
		
	lda z_d						;Frequency H byte
	lsr
	lsr
	lsr
	lsr
	eor #%00001111
	sta $400E					;N---FFFF	N=Noise type, F=Frequency 
	lda #0
	sta $400F					;CCCCC---	 length Counter load register
	rts 	
ChibiSoundPro_NotNoise:			


	lda ChannelNoise,y
	beq noisestilloff			;Noise already off
	
	lda #0
	sta ChannelNoise,y			;Mark noise as off
	lda #%11110111				
	jsr MixChannelsAnd			;Mute noise
	
	
NoiseStillOff:
	jsr NesSetVolume			;Set Channel X Volume
								;Must do this before setting freq
NesSetFrequency:	
	lda #0						;Buildup H byte
	
	rol z_e
	rol z_d						;Only want
	rol							;DDDDDDDD EE------
	rol z_e
	rol z_d
	rol
	
	eor #%00000011
	ldy #3
	sta (z_bc),y 				;CCCCCHHH	frequency H byte, 
	dey							;length Counter load register
	
	lda z_d						;D Now L byte
	eor #%11111111
	sta (z_bc),y				;LLLLLLLL	frequency L byte
chibisoundpro_update:
	rts
	

SilenceChannel:
	txa					
	eor #255					;Mask to silence channel (Set bit=0)
MixChannelsAnd:
	and channelmixer			;Set Channel bit to 0
MixChannels2:
	sta $4015					;DF-54321 - Channel 12345 on 
	sta channelmixer			;(Writing resets Frequency) 
	rts	
	
NesEnableChannel:	
	txa
	ora channelmixer			;Set Channel bit to 1
	jmp MixChannels2
	
	
NesSetVolume:
	lda z_h
	and #%11110000				;only use top 4 bits of volume
	beq SilenceChannel
	lsr
	lsr
	sec							;same as ora #%01000000		
	ror
	lsr
	ldy #0
	sta (z_bc),y				;%CCLEVVVV Volume, Envelope Length 
									;counter,duty Cycle
	jsr NesEnableChannel
	
	lda z_c
	cmp #$08	
	beq NesTriangle				;Trangle channel has no volume
	rts
	
	
NesTriangle:
	lda #$F0
	sta $4008					;SLLLLLLL L=Linear Counter Load S=Start 
	
	sec							;Force top bit to 1
	ror z_d						;Shift Triangle pitch to roughly 
	ror z_e							;match Rectangle
	rts
	
	
	align 4
channelmap:
	db $00,$04,$08,$04			;Channel base $40xx
	
channelmask:					;Mask for 4015h - %DF-54321 (1=on)
	db %00000001,%00000010,%00000100,%00000010
	
channelnoise equ chibisoundram+64	;Virtual noise channel on?

channelmixer equ channelnoise+8		;Copy of 4015h

