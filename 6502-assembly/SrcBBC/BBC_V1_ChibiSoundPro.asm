
;SmsSimpleNoise equ 1	;This limits noise frequency to 0-2
						;Otherwise we use channel 2 for frequency

;SmsTranspose equ 1		;The SMS can't do accurate low tones


	;Octave
ChibiOctave:
	dw $0020,$0030,$0040,$0080,$00C0,$0100,$0140 ;0
	
	;   E     F     G      A     B     C     D   
	;   41.2  43.6   49    55   61.7  65.4  73.4
	ifndef SmsTranspose
		dw $0180,$01C0,$0200,$0240,$0280,$02C0,$0300 ;1
	endif
	;   82.4  87.3  98    110   123    130   146
	dw $0340,$0380,$03C0,$03AA,$1D5E,$295A,$4161 	;2
	;	164   174    196   220 	246   261   293
	dw $562A,$6024,$717C,$80DB,$8E9D,$9516,$A0A5	;3
	;   329   349   392   440   493   523    587
	dw $AB17,$AFC3,$B8A2,$C076,$C746,$CA8F,$D060 	;4
	;    659  698   783   880   987   1046  1174
	dw $D566,$D7DE,$DC17,$E01A,$E3AF,$E520,$E801 	;5
	;   1318  1396  1567  1760  1975  2093  2349
	dw $EAAE,$EBEB,$EE11,$F003,$F1C3,$F2B2,$F3D2	;6
	
	dw $F400,$F600,$F800,$FACC,$FC0,$FE00,$FFFF ;EXTRA
	   


;H=Volume (0-255 Higher num = Louder) 
;L=Channel Num (0-127 unused channels will wrap around) / Top Bit=Noise
;DE=Pitch (0-65535) (Higher num = Higher Freq)

chibisoundpro_Set:		
	lda 255					;Set all bits to write
	sta $FE43 				;Data direction port

	lda z_l
	and #%10000000				;Noise Bit
	bne chibisoundpro_noise		;Nose on!
	
	lda z_l
	and #%00000011			;We only have 3 channels
	tay
	lda channelmask,y		;Lookup table for channel bits 
	sta z_b					;B=Hardware channel setting 
	
	lda ChannelNoise,y		;Get Noise state
	beq noisestilloff		;Check if we need to turn off noise
	lda #%11111111			;noise was on, now off
	sta $FE41
	lda #0
	sta ChannelNoise,y		;Clear Noise flag
	
	jmp noisestilloff
	


	;We have to support 3 noise capable channels,
	;But we only have 1 actual noise channel
	;so we track the noise state of all 3 with 'ChannelNoise'
	
chibisoundpro_noise:
	lda z_l
	and #%00000011		;Lookup for noise state
	tay
	lda #1
	sta ChannelNoise,y	;Set noise state of 'virtual channel' to on

		;%----VVVV
	lda #%10011111		;set volume (0=Max)
	ora z_b				;channelnum
	sta $FE41			;Silence tone channel
	
	ifdef smssimplenoise 	;Only 2 bit noise
		lda z_d
		and #%11000000		;Top two noise bits
		clc
		rol
		rol
		rol
		eor #%00000011		;%1cct-mrr
		sec
		sbc #1				;Need value 0-2
noisezero:
		ora #%11100100
		sta $FE41			;Enable our noise channel (3)

		lda z_h				;Volume %VVVVVVVV
		lsr
		lsr
		lsr
		lsr					;Volume %----VVVV

		eor #%11111111		;set volume (chn3)
		sta $FE41
		rts
		
	else
		lda #%01000000		;Full Frequency noise, but
		sta z_b				;noise channel uses tone 2
		
		lda #%11011111	;1cctvvvv	(latch - channel type volume)
		sta $FE41		;mute tone 2 (Just need it's frequency)
		
		lda #%11100111	;1cct-mrr	(latch - channel type... noise mode (1=white) rate (rate 11= use tone channel 2)
		sta $FE41
	endif

noisestilloff:
	lda z_d
	eor #255		;Flip Frequency bits
	sta z_d

	lda z_e
	eor #255
	
	lsr z_d			;Bitshift %DDDDDDDD EEEEEEEE
	ror 			;to 10 bit %--DDDDDD DDEE----
	lsr z_d
	ror 		

	lsr 
	lsr 
	lsr 
	lsr 			;%----DDEE
		 ;1CCTLLLL	(Latch - Channel Type DataL
	ora #%10000000		; low tone
	ora z_b				;channelnum
	sta $FE41
	
	lda z_d			  	;%0-HHHHHH
	and #%00111111		;high tone %--DDDDDD
	sta $FE41
	

	ifndef smssimplenoise
		lda z_l
		and #%10000000	;we're done if there is no noise
		beq noiseoff
		lda #%01100000	;Want to set volume of channel 3
		sta z_b
noiseoff:
	endif	
	
	lda z_h				;Volume %VVVVVVVV
	lsr
	lsr
	lsr
	lsr					;%----VVVV

	eor #%10011111		;set volume (0=Max)
	ora z_b				;channelnum
	sta $FE41

chibisoundpro_init:		;Only needed for systems that need pre-setup of the sound system
chibisoundpro_update:	;Only needed for systems which don't make a constant tone
	rts

	
	align 4
channelmask:
	db %00000000,%00100000,%01000000,%00000000	;Chn4 will use Chn 0

channelnoise equ chibisoundram+64				;First 64 chibisound bytes reseved for tracker

	
