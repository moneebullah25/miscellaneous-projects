
ChibiOctave:
	;   E     F     G      A     B     C     D   
	dw $0200,$0300,$0400,$0500,$0600,$0700,$0800 ;0
	;   41.2  43.6   49    55   61.7  65.4  73.4
	ifndef SmsTranspose
	dw $0A00,$0C00,$1000,$1200,$1400,$1500,$3000 ;1
	endif
	;   82.4  87.3  98    110   123    130   146
	dw $4300,$4F00,$6200,$7200,$7f00,$8900,$9700 ;2
	;	164   174    196   220 	246   261   293
	dw $A500,$a700,$AE00,$B900,$C200,$C500,$CD00 ;3
	;   329   349   392   440   493   523    587
	dw $D100,$D500,$D900,$DD00,$E000,$E300,$E400 ;4
	;    659  698   783   880   987   1046  1174
	dw $E800,$EA00,$EC00,$EF00,$F000,$F200,$F300 ;5
	;   1318  1396  1567  1760  1975  2093  2349
	dw $f400,$F600,$F700,$F700,$f800,$f800,$fB00 ;6
	dw $FC00,$fC00,$FD00,$FDCC,$FE00,$FE00,$FFFF ;EXTRA
	

;h=volume (0-255) 
;l=channel num (0-127 unused channels will wrap around) / top bit=noise
;de=pitch (0-65535)

	
ChibiSoundPro_Set:			
	lda z_l				;%-CCCCCCC
	and #%00000011		
	asl
	asl
	asl					;8 bytes per channel
	tay					;Channel offset
	
	lda z_h
	lsr 				;%-VVVVVVV
	sta $fd20,y			;Volume (127=max 0=min)	
	
	lda z_d					
	lsr					;%-FFFFFFF
	eor #%01111111
	sta $fd24,y			;Timer Backup Value  
						; (effectively frequency)

	lda z_l
	and #%10000000		;Noise Bit
	beq ChibiSound_NoNoise
	lda #$F0			;16= Noise
	jmp ChibiSound_NoiseSet
ChibiSound_NoNoise:	
	lda #$01			;1= Good Tone
ChibiSound_NoiseSet:
	sta $fd21,y			;Shift register feedback enable
	rts
	
	

ChibiSoundPro_Init:
;Sound On
	stz $Fd50				;LLLLRRRR	LR Vol - 0=all on / 255=all off
	
;Set up the 4 channels
	ldy #0				
ChibiSoundPro_Initb:
	lda #0
	sta $fd23,y			;SSSSSSSS	Shift Regsiter L
	sta $fd27,y			;SSSS-CBB	S=Shift Register H, 
									;C=Clock state B=Borrow
									
	lda #$80			;$80=Silent
	sta $fd22,y			;Reset Audio Output Value (raw data)
	
	lda #%00011110 		;FTIRCKKK	F=Feedback bit 7 , reset Timer done, 
						;enable Integrate, enable Reload enable Count,
						;clocK select
	sta $fd25,y			;Audio Control bits	
	
	tya
	clc 
	adc #8
	tay
	cpy #8*4
	bne ChibiSoundPro_Initb
	
ChibiSoundPro_Update:
	rts
	

;FD20	FD20	Audio Channel 0 – 2’s compliment Volume control		0-127
;FD21	FD21	Audio Channel 0 – Shift register feedback enable		eg %00010000
;FD22	FD22	Audio Channel 0 – Audio Output Value (Raw Data)	Eg $80
;FD23	FD23	Audio Channel 0 –Lower 8 bits of shift register		Eg 0
;FD24	FD24	Audio Channel 0 – Audio Timer Backup Value		eg 0-63
;FD25	FD25	Audio Channel 0 – Audio Control Bits 	FTIRCKKK	eg %00011110
;FD26	FD26	Audio Channel 0 – Audio Counter	
;FD27	FD27	Audio Channel 0 –Other Audio Bits		Eg 0

;FD28	FD2F	Audio Channel 1 – Same as Channel 0 	

;FD30	FD37	Audio Channel 2 – Same as Channel 0	

;FD38	FD3F	Audio Channel 3 – Same as Channel 0	

;FD40	FD40	ATTENREG0	LLLLRRRR – Audio Attenuation	
;FD41	FD41	ATTENREG1	LLLLRRRR – Audio Attenuation	
;FD42	FD42	ATTENREG2	LLLLRRRR – Audio Attenuation	
;FD43	FD43	ATTENREG3	LLLLRRRR – Audio Attenuation	
;FD44	FD44	MPAN	Stereo attenuation selection	
;FD50	FD50	MSTEREO	Stereo disable	LLLLRRRR	0=all on 255=all off

