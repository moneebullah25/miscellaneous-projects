
ChibiOctave:
	;   E     F     G      A     B     C     D   
	dw $0000,$0000,$0000,$0000,$0000,$0000,$0000 ;0
	;   41.2  43.6   49    55   61.7  65.4  73.4
	ifndef SmsTranspose
		dw $0000,$0000,$0000,$0000,$0000,$0000,$0000 ;1
	endif 
	;   82.4  87.3  98    110   123    130   146
	dw $0000,$0000,$0000,$0000,$0000,$0e00,$1f00 ;2
	;	164   174    196   220 	246   261   293
	dw $3c00,$4a00,$5e00,$6200,$7f00,$8800,$9300;3
	;   329   349   392   440   493   523    587
	dw $9f00,$a500,$af00,$b800,$c019,$c300,$ca00 ;4
	;    659  698   783   880   987   1046  1174
	dw $d000,$d200,$d800,$dc00,$e000,$e200,$e500 ;5
	;   1318  1396  1567  1760  1975  2093  2349
	dw $e800,$e900,$Ec00,$ee00,$f000,$f100,$f300;6
	dw $F500,$f700,$F900,$FcCC,$Fd00,$FE00,$FFFF ;EXTRA
	
;Pokey at $D200 on A800 / $E800 on A5200


;H=Volume (0-255) 
;L=Channel Num (0-127 unused channels will wrap around) / Top Bit=Noise
;DE=Pitch (0-65535)

ChibiSoundPro_Set:
	lda z_l
	and #%00000011	;Channel number %NCCCCCCC
	asl
	tay				;$D200-1 / 2-3 / 4-5 / 6-7
	
	lda z_d		
	eor #255
	sta POKEY,Y		;FFFFFFFF	F=Frequency
	iny
	
	lda #%10100000
	sta z_b			;%1010---- Square wave
	
	lda z_l
	and #%10000000	;N-------
	beq ChibiSoundProNoNoise
	
	lda #%00000000
	sta z_b			;0000---- Noise	
ChibiSoundProNoNoise:	

	lda z_h			;VVVV---- Volume
	lsr
	lsr
	lsr
	lsr
	ora z_b
	sta POKEY,Y		;NNNNVVVV	N=Noise (0=noise / 10=Square wave)
						;V=Volume (15=loudest)
ChibiSoundPro_Update:
	rts

	

ChibiSoundPro_Init:
	lda #0			;Reset POKEY sound Control
	sta  POKEY+$08	;N1234HHS	N=Noise bit depth 1234=Channel Clocks 
	rts
	
	
	
	