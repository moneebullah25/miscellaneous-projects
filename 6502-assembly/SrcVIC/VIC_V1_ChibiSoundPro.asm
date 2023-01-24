

ChibiOctave:
	;   E     F     G      A     B     C     D   
	dw $6100,$6180,$6200,$6280,$6300,$6380,$6980 ;0
	;   41.2  43.6   49    55   61.7  65.4  73.4
	dw $6F80,$7180,$7780,$7B80,$7F80,$8000,$8600 ;1
	;   82.4  87.3  98    110   123    130   146
	dw $8C00,$8E00,$9400,$9800,$9C00,$A000,$A600 ;2
	;	164   174    196   220 	246   261   293
	dw $AC00,$AE00,$B400,$B800,$BC00,$C000,$C600 ;3
	;   329   349   392   440   493   523    587
	dw $CC00,$CE00,$D400,$D800,$DC00,$E000,$E600 ;4
	;    659  698   783   880   987   1046  1174
	dw $EC00,$EE00,$F400,$F800,$FC00,$FD00,$FE00 ;5
	;   1318  1396  1567  1760  1975  2093  2349
	dw $FF00,$FF00,$FF00,$FFF0,$FF00,$FF00,$FF00 ;6
	dw $FF00,$FF00,$FF00,$FF00,$FF00,$FF00,$FF00 ;EXTRA

	
	

chibisoundpro_init:
	ldx #0
	lda #0
chibisoundpro_Zero:	
	sta ChannelCache,x		;Clear Cache
	inx
	cpx #16
	bne chibisoundpro_Zero
	rts	   
		
	

;H=Volume (0-255) 
;L=Channel Num (0-127 unused channels will wrap around) / Top Bit=Noise
;DE=Pitch (0-65535)

chibisoundpro_Set:		
	;lda z_L
	;and %01111111
	;bne SkipChannel		;Only one channel
	
	lda z_L
	and #%00000011
	asl
	asl
	tax					;Get virtual channel pos
	
	lda z_h
	sta ChannelCache,x
	inx
	
	lda ChannelCache,x
	and #%10000000
	beq NoiseDone			;Maybe Turn noise channel off
		
NoiseWasON:
	lda z_l
	and #%10000000
	bne NoiseDone			;Noise still on - do nothing
	
	sta $900D				;Noise now off
	
NoiseDone:	
	lda z_l
	sta ChannelCache,x
	inx
	lda z_d
	sta ChannelCache,x
	inx
	lda z_e
	sta ChannelCache,x
	
	
;Lets look for loudest virtual channel	
	ldx #0					
	stx z_h
chibisoundpro_CheckNext:
	lda ChannelCache,x		;Virtual Channel cache volume
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
	inx					;Skip this virtual channel
	inx
chibisoundpro_Done:
	inx
	ifndef Singlechannel
		cpx #16
		bcc chibisoundpro_CheckNext
	endif

	lda #$90			;$9000 = Base of sound registers
	sta z_b
	lda #0
	sta z_c
	
	sta $900A			;Silence the tone channels
	sta $900B
	sta $900C
	
	
	lda z_h
	and #%11110000		;VVVVVVVV - Volume
	ror 
	ror 
	ror 
	ror
	sta z_h				
	
	lda $900E			;CCCCVVVV	V=Volume C=Aux color
	and #%11110000		;Get Aux Color bits CCCC----
	ora z_h				;OR in volume 		----VVVV
	sta $900E			;CCCCVVVV	V=Volume C=Aux color
	
	
	lda z_l	
	and #%10000000		;Rest Noise bit
	beq chibisoundpro_NoNoise

	ldy #$0D
	lda z_d
	sec 
	ror				;Top bit 1
	sta (z_bc),y	;$900D - OFFFFFFF ... O=On F=Frequency
	rts
chibisoundpro_NoNoise:	
		
		
	lda z_d
	and #%11100000		;Frequency bits
	lsr
	lsr
	lsr
	lsr
	tax					;%----OOO- ... Octave in lookup table
									;   (2 bytes per entry)
	lda ChibiSoundOctAddr,x
	tay					;Base address in Y
	inx
	
	
	lda z_d				;---OOOOO O-------
	rol z_e
	rol 
	and #%00111111		;60 per octave
	sta z_d
	
	lda ChibiSoundOctAddr,x
	cmp #135			
	beq NoFix2
	lsr z_d				;30 per octave
	cmp #195			
	beq NoFix2
	lsr z_d				;15 per octave
NoFix2:	

	lda z_d
	clc
	adc ChibiSoundOctAddr,x
	sta (z_bc),y		; OFFFFFFF ... O=On F=Frequency
SkipChannel:
chibisoundpro_Update:		
	rts
	
	;Addr,OctBase,Addr,OctBase...
ChibiSoundOctAddr:
	db $0A,128,$0A,128,$0A,128,$0A,135,$0A,195,$0B,195,$0C,195,$0C,225


ChannelCache equ chibisoundram+128	;Cache for 4 sound channels

 ; +-----------------------------------------------------+ 
 ; |  NOTE          VALUE           NOTE          VALUE  |
 ; +-----------------------------------------------------+
 ; |   C             135             G             215   |
 ; |   C#            143             G#            217   |
 ; |   D             147             A             219   |
 ; |   D#            151             A#            221   |
 ; |   E             159             B             223   |
 ; |   E +           161             B+            224   |
 ; |   F             163             C             225   |
 ; |   F#            167             C#            227   |
 ; |   G             175             D             228   |
 ; |   G#            179             D#            229   |
 ; |   A             183             E             231   |
;									 E+            231
 ; |   A#            187             F             232   |
 ; |   B             191             F#            233   |
;      B+            193
 ; |   C             195             G             235   |
 ; |   C#            199             G#            236   |
 ; |   D             201             A             237   |
 ; |   D#            203             A#            238   |
 ; |   E             207             B             239   |
 ; |   E+            207             B+            239   |
 ; |   F             209             C             240   |
 ; |   F#            212             C#            241   |
 ; +-----------------------------------------------------+
 ; | S3 ($900C)                                     |
 ; | ----------                                     |
 ; |    D  O     228                                |
 ; |    E  C                                        |
 ; |    F  T                                        |
 ; |    G  A                        HIGHEST OCTAVE  |
 ; |    A  V                                        |
 ; |    B  E                                        |
 ; |    C  3     S2 ($900B)                         |
 ; | ----------  ----------                         |
 ; |    D  O        D  O                            |
 ; |    E  C        E  C                            |
 ; |    F  T        F  T                            |
 ; |    G  A        G  A                            |
 ; |    A  V        A  V                            |
 ; |    B  E        B  E                            |
 ; |    C  2        C  3     S1 ($900A)             |
 ; | ----------  ----------  ----------             |
 ; |    D  O        D  O        D  O                |
 ; |    E  C        E  C        E  C      135/195/225     |
 ; |    F  T        F  T        F  T                |
 ; |    G  A        G  A        G  A                |
 ; |    A  V        A  V        A  V                |
 ; |    B  E        B  E        B  E                |
 ; |    C  1        C  2        C  3                |
 ; | ----------  ----------  ----------             |
 ; |                D  O        D  O                |
 ; |                E  C        E  C                |
 ; |                F  T        F  T                |
 ; |                G  A        G  A                |
 ; |                A  V        A  V                |
 ; |                B  E        B  E                |
 ; |                C  1        C  2                |
 ; |             ----------  ----------             |
 ; |                            D  O                |
 ; |                            E  C                |
 ; |                            F  T                |
 ; | LOWEST OCTAVE              G  A                |
 ; |                            A  V                |
 ; |                            B  E                |
 ; |                            C  1       135      |
 ; |                         ----------             |
 ; +------------------------------------------------+