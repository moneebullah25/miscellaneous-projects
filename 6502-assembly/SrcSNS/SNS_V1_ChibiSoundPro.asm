ChibiOctave:
	;   E     F     G      A     B     C     D   
	dw $0000,$0020,$0040,$0080,$01C0,$0180,$0210 ;0
	;   41.2  43.6   49    55   61.7  65.4  73.4
	ifndef SmsTranspose
	dw $02E0,$0340,$0380,$03a0,$0420,$0450,$04a0 ;1
	endif
	;   82.4  87.3  98    110   123    130   146
	dw $0570,$05a0,$0660,$0710,$0840,$0860,$0960 ;2
	;	164   174    196   220 	246   261   293
	dw $0ac0,$0b10,$0ca0,$0e20,$1000,$10b0,$1280;3
	;   329   349   392   440   493   523    587
	dw $1520,$1670,$1910,$1c40,$1f90,$21a0,$25d0 ;4
	;    659  698   783   880   987   1046  1174
	dw $2a40,$2cd0,$3270,$3870,$3f70,$4320,$4b30 ;5
	;   1318  1396  1567  1760  1975  2093  2349
	dw $5490,$59a0,$6490,$70f0,$7e80,$8600,$96a0;6
	dw $F400,$F600,$F800,$FACC,$FC0,$FE00,$FFFF ;EXTRA
	   

	

ChibiSoundPro_INIT:		;Call this to get ChibiSound ready!

	jsr SPC7000_Sound_INIT	;Load the driver into the SPC 700
		
	lda #$2C			;Echo Volume Left	-VVVVVVV	Volume
	jsr ProcessSoundCommandZero		;A=Reg Val=0
	
	lda #$3C			;Echo Volume Right	-VVVVVVV	Volume
	jsr ProcessSoundCommandZero		;A=Reg X=Val
	
	lda #$0F			;fF	COEF	8-tap FIR Filter coefficients 
						;SCCCCCCC	Signed Coeficcient filter f=0
						
	jsr ProcessSoundCommandZero		;A=Reg Val=0
	
	lda #$2D			;Pitch modulation	CCCCCCC-	Channel (1-7)
	jsr ProcessSoundCommandZero		;A=Reg Val=0
	
	ldx #$7f			;Max volume
	lda #$0C			;Main Volume Left	-VVVVVVV	Volume
	jsr ProcessSoundCommandX		;A=Reg X=Val
	lda #$1C			;Main Volume Right	-VVVVVVV	Volume
	jsr ProcessSoundCommandX		;A=Reg X=Val
	
	lda #$5D			;Offset of source directory (sample)
						;(DIR*100h = memory offset)	OOOOOOOO
						; Offset $oo00
						
	ldx #$03			;Data at $0300
	jsr ProcessSoundCommandX		;A=Reg X=Val

	lda #$4D			;Echo enable	CCCCCCCC	Channel (0=off)
	jsr ProcessSoundCommandZero		;A=Reg Val=0

	lda #$6C	;DSP Flags. (used for MUTE,ECHO,RESET,NOISE CLOCK)
				;RMENNNNN	Reset (0=off) Mute (0=off) 
				;Echo (1=0ff) N=Noise clock
	ldx #%00011111
	jsr ProcessSoundCommandX	;A=Reg X=Val
	
	ldy #7			;Set up all 8 channels (0-7)
	
InitChannel:	
	lda #$04		;Source number (references the source
					;  directory)	SSSSSSSS	Source
	ldx #0
	jsr ProcessSoundCommandXofchannel		;A=Reg Val=0
		
	
	lda #$05		;If bit7 is set, ADSR is enabled. 
					; If cleared GAIN is used.	EDDDAAAA
					; 	Enable, Dr, Ar
	ldx #0
	jsr ProcessSoundCommandXofchannel		;A=Reg Val=X
	lda #$06		;These two registers control the 
					; ADSR envelope.	LLLRRRRR	sL,sR
	
	jsr ProcessSoundCommandXofchannel		;A=Reg Val=X;
	
	lda #$07		;c7	GAIN	This register provides function
					; for software envelopes.	GGGGGGGG
					;  G=Envelope bits
	ldx #255
	jsr ProcessSoundCommandXofchannel		;A=Reg Val=X
	
	dey
	bpl InitChannel ; repeat until <0
		
ChibiSoundPro_Update:	
	rts
	

	
ChibiSoundPro_Set:
	lda #$5C		;Key Off 	CCCCCCCC	Channel
	jsr ProcessSoundCommandZero		;A=Reg X=Val
		
	lda z_l
	and #%00000111		;%-CCCCCCC - Channel number
	tay
	
	lda z_h				;%VVVVVVVV - Volume
	bne ChibiSound_NotSilent
	
	lda ChannelLookupB,y
	tax	
	lda #$5C			;Key Off 	CCCCCCCC	Channel
	jmp ProcessSoundCommandX		;Set RegA to valueX
ChibiSound_NotSilent:
	
	
	lda z_l
	and #%10000000	;%N------- - Noise State
	beq ChibiSound_NoNoise
ChibiSound_Noise:		
	lda #$3D		;Noise enable	CCCCCCCC	Channel (0=off)
	jsr ProcessSoundCommandX_OR	;A=Reg 

	lda z_d			;NNNN---- Frequency
	lsr				;Alter frequency range of noise
	lsr	
	lsr
	lsr			
	sta z_b			;----NNNN
	lsr
	add z_b			;-----NNN
	add #8
	tax
	lda #$6C	;DSP Flags. (used for MUTE,ECHO,RESET,NOISE CLOCK)
		;RMENNNNN	Reset (0=off) Mute (0=off) 
		;Echo (1=0ff) N=Noise clock
	jsr ProcessSoundCommandX	;A=Reg X=Val

	lda z_h			;VVVVVVV volume
	lsr 			;Lower volume for noise
	jmp ChibiSound_NoiseDone	;Set Volume
	
	
ChibiSound_NoNoise:	
	;Disable noise
	lda #$3D		;Noise enable	CCCCCCCC	Channel (0=off)
	jsr ProcessSoundCommandX_NOT	;A=Reg 

	lda z_h			;VVVVVVV volume
	
ChibiSound_NoiseDone:	
	lsr 			;-VVVVVVV
	tax
	lda #$00		;Left Volume	-VVVVVVV	Volume
	jsr ProcessSoundCommandXofchannel		;A=Reg X=Val
	lda #$01		;Right Volume	-VVVVVVV	Volume
	jsr ProcessSoundCommandXofchannel		;A=Reg X=Val
	
					;PPPPPPPP ppppppppp
	lsr z_d			
	ror z_e			;-PPPPPPP Ppppppppp
	lsr z_d			
	ror z_e			;--PPPPPP PPppppppp
	
;You may want to remove this depending on your sample
	lsr z_d		
	ror z_e			;---PPPPP PPPpppppp
	
	ldx z_d
	lda #$03		;Pitch H	--PPPPPP	Pitch
	jsr ProcessSoundCommandXofchannel	;A=Reg X=Val
	ldx z_e
	lda #$02		;Pitch L	PPPPPPPP	Pitch
	jsr ProcessSoundCommandXofchannel	;A=Reg X=Val

	
;Lets play the sound! KEY ON!
	lda ChannelLookupB,y	
	tax	
	lda #$4C		;Key On	CCCCCCCC	Channel
	jmp ProcessSoundCommandX		;Set RegA to valueX


	
;Turn ON A=Reg Y=channel (0-7)
ProcessSoundCommandX_OR:		
	pha
	phy
		pha
		lda ChannelLookupB,y		;Get bit for chn Y
		ply
		ora ChannelBuffer,y			;set bit of channel
	jmp ProcessSoundCommandX_B
	
;Turn OFF A=Reg Y=channel (0-7)
ProcessSoundCommandX_NOT:			
	pha
	phy
		pha
			lda ChannelLookupB,y	;Get bit for chn Y
			eor #255			
		ply
		and ChannelBuffer,y			;clear bit of channel
ProcessSoundCommandX_B:
		sta ChannelBuffer,y
		tax							;New param->X
	ply
	pla
	jmp ProcessSoundCommandX		;Set RegA to valueX
	
	
;Set Reg A of channel Y to Val X
ProcessSoundCommandXofchannel:		
	ora ChannelLookupA,y			;Find channel nibble for chn Y
	jmp ProcessSoundCommandX		;Set RegA to valueX
		
	
	
;Used to track the noise state 
ChannelBuffer equ chibisoundram+64	;Reg cache
		
;Top nibble channel mask
ChannelLookupA:	db $00,$10,$20,$30,$40,$50,$60,$70	

;Bit channel mask
ChannelLookupB: db %00000001,%00000010,%00000100,%00001000
				db %00010000,%00100000,%01000000,%10000000
	
	
;Sample For Chibisound pro
;We're going to load this into $0300 (IN SPC Ram)

SFXBank:						
	dw SFXBank_Sound1-SFXBank+$300	;Sample 0 main
	dw SFXBank_Sound1-SFXBank+$300	;Sample 0 Loop
									;Sample 1 main...

SFXBank_Sound1:	;Samples (9 bytes per sample set)
	db %10110011		;Header SSSSFFLE (Looping last sample)
	;   SSSSFFLE S= bitshift (0-12) F=Filter L=Loop E=End of sample
	
;Samples (1 nibble per sample ADPCM)
	db $77,$77,$77,$00,$88,$88,$88,$00	
	
	
	include "\srcSNS\SPC700_Compatibility.asm"
	include "\srcSNS\SNS_V1_SPC700_Driver.asm"