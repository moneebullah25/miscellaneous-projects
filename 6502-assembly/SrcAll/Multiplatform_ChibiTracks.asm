

;ChibiTracks_AllowRelocation equ 1
;ChibiTracks_AllowSpeedChange equ 1
;ChibiTracks_AllowSpeedChange_Inst equ 1		;Affect Instruments too


;Command Byte Definitions

Cmd_Ptch equ $0F	;$0F,n
Cmd_Volu equ $0E	;$0E,n - Absolute volume n
Cmd_Note equ $0D	;$0D,n - Play Note pitch n
Cmd_Loop equ $0C	;$0C,n - Jump back n bytes
Cmd_Inst equ $0B	;$0B,n - select instrument n

Cmd_VolA equ $F0	;&F0+n - Volume Adjust
Cmd_VolD equ $00	;&F0+ -n - Volume Adjust


Cmd_PtcD equ $E0	;$E0+n
Cmd_PtcU equ $D0	;$D0+n
Cmd_Nois equ $C0	;$C0+n (1/0) - Noise On/Off

Cmd_Pend equ $10	;$10 - Pattern End

Seq_Repeat equ 255

;0 = end of commands

;0F,x = Pitch to x
;0E,x = vol to x
;0D,x = note to x
;0C,x = Loop to Offset -0 to -255 bytes
;0B,x = Play Instrument x

;Fx = Vol shift -8 to +7
;Ex = Pitch shift down 0 to -15
;Dx = Pitch shift up 0 to +15
;Cx = Noise state x (1=on 0=off)
;10 = End of Pattern

;Pattern 255=Repeat whole song






	ifdef chibitracks_allowspeedchange
speedmult:				;A=A*Songspeed
		ldx songspeed
		dex
		beq speedmultDone
		sta z_c
speedmultb:
		clc
		adc z_c			;Multiply C by X
		dex 
		bne SpeedMultB
speedmultDone:
		rts
	endif


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


ChibiTracks_StartSong:

;Reset Channel Data
	lda #0						;Zero byte
	ldy #channeldatalengthtotal	;Data pos
	tax							;Channel Num
ChannelClearAgain:	
	sta channelstate0,y
	dey
	bpl ChannelClearAgain
	
;Set up Channel Numbers
	stx channelstate0+2			;Channel num 0
	inx
	stx channelstate1+2			;Channel num 1
	inx
	stx channelstate2+2		 	;Channel num 2
		
	
;	lda #0			;start with pattern 0 in sequence 
startsongagain:
	pha
		loadpairfrom z_hl,songbase

		ldy #0
		lda (z_hl),y				;get channel count
		sta songchannels
		sta z_b
		
		iny
		lda (z_hl),y				;get RepPoint count
		sta RepPoint
		
		iny
		lda (z_hl),y				;get SongSpeed count
		sta SongSpeed	
		
		iny
		lda (z_hl),y				;get SongBaseL
		sta z_e
		iny
		lda (z_hl),y				;get SongBaseH
		sta z_d
		
		
		lda SongBase				;Load address
		sec
		sbc z_e
		sta SongOffset				;Calculate offset Load-Compiled 
		sta z_c
		lda SongBase+1
		sbc z_d
		sta SongOffset+1
		tax
		
		iny
		lda (z_hl),y				;get PatternListL
		clc
		adc z_c
		sta PatternList
		iny
		txa
		adc (z_hl),y				;get PatternListH
		sta PatternList+1
		
		iny
		lda (z_hl),y				;get InstrumentlistL
		clc
		adc z_c
		sta Instrumentlist
		iny
		txa
		adc (z_hl),y				;get InstrumentlistH
		sta Instrumentlist+1
		
		iny
		tya
		clc
		adc z_l				;Update L of HL pair (Song data addr)
		sta z_l
		bcc startsongagainb
			inc z_h
startsongagainb:

		loadpair z_ix,channelstate0	;First Channel to init
		ldx z_b
	pla
startnextseq:
	sta z_c						;Use Rep Point
	pha
		txa	;Channel Count
		pha
			jsr InitSongSequence	;init a channel
			jsr GetNextChannel		;Move to next channel
		pla
		tax
	pla 
	dex 						;repeat for other channels
	bne startnextseq
	rts


GetNextChannel:					;Add ChannelLength to IX
	lda #channeldatalength
	clc
	adc z_ixl
	sta z_ixl
	bcc GetNextChannelDone
	inc z_ixh
GetNextChannelDone:	
	rts
	
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


	
	
InitSongSequence:		;z_C =Loop point / sequence offset
	ldx #0
	lda (z_hl,x)
	sta z_e				;Get address of Sequence into DE
	jsr IncHL
	lda (z_hl,x)
	sta z_d
	jsr IncHL
	
	pushpair z_hl
		lda z_c		 	;Use Rep Point
		clc
		adc z_e			;Add Loop point L
		sta z_e
		bcc initsongsequenceB
			inc z_d 	;Add Loop point H
initsongsequenceB
		ifdef ChibiTracks_AllowRelocation
			clc
			lda z_e
			adc SongOffset ;Add song offset to Sequence address
			sta z_e
			lda z_d
			adc SongOffset+1
			sta z_d
		endif
	
		jsr getnextsequence ;Load in the Pattern from the 
							;sequence
		
		ldy #chn_patl	;Save Next sequence Addr
		jsr StoreHLtoIX

		ldy #Chn_InsT
		lda #1			;Force Instrument update
		sta (z_ix),y

		ldy #chn_insl	
		lda #<ChibsoundSilentIns ;Silent Istrument (no sound)
		sta (z_ix),y	;Save Instrument address
		iny 	
		lda #>ChibsoundSilentIns
		sta (z_ix),y
	pullpair z_hl
	rts

	
	
	
getnextsequence:		;Sequence list in DE
	ldx #0				
	lda (z_de,x)		;Next Sequence
	cmp #255
	beq RestartSong		;255=repeat 

	ifdef debugsong
		sta z_spec
		PushAll
		pushpair z_hl
		pushpair z_de
		pushpair z_bc
			jsr newline
			lda z_spec
			jsr showhexsafe	;show new pattern for debug
		pullpair z_bc
		pullpair z_de
		pullpair z_hl
		Pullall
	endif
	
	asl					;2 bytes per entry
	sta z_c
	jsr incde			;move to next pattern

	ldy #chn_seql		;Save new Sequence address
	lda z_e
	sta (z_ix),y
	iny ;ldy #chn_seqh
	lda z_d
	sta (z_ix),y

	clc
	lda patternlist
	adc z_c				;Get Pattern list entry
	sta z_e
	lda patternlist+1
	adc #0
	sta z_d
		
	;ldx #0
	lda (z_de,x)
	sta z_l				;HL=Pattern address
	jsr IncDE
	lda (z_de,x)
	sta z_h
	
	ifdef chibitracks_allowrelocation
		clc
		lda z_l
		adc SongOffset	;Get Pattern address
		sta z_l
		lda z_h
		adc SongOffset+1
		sta z_h
	endif
		
	ldy #chn_patt
	lda #1				;Force an update
	sta (z_ix),y		;Pattern Time
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


RestartSong:			;At the end of pattern list
	pla
	pla
	pla
	pla
	pla					;clear down the stack
	
	lda RepPoint		;Use Rep Point 
	jsr startsongagain
	;jmp chibitracks_Play
	
	
	
chibitracks_play:
	loadpair z_ix,channelstate0	;Pointer to channel data

	ldx songchannels		;Channel count
updatenextseq:
	txa
	pha
		ldy #chn_patT		;Pattern Time
		lda (z_ix),y		;Pattern needs updating?
		sec
		sbc #1
		sta (z_ix),y	
		bne ChibiTracks_NoPatChange
			jsr readpattern	;0=yes... process pattern
ChibiTracks_NoPatChange:

		ldy #chn_insT		;Instrument Time
		lda (z_ix),y		;No Delay (no more commands)
		beq updatenextseq_nochange
		lda (z_ix),y		;Instrument needs updating?
		sec
		sbc #1
		sta (z_ix),y	
		bne updatenextseq_nochange
			jsr updatechannel ;0=Yes process instrument
updatenextseq_nochange:			

		jsr GetNextChannel
	pla
	tax
	dex						;repeat for other channels
	bne updatenextseq
	jmp chibisoundpro_update ;Update hardware
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

readpattern:
	ldy #chn_patL			;Load current pattern point HL
	jsr LoadHLfromIX
	
	ldx #0
	lda (z_hl,x)			;Get Next pattern line time	
	ifdef chibitracks_allowspeedchange
		jsr speedmult		;Multiply speed if required
	endif
	
	ldy #chn_patt			;update pattern line time

	jsr processsoundcommandsPlus ;Read this lines commands

	ldy #chn_patL			;Update current pattern point
	;jmp StoreHLtoIX
	
StoreHLtoIX:				;Store HL to IX,Y
	lda z_l
	sta (z_ix),y			;Store L
	iny
	lda z_h
	sta (z_ix),y			;Store H
	rts	

LoadHLfromIX:				;Load HL from IX,Y
	lda (z_ix),y
	sta z_l					;Load L
	iny 
	lda (z_ix),y			;Load H
	sta z_h
	rts
	
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

updatechannel:			;Update instrument
	ldy #chn_insL
	jsr LoadHLfromIX		;Get instrument point HL

	ldx #0					;Get Next Inst Delay
	lda (z_hl,x)			;0=instrument ends
	beq stopinst			;Stop note

	ifdef chibitracks_allowspeedchange_inst
		jsr speedmult
	endif

	ldy #chn_inst			;Update Instrument delay
	jsr processsoundcommandsPlus ;Process instrument commands

	ldy #chn_insl
	jsr StoreHLtoIX			;update instrument address HL

	
;we need to play the current tone
	ldy #chn_note			
	lda (z_ix),y			;Note num
	sta z_e

	ldy #chn_pitc			
	lda (z_ix),y			;Pitch shift

	jsr tweentone			;A= ooooffff	
							;o=offset -16 to +15 f=fraction 0-15

	ifdef debugnotes
		lda z_d				;show frequency for debug
		jsr showhexsafe
		lda z_e
		jsr showhexsafe
	endif

	ldy #chn_volu
	lda (z_ix),y			;Volume
	bne InstSetVol
stopinst:
	lda #0		;instrument ended - silence channel
InstSetVol:
	sta z_h

	ldy #chn_cnum
	lda (z_ix),y			;Channel + Noise State (Bit 7)
	sta z_l
	jmp chibisoundpro_set



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	
processsoundcommandsPlus:
	sta (z_ix),y			;Store updated Time
	jsr inchl
	
processsoundcommands:		;Process Instrument/Pattern commands
instagain:
	
	ldx #0
	lda (z_hl,x)			;Get command
	
	pha
		jsr inchl
	pla
	bne instendbx			;0=end of commands
		rts					;Done!
instendbx
	tax						;Back up for later

	and #$f0				;top nibble=command
	beq instmultibyte		;0x=multibyte commands

	cmp #$f0				;Xn commands X=command n=param
	beq instvol
	cmp #$d0
	beq instpitchup
	cmp #$e0
	beq instpitchdown
	cmp #$c0
	bne instnoisex		

;instnoise
	lda #%01111111			;Keep Channel Number
	ldy #chn_cnum			;%-CCCCCCC
	and (z_ix),y
	sta z_c
	
	txa
	and #%00000001			;Noise setting
	clc						
	ror
	ror						;%N-------
	ora z_c					;ChannelNum
	sta (z_ix),y
	jmp instagain
	
instnoisex:
	cmp #$10
	bne patternendx			;Unknown command
	
;patternend
	ldy #chn_seql
	lda (z_ix),y			;Get Sequence L
	sta z_e
	iny 
	lda (z_ix),y			;Get Sequence H
	sta z_d
	jmp getnextsequence	
	
patternendx
	rts						;unknown! (shrug)

	;$b0 a0 90 80 70 60 50 40 30 20 spare!
	
	
instpitchdown:
	txa
	and #%00001111			;pitch bend
	eor #%11111110
	ldy #chn_pitc
	sta (z_ix),y			;bend down
	jmp instagain

instpitchup:
	txa
	and #%00001111			;pitch bend
	ldy #chn_pitc
	sta (z_ix),y			;bend up
	jmp instagain

	
instvol:
	txa
	and #%00001111			;volume shift
	asl 
	asl 
	asl 
	asl 					;%VVVV----
	bmi instvoldown			;Top bit 1?
	ldy #chn_volu	
	clc
	adc (z_ix),y
	bcs instvoldone
		lda #255			;over max vol
instvoldone:
	ldy #chn_volu			;volume
	sta (z_ix),y
	jmp instagain

instvoldown:
	clc
	ldy #chn_volu
	adc (z_ix),y
	bcs instvoldone
		lda #0				;under min vol
	beq instvoldone	

	
instmultibyte:	;0x,n commands x=command n=parameter 
	txa
	pha
		ldx #0
		lda (z_hl,x)		;get parameter byte
		tax 				;Store in X
		jsr inchl
	pla						;Check Command low nibble
	cmp #$0f
	beq instbytepitch
	cmp #$0e
	beq instbytevol
	cmp #$0d
	beq instbytenote
	cmp #$0c
	beq instloop
	cmp #$0b
	beq instplayinstrument
	rts		;unknown! (shrug)

	;$0a 09 08 07 06 05 04 03 02 01 spare!

	
instloop:
	stx z_c
	lda #255
	sta z_b
	jsr addhl_bc			;Junp back to previous script pos
	jmp instagain

	
instbytenote:
	txa  					;note byte
	ldy #chn_note
	sta (z_ix),y
	jmp instagain

instbytevol:
	txa  					;vol byte
	ldy #chn_volu
	sta (z_ix),y
	jmp instagain

instbytepitch:
	txa  					;bend byte
	ldy #chn_pitc
	sta (z_ix),y
	jmp instagain

	
instplayinstrument:
	txa						;Instrument number
	asl 					;2 bytes per inst address

	clc
	adc instrumentlist		;Ins address list
	sta z_e
	lda #0
	tay
	adc instrumentlist+1
	sta z_d					;Instrument vector in DE
	
	lda (z_de),y			;instrument data address
	sta z_c
	iny				
	lda (z_de),y		
	sta z_b					;BC=Script address

	lda #1
	ldy #chn_insT			;Trigger Instrument update 
	sta (z_ix),y			;Next tick
	
	ifdef ChibiTracks_AllowRelocation
		ldy #chn_insl
		lda z_c
		adc SongOffset
		sta (z_ix),y
		iny 
		lda z_b
		adc SongOffset+1
		sta (z_ix),y
	else			
		ldy #chn_insl	;Save address of instrument script
		lda z_c
		sta (z_ix),y
		iny 
		lda z_b
		sta (z_ix),y
	endif
	jmp instagain

	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ChibsoundSilentIns:
;	db 1,	$0E,0	,	0
	db 0

ChannelDataLength equ 16
ChannelDataLengthTotal equ 16*3		;48 bytes - but should allocate at least 64+32 (32 for platform vars)

ChannelState0 equ ChibiSoundRam
ChannelState1 equ ChibiSoundRam+16
ChannelState2 equ ChibiSoundRam+16+16

;Channel Data layout
Chn_Note equ 0
Chn_Volu equ 1
Chn_CNum equ 2
Chn_Pitc equ 3
Chn_InsL equ 4
Chn_InsH equ 5			;Must follow L
Chn_InsT equ 6
Chn_PatL equ 7
Chn_PatH equ 8			;Must follow L
Chn_PatT equ 9
Chn_SeqL equ 10
Chn_SeqH equ 11			;Must follow L


; DChannelState0:
	; db 0	;Note (0-55) 0
	; db 255	;Volume 1
	; db 0 	;Channel/Noise state 2
	; db 0	;Pitch shift 3 
	; dw ChibsoundSilentIns ;4 5
	; db 1	;Timeout for current inst state 6

	; dw 0	;7 8 pattern 
	; db 0		;9 timeout

	; dw 0  ;10 11 sequence

	; db 0,0,0,0	;Unused

; DChannelState1:
	; db 0	;Note (0-55) 0
	; db 255	;Volume 1
	; db 1 	;Channel/Noise state 2
	; db 0	;Pitch shift 3 
	; dw ChibsoundSilentIns ;4 5
	; db 1	;Timeout for current inst state 6

	; dw 0	;7 8
	; db 0		;9

	; dw 0  ;10 11

	; db 0,0,0,0	;Unused

; DChannelState2:
	; db 0	;Note (0-55) 0
	; db 255	;Volume 1
	; db 2 	;Channel/Noise state 2
	; db 0	;Pitch shift 3 
	; dw ChibsoundSilentIns ;4 5
	; db 1	;Timeout for current inst state 6

	; dw 0	;7 8
	; db 0		;9

	; dw 0  ;10 11

	; db 0,0,0,0	;Unused


	