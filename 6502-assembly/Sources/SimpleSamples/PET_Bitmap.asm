;DoubleWidth equ 1 ;For 80x25 pet

	include "\SrcALL\V1_Header.asm"		;Cartridge/Program header - platform specific
	include "\SrcAll\BasicMacros.asm"		;Basic macros for ASM tasks

	SEI						;Stop interrupts
	jsr Cls					;Clear the screen
	
	
	;lda #12
	;sta $E84C	;Enable Graphics on later PETs (12=Gra 14=Lower)

		
	ldx #8
	ldy #0
	jsr GetVDPScreenPos		;Get Vram Address in z_DE
	
	loadpair z_hl,Bitmap	;Source bitmap
	lda #24
	sta z_b		;Wid
	lda #24
	sta z_c		;Hei
	jsr ShowBitmap
	
	
	ldx #4		;Xpos
	ldy #24		;Ypos
	jsr GetVDPScreenPos		;Get Vram Address in z_DE
	
	loadpair z_hl,BitPatterns
	lda #32
	sta z_b		;Wid
	lda #1
	sta z_c		;Hei
	jsr ShowBitmap
	
	rts
	
ShowBitmap:	;Show Zero terminated 'bitmap' from z_HL to z_DE
				;Size (W,H) z_B,z_C 
					
	ldy z_b				;Width
ShowBitmapH:
	dey
	lda (z_hl),y		;Transfer one char
	sta (z_de),y
	cpy #0				;Transfer bytes until char 0
	bne ShowBitmapH		;next Char
	
	jsr GetNextLine		;Next Dest line
	
	clc
	lda z_b				;next line of source data.
	adc z_l
	sta z_l
	lda z_h
	adc #0
	sta z_h
	
	dec z_c
	bne ShowBitmap		;Next line
	rts	
	
	
GetVDPScreenPos:	; Get Screen address BC=XYpos in z_DE
	lda #$80			;Screen base is $8000
	sta z_d		
	
	txa					;Xpos
	ifdef ScreenWidth32
		clc
		adc #4
	endif
	sta z_e

	tya					;Ypos
	beq GetVDPScreenPos_YZero
GetVDPScreenPos_Addagain:	;Repeatedly add screen width 
	clc							;Y times
	lda z_e
	ifdef DoubleWidth
		adc #80			;80 bytes per line
	else
		adc #40			;40 bytes per line
	endif
	sta z_e
	lda z_d
	adc #0			;Add Carry
	sta z_d
	
	dey
	bne GetVDPScreenPos_Addagain
GetVDPScreenPos_YZero:
	rts
	
	
GetNextLine:				;Move z_DE down a line
	clc
	ifdef DoubleWidth
		lda #80				;Move Dest down 1 line
	else
		lda #40				;Move Dest down 1 line
	endif
	adc z_e
	sta z_e					;Update Low byte
	lda z_d
	adc #0					;Update High byte with carry
	sta z_d
	rts	
	
	
BitPatterns:
	db 'X',$60,'X',$7E,'X',$7C,'X',$E2
	db 'X',$7B,'X',$61,'X',$FF,'X',$EC
	db 'X',$6C,'X',$7F,'X',$E1,'X',$FB
	db 'X',$62,'X',$FC,'X',$FE,'X',$E0
	db 0
	
Bitmap:
	incbin "\ResAll\Sprites\RawPet.RAW"
	db 0
	
	
	include "\SrcAll\monitor.asm"			;Debugging tools
	include "\SrcAll\BasicFunctions.asm"	;Basic commands for ASM tasks
	
	

	include "\SrcALL\V1_Functions.asm"		;Basic text to screen functions
	include "\SrcAll\V1_BitmapMemory.asm"	;Bitmap functions for Bitmap screen systems
	include "\SrcALL\V1_Footer.asm"			;Footer for systems that need it
	
	