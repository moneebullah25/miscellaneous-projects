

z_Regs 		equ $20

z_HL equ z_Regs
z_L  equ z_Regs
z_H  equ z_Regs+1


z_DE equ z_Regs+4
z_E  equ z_Regs+4
z_D  equ z_Regs+5


	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
	ORG $0C00	;Program Start
	sei 	;Disable interrupts
	
	lda $C050 ; TXTCLR:   Display Graphics
	lda $C052 ; MIXCLR:   Display Full Screen
	lda $c057 ; HIRES:    Display HiRes Graphics
	lda $C055 ; TXTPAGE2: If 80STORE Off: Display Page 2
	

	
	lda #<Bitmap	;Source Bitmap Data
	sta z_L
	lda #>Bitmap
	sta z_H

	ldx #6			;Xpos
	ldy #8			;Ypos
	
bmpwidth equ 8		
	lda #6			;HEIGHT 6 Strips of 8 lines = 48 pixels tall
NexBitmapNextStrip:	
	pha
	phx
		phy
			jsr GetScreenPos
			ldx #8			;8 lines per square
BitmapNextLine:
			phy
				ldY #0
BitmapNextByte:
				lda (z_hl),Y ;Read byte from source
				sta (z_de),Y ;Write to screen
				inY
				cpY #bmpwidth				
				bne BitmapNextByte
						
				tya			
				clc
				adc z_l		;ADD Y to Z_HL to move source 
				sta z_l
				lda z_h
				adc #0
				sta z_h	
					
				lda z_d		;move mempos down a line
				clc
				adc #$04	;add $0400 to the line number 
				sta z_d		;  (only works within an  8 line block)
			ply
			dex
			bne BitmapNextLine	;Some systems need a recalc every 8 lines
		pla
		clc
		adc #8				;Move Y down 8 lines
		tay
	plx
	pla
	sec
	Sbc #1							
	bne NexBitmapNextStrip			

	jmp *
	
	
GetScreenPos:
	;Screen layout is split in 3 parts according to Y line
	;AABBBCCC - AA*$0028  BBB*$0080  CCC*$0400
	lda #0
	sta z_e
	tya 				;--BBB---	;Multiply by $0080
	and #%00111000
	lsr
	lsr
	lsr					
	lsr					;Shift 1 bit right 
	ror z_e
	adc #$40			;Screen base
	sta z_d
	tya					;AA------		;multiply by $0028
	rol 				;Get 1st A from AA------ 
	bcc GetScreenPos_SecondThird
GetScreenPos_ThirdThird:
	lda z_e
	clc
	adc #$50			;3/3 = Add $0050 to address
	jmp GetScreenPos_ThirdDone
GetScreenPos_SecondThird:
	rol 				;Get 2nd A from AA------ 
	bcc GetScreenPos_FirstThird
	lda z_e
	clc
	adc #$28			;3/2 = Add $0028 to address
GetScreenPos_ThirdDone:	
	sta z_e
GetScreenPos_FirstThird:;1/3 = Add nothing to addreess
	tya 			;-----CCC	;Multiply by 4
	and #%00000111
	asl
	asl
	adc z_d
	sta z_d
	
	txa				;Process X
	clc
	adc z_e			;Add X to calculated address
	sta z_e
	rts
	

	
	
	
Bitmap:
	incbin "\ResAll\Sprites\RawAP2.RAW"
	;incbin "\ResAll\Sprites\RawAP2_4col.RAW"
BitmapEnd: