
	org $200-10
	db $80,$08,$02,$00,$40,$0A,$42,$53,$39,$33
	;Our program starts at $0200
	
	
	
z_Regs 		equ $20

z_HL  equ z_Regs		;Zeropage Values for our use
z_L   equ z_Regs
z_H   equ z_Regs+1
z_DE  equ z_Regs+4
z_E   equ z_Regs+4
z_D   equ z_Regs+5
z_As  equ z_Regs+6
z_ixl equ z_Regs+8	

Cursor_X 	equ $40		;Text position for next char
Cursor_Y 	equ Cursor_X+1

;ScreenInit	-	SUZY chip needs low byte setting first 
					;OR IT WILL WIPE THE HIGH BYTE!
	
	;Set screen ram pointer to $C000
	stz $FD94		;DISPADR	Display Address L (Visible)
	lda #$C0	
	sta $FD95		;DISPADR	Display Address H (Visible)
	
	
	;Do the palette
	;lda #%00000000	;Palette Color 0 ----GGGG
	stz $FDA0
	lda #%01110000	;Palette Color 0 BBBBRRRR
	sta $FDB0
	
	lda #%00001111	;Palette Color 15 ----GGGG
	sta $FDAF
	;lda #%00001111	;Palette Color 15 BBBBRRRR
	sta $FDBF
	
	
	
	
	

	;Load in the address of the Message into the zero page
	lda #>HelloWorld
	sta z_h				;H Byte
	lda #<HelloWorld
	sta z_l				;L Byte
		
	jsr PrintStr		;Show to the screen		
	
	jsr NewLine			;Start a new line
	jsr PrintStr		;Show to the screen		
	
	jmp *					;Infinite Loop
	
HelloWorld:
	db "Hello World",255
		
	
	
	
PrintStr:
	ldy #0				;Set Y to zero
PrintStr_again:
	lda (z_hl),y			;Load a character from addr in $20+Y 
	
	cmp #255			;If we got 255, we're done
	beq PrintStr_Done
	
	jsr PrintChar		;Print Character
	iny					;Inc Y and repeat
	jmp PrintStr_again
PrintStr_Done:
	rts	

NewLine:		
		stz Cursor_X	;Zero X
		inc Cursor_Y	;Increase Y
	rts
	
	
	
PrintChar:
	sec
	sbc #32				;No Characters below 32 in our font
	
	phx
	phy
	ldx z_h				;Back up registers and Zeropage
	phx
	ldx z_l
	phx	
	;Calculate font pos = BitmapFont + ( (Char-32) *8)
		
		stz z_H			;%00000000 00000001
		asl 
		rol z_H			;%00000000 00000010
		asl 
		rol z_H			;%00000000 00000100
		asl 
		rol z_H			;%00000000 00001000
		clc
		adc #<BitmapFont
		sta z_l
		
		lda z_h
		adc #>BitmapFont
		sta z_h
	
;ScreenAddr=$C000 + ($280*CursorY) + CursorX
	
		stz z_e
					
		lda Cursor_Y ;Ypos*$280 (Ypos * %00000010 10000000)
		lsr 
		ror z_e
		sta z_d
		
		lda Cursor_Y
		asl
		adc z_d
		sta z_d
		
		lda Cursor_X	;Add Xpos
		asl
		asl
		clc
		adc z_e
		sta z_e
		
		lda z_d
		adc #$C0		;ScreenBase=$C000
		sta z_d
		
		ldy #0
nextFontLine:
		phy
			lda (z_HL),y	;Get Byte from font %76543210
			ldy #00
			sta z_AS
MoreFontLine:
			lda #0
			rol z_As	;Shift a Bit out %6543210- 7
			rol 			;%-------7
			asl 			;%------7-
			asl 			;%-----7--
			asl 			;%----7---
			rol z_As	;Shift a Bit out %543210-- 6
			rol 			;%---7---6	
			sta z_ixl
			asl				;%--7---6-
			ora z_ixl		;%--77--66
			asl				;%-77--66-
			ora z_ixl		;%-777-666
			asl				;%777-666-
			ora z_ixl		;%77776666
			sta (z_DE),Y ;Write byte to screen
			iny
			cpy #4
			bne MoreFontLine
			clc
			lda #$50		;Move Down 1 Line ($50 Bytes)
			adc z_e
			sta z_e
			lda #0
			adc z_d
			sta z_d
SkipFontLine:
		ply
		iny
		cpy #8				;Next Y line of character
		bne nextFontLine
			
		inc Cursor_X		;Move Across screen
		lda Cursor_X
		cmp #20				;Start a new line if at end of screen
		bne PrintChar_NotNextLine
		jsr NewLine
PrintChar_NotNextLine:
	plx
	stx z_l					;Restore Registers and Zero Page
	plx
	stx z_h	
	ply
	plx
	rts

Bitmapfont:			;Chibiakumas bitmap font (1bpp)
	incbin "\ResALL\Font96.FNT"	
	