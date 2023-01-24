
	;Basic program to execute our ASM binary
*=$0401
	db $0e,$04,$0a,$00,$9e,$20,$28, $31,$30,$34,$30,$29,$00,$00,$00
;org $0410

	
	lda #0
	lda #>HelloWorld		;Load address of message into Zeropage $20/1
	sta $21
	lda #<HelloWorld
	sta $20
	
	jsr PrintStr			;ShowString routine
	jsr Newline
	jsr Newline
	
	
	rts						;Return to basic
	

HelloWorld:
	db "Hello World",255
	
PrintStr:
        ldy #0				;Reset Y
PrintStr_again:
        lda ($20),y		;Read in a character
        
		cmp #255
        beq PrintStr_Done	;Return if we've reached a 255
        
		jsr PrintChar		;Print to screen if not
        iny
        jmp PrintStr_again	;repeat
PrintStr_Done:
        rts					;Return when done					
		
NewLine:
	lda #13
	jmp PrintChar
	
PrintChar:		;DefaultFont
	cmp #96					;Check if character >96
	bcc	PrintCharOK
	and #%11011111			;Convert to uppercase
PrintCharOK:
	jmp $ffd2				;CHROUT - Output a character
	
		
	