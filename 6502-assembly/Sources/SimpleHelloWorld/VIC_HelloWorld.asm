
* = $1001
		; BASIC program to boot the machine language code
		db $0b, $10, $0a, $00, $9e, $34, $31, $30, $39, $00, $00, $00


	;start of code $100A
	
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
	
PrintChar:	;DefaultFont
	cmp #96					;Check if character >96
	bcc	PrintCharOK
	and #%11011111			;Convert to uppercase
PrintCharOK:
	jmp $ffd2
	
		
	