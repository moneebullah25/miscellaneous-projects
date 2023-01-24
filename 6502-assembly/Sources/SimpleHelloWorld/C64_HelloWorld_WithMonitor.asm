
z_Regs 		equ $20			;Fake Registers
SPpage 		equ $0100		;Stackpointer Address

	;Basic macros for ASM tasks
	include "\SrcAll\BasicMacros.asm"
	

	;Init Routine
*=$0801
	db	$0E, $08, $0A, $00, $9E, $20, $28,  $32, $30, $36, $34, $29, $00, $00, $00  
*=$0810	;Start at $0810


	;lda #$0e				;Full Charset (not just uppercase)
	;jsr $ffd2				;CHROUT - Output a character  
	
	;Load in the address of the Message into the zero page
	lda #>HelloWorld
	sta $21				;H Byte
	lda #<HelloWorld
	sta $20				;L Byte
	
	jsr PrintStr		;Show to the screen
	
	jsr NewLine			;Start a new line
		
	jsr monitor 		;Show registers to screen
	
	jsr MemDump			;Show Some Ram to screen
    word $0800      	;Address to show
    byte $3         	;Lines
	
	rts					;Return to basic
	
PrintStr:
	ldy #0				;Reset Y
PrintStr_again:
	lda ($20),y			;Read in a character
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
	jmp $ffd2				;CHROUT - Output a character  
	

PrintChar2:	;Upper+LowerCase Font
	cmp #64					;Check if character >96
	bcc	PrintCharOKB
	eor #%00100000			;Convert to uppercase
PrintCharOKB:	
	jmp $ffd2				;CHROUT - Output a character  
		
	
HelloWorld:					;Test string to show
	db "Hello World",255
		
	include "\SrcAll\monitor.asm"			;Debugging tools
	include "\SrcAll\BasicFunctions.asm"	;Basic commands for ASM tasks
	