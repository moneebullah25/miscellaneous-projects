
z_Regs 		equ $20			;Fake Registers
SPpage 		equ $0100		;Stackpointer Address

	;Basic macros for ASM tasks
	include "\SrcAll\BasicMacros.asm"

* = $1001
		; BASIC program to boot the machine language code
		db $0b, $10, $0a, $00, $9e, $34, $31, $30, $39, $00, $00, $00


	;start of code
	
	lda #0
	lda #>HelloWorld		;Load address of message into Zeropage $20/1
	sta z_h
	lda #<HelloWorld
	sta z_l
	
	jsr PrintStr		;ShowString routine
	jsr Newline
	
	jsr monitor 		;Show registers to screen
	
	jsr MemDump			;Show Some Ram to screen
    word $3000      		;Address to show
    byte $3         		;Lines
	
	rts					;Return to basic
	

HelloWorld:
	db "Hello World",255
	
PrintStr:
        ldy #0				;Reset Y
PrintStr_again:
        lda (z_hl),y		;Read in a character
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
	
PrintChar:	
	cmp #96					;Check if character >96
	bcc	PrintCharOK
	and #%11011111			;Convert to uppercase
PrintCharOK:
	jmp $ffd2
	
		
	
	include "\SrcAll\monitor.asm"			;Debugging tools
	include "\SrcAll\BasicFunctions.asm"	;Basic commands for ASM tasks
	