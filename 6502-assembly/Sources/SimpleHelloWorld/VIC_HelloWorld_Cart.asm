	
z_Regs 		equ $20			;Fake Registers
SPpage 		equ $0100		;Stackpointer Address

	;Basic macros for ASM tasks
	include "\SrcAll\BasicMacros.asm"
	
* = $A000
	;Cartridge Header 
	dw ProgramStart
	dw ProgramStart
	db $41,$30,$C3,$C2,$CD		;ROM Header
ProgramStart:

	;Initialise hardware (Basic normally does this)
	JSR	$FD8D		;RAMTAS - Initialise System Constants
	JSR	$FD52		;RESTOR - Restore Kernal Vectors (at 0314)
	JSR	$FDF9		;IOINIT - Initialize I/O registers
	JSR	$E518		;CINT1  - Initialize I/O

; Start of our program	
	
	lda #>HelloWorld;Load address of message into Zeropage $20/1
	sta z_h
	lda #<HelloWorld
	sta z_l
	
	jsr PrintStr	;ShowString routine
	jsr Newline
	jsr Newline
	
	
	jsr monitor 	;Show registers to screen
	
	jsr MemDump		;Show Some Ram to screen
    word $3000      	;Address to show
    byte $3         	;Lines
	
	jmp *			;Halt CPU

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
	jmp PrintChar			;Chr 13 for newline
	
PrintChar:	
	cmp #96					;Check if character >96
	bcc	PrintCharOK
	and #%11011111			;Convert to uppercase
PrintCharOK:
	jmp $ffd2
	
		
	
	include "\SrcAll\monitor.asm"			;Debugging tools
	include "\SrcAll\BasicFunctions.asm"	;Basic commands for ASM tasks
	