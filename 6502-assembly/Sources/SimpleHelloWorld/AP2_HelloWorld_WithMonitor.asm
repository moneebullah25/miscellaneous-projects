
	;Basic macros for ASM tasks
	include "\SrcAll\BasicMacros.asm"

z_Regs equ $40
SPpage equ $0100


NewLine equ $FC62	;CR - Carriage Return to Screen

	
	ORG $0C00	;Program Start
	
	jsr NewLine			;Start a new line

	
	;Load in the address of the Message into the zero page
	lda #>HelloWorld
	sta $41				;H Byte
	lda #<HelloWorld
	sta $40				;L Byte
		
	jsr PrintStr		;Show to the screen
	

	jsr NewLine			;Start a new line
		
	jsr monitor 		;Show registers to screen

	jsr MemDump			;Show Some Ram to screen
	word $3000      		;Address to show
	byte $3         		;Lines
	
	lda #0
	rts
	
PrintChar:
	pha
		clc
		adc #128		;Correction for weird character map!
		jsr $FDF0		;COUT1 - Output Character to Screen
	pla
	rts
	
				

PrintStr:
	ldy #0				;Set Y to zero
PrintStr_again:
	lda ($40),y			;Load a character from addr in $20+Y 
	
	cmp #255			;If we got 255, we're done
	beq PrintStr_Done
	
	jsr PrintChar		;Print Character
	iny					;Inc Y and repeat
	jmp PrintStr_again
PrintStr_Done:
	rts	

	
HelloWorld:				;255 terminated string
	db "Hello World",255

	
	
	;Debugging tools
	include "\SrcAll\monitor.asm"		
	;Basic commands for ASM tasks
	include "\SrcAll\BasicFunctions.asm"	
	
	