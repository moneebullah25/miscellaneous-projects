
   ; CRT format cartridge header
   org $7FB0						  
   byte "C64 CARTRIDGE   "	   		; Cartridge Signature
   byte $00,$00,$00,$40			   	; Header length $00000040
   byte $01,$00					    ; Version (1.00)
   byte $00,$00 				 	; Cartridge Type... $0000 = normal
   byte $00						   	; Exrom Status... $00 = none
   byte $00 					   	; Game Line Status... $00 = none
   byte $00,$00,$00,$00,$00,$00    	; Unused
   ;     12345678901234567890123456789012
   byte "CHIBIAKUMAS.COM                 "	; 32 byte cartridge name

   ;******************************
   ; Chip Packet Header ($10)
   ;******************************

   org $7FF0
   byte "CHIP"
   byte $00,$00,$40,$10		;Chip Packet Length $00002010
   byte $00,$00				;Chip type 0 = ROM, 1 = RAM
   byte $00,$00				;Bank Location $0000 = normal cartridge
   byte $80,$00				;Load location $8000
   byte $40,$00				;Rom image size $4000

   org $8000				;Start of ROM
   
   word Startup				;Startup Vector
   word Startup				;Restore Vector
   byte $C3,$C2,$CD,$38,$30	

Startup:

   jsr $FF84		;IOINIT. Initialize CIA's, SID volume; setup memory configuration; set and start interrupt timer.
   jsr $FF87		;RAMTAS. Clear memory addresses $0002-$0101 and $0200-$03FF; run memory test and set start 
						;and end address of BASIC work area accordingly; set screen memory to $0400 and datasette buffer to $033C.
   jsr $FF8A		;RESTOR. Fill vector table at memory addresses $0314-$0333 with default values.
   jsr $FF81		;SCINIT. Initialize VIC; restore default input/output to keyboard/screen; clear screen; set PAL/NTSC switch and interrupt timer.

;Your Program Starts Here!

	lda #$0e			;Full Charset (not just uppercase)
	jsr $ffd2			;CHROUT - Output a character  

	;Load in the address of the Message into the zero page
	lda #>HelloWorld
	sta $21				;H Byte
	lda #<HelloWorld
	sta $20				;L Byte
	
	jsr PrintStr		;Show to the screen
	
	jsr NewLine			;Start a new line
		
	jmp *				;Infinite Loop
	
	
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
	lda #13				;CR (new line)
	jmp PrintChar
	
	
; PrintChar:	;DefaultFont
	; cmp #96				;Check if character >96
	; bcc	PrintCharOK
	; and #%11011111		;Convert to uppercase
; PrintCharOK:
	; jmp $ffd2			;CHROUT - Output a character  
	
	
PrintChar:	;Upper+LowerCase Font
	cmp #64					;Check if character >96
	bcc	PrintCharOKB
	eor #%00100000			;Convert to uppercase
PrintCharOKB:	
	jmp $ffd2				;CHROUT - Output a character  
		
	
HelloWorld:				;Test string to show
	db "Hello World",255
	
	
	
	