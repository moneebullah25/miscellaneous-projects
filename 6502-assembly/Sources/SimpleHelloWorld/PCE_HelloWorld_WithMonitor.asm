
	include "\SrcAll\BasicMacros.asm" ;Basic macros for ASM tasks

z_Regs 		equ $60		;Fake Registers
SPpage		equ $2100 	;StackPointer is at an odd address on the PCE!
Cursor_X 	equ $40		;Used for Printchar
Cursor_Y 	equ $41

	org $e000		;bank $0
	
ProgramStart:
	sei				;Disable interrupts
	csh				;Highspeed Mode
	cld				;Clear Decimal mode
	
	lda #$f8		;map in RAM
	tam #%00000010	;TAM1 (2000-3FFF)

	lda #$ff		;map in I/O (#$ff)
	tam #%00000001	;TAM0 (0000-1FFF)
	tax				
	txs				;Init stack pointer
		
		;      T12 - TIQ, IRQ1, IRQ2
	lda #%00000111
	sta $1402		;IRQ mask... 1=Off
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		

;	ScreenInit
	st0 #5				;RegSelect 5
		 ;BSXXIIII	Backgroundon Spriteon eXtendedsync Interruptenable
	st1 #%10000000		;Background ON, Sprites On
	st2 #0
	
	st0 #9			
		; 0BBB0000
	st1 #%00000000		;BACKGROUND Tilemap size (32x32)
	st2 #0
	
;Reset Background scroll registers
	st0 #7				;Background X-scroll (------XX XXXXXXXX)
	st1 #0
	st2 #0
	
	st0 #8				;Background Y-scroll (-------Y YYYYYYYY)
	st1 #248			;Move Byte pos 0 to top left of screen 
	st2 #0				
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
;	Init Palette

;Background Color
	stz $0402			;Palette address L
	stz $0403			;Palette address H
	
	lda #%00000111		 ;GGRRRBBB
	sta $0404				
	stz $0405			;-------G
	
;Font color
	lda #15		
	sta $0402			;Palette address L
	stz $0403			;Palette address H
	lda #%11111000			
	sta $0404			;GGRRRBBB
	lda #%00000001
	sta $0405			;-------G

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
;	Init Font

	st0 #0		;set Address reg to $1000 
	st1 #$00		;we'll put our font there (tiles 256+)
	st2 #$10
	
	st0 #2				;Select Data reg
	
	lda #>Bitmapfont	;Address of our font
    sta $61
    lda #<Bitmapfont
	sta $60
	ldx #3				;96*8=256*3
	ldy #0	
FontNextChar:
	phx	
		phy
			jsr FontPart ;Do Bitplanes 0/1
		ply
		jsr FontPart	 ;Do Bitplanes 2/3
	plx
	cpy #0
	BNE FontNextChar
	inc $61
	dex 
	bne FontNextChar
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
;	Cls

	st0 #0			;VDP reg 0 (address)
	st1 #$00		;L - Start of tilemap $0000
	st2 #$00		;H
	
	st0 #2			;Select VDP Reg2 (data)	
	
	ldx #4
	ldy #0			;1024 tiles total (32x32)
ClsAgain:	
	st1 #0			;Fill the entire area with our "Space tile"
	st2 #%00000001		;(tile 256)
	dey
	bne ClsAgain
	dex 
	bne ClsAgain
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		

;Load in the address of the Message into the zero page
	lda #>HelloWorld
	sta $61				;H Byte
	lda #<HelloWorld
	sta $60				;L Byte
		
	jsr PrintStr		;Show to the screen		
		
	jsr NewLine			;Start a new line
	
	
	
	
	jsr monitor 		;Show registers to screen
	
	jsr MemDump			;Show Some Ram to screen
    word $3000      		;Address to show
    byte $10         		;Lines
	
	
	jmp *				;Infinite Loop
		
HelloWorld:
	db "Hello World",255		
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		

FontPart:
	ldx #8 
FontPartAgain:
	lda ($60),Y	
	;sta_00 $02		;I use my macro here - I need to write to VramDataWrite at $0002
	sta $0102		;This does not work, as the CPU redirects it to $2002
	sta $0103		;just set second plane to 0
	iny
	dex
	BNE FontPartAgain		;Write the first 8 lines 
	rts
	
Bitmapfont:								
	incbin "\ResALL\Font96.FNT"			
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		
				
PrintChar:
	pha
		st0 #0			;Reg0=Select Addr
;Address=(Ypos *32) + X
		lda Cursor_Y
		asl				;%00000111
		asl
		asl
		asl
		asl				;%11100000
		ora Cursor_X
		sta $0102		;Address L
		
		lda Cursor_Y
		lsr				;%11111000
		lsr
		lsr				;%00011111
		sta $0103		;Address H
	pla
	pha
		st0 #2			;Reg2=Write Byte Data
		sec
		sbc #32		;We have no characters below 32
		
		sta $0102		;Store Char 
		st2 #%00000001	;Font Tile are 256+			
		
		inc Cursor_X
		lda Cursor_X
		cmp #32			;Are we at end of line
		bne PrintChar_NotNextLine
		jsr NewLine
PrintChar_NotNextLine:	
	pla
	rts
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		

	
PrintStr:
	ldy #0				;Set Y to zero
PrintStr_again:
	lda ($60),y			;Load a character from addr in $60+Y 
	
	cmp #255			;If we got 255, we're done
	beq PrintStr_Done
	
	jsr PrintChar		;Print Character
	iny					;Inc Y and repeat
	jmp PrintStr_again
PrintStr_Done:
	rts	
	
NewLine:
	stz Cursor_X		;Clear Xpos
	inc Cursor_Y		;Increase Ypos
	rts	
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	
	include "\SrcAll\BasicFunctions.asm"	;Basic commands for ASM tasks
	
	;Debugging tools
	include "\SrcAll\monitor.asm"	
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	org $fffe
	dw ProgramStart			;Reset Vector 
	