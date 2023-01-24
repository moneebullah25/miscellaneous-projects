
	org $BFF0

	db "NES",$1a		;ID
	db $01				;Rom pages (16k each)
	db $0				;CHR-ROM pages
	db %01000010		;mmmmFTBM		mmmm = mapper no bottom 4 bits , Four screen vram layout, Trainer at &7000, Battery ram at &6000, Mirror (0=horiz, 1=vert)
	db %00000000		;mmmm--PV 		mapper (top 4 bits...  Pc10 arcade, Vs unisystem )
	db 0				;Ram pages
	db 0,0,0,0,0,0,0
						;We selected Mapper 4 - it has 8k VRAM , 8K Sram and 128k rom
	
Cursor_X 	equ $40		;Position of next printed character
Cursor_Y 	equ Cursor_X+1

vblanked 	equ $7F		;Zero page address of Vblank count

nmihandler:		;This procuedure runs after each frame (See footer.asm)
	php
	inc vblanked		;Alter Vblank Zero page entry
	plp
irqhandler:	;Do nothing
	rti


	

ProgramStart:
	sei					;Interrupts off
	cld					;Clear Decimal flag
	
	ldx #$ff			;Set up stack
	txs
	
	lda #%10000000		;Turn on extra ram at $6000-%7FFF
	sta $A001


	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
;ScreenInit
	lda #$00		;Reset Cursor pas
	sta Cursor_X 
	sta Cursor_Y
	tay 
	
	;Pattern table &0000
	sta $2006		;PPUADDR H
	sta $2006		;PPUADDR L
	
	lda #BitmapFont&255	;Address of font
	sta $20
	lda #BitmapFont/256
	sta $21
		
	ldx #3	;Y=0 (768 lines total)
fontchar_loop:
	txa
	pha	
fontchar_loop2:
		tya
		pha
			jsr Font_DoBitplane	;Bitplane 0
		pla
		tay
		jsr Font_DoBitplane		;Bitplane 1
		
		tya
		bne fontchar_loop2		;Repeat until Y=0
	inc $21
	pla
	tax
	dex
	bne fontchar_loop
	
	
;Palette
	lda #$3F		;Select Palette ram &3F00
	sta $2006		;PPUADDR H
	txa ;X=0
	sta $2006		;PPUADDR L
	
	ldx #4
PaletteAgain
	lda Palette-1,x 
	sta $2007		;PPUDATA
	dex 
	bne PaletteAgain

;Turn ON the screen
	lda #%00011110 	;(Sprite enable/back enable/Sprite leftstrip / backleftstrip)
	sta $2001		;PPUMASK
	
	lda #$80		;NMI enable (Vblank)
	sta $2000		;PPUCTRL - VPHB SINN

	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	
;Load in the address of the Message into the zero page
	lda #>HelloWorld
	sta $21				;H Byte
	lda #<HelloWorld
	sta $20				;L Byte
	
	jsr PrintStr		;Show to the screen
	
	jsr NewLine			;Start a new line
				
	jmp *				;Infinite loop

HelloWorld:				;255 terminated string
	db "Hello World",255
		
Palette:  
; 	Color   3   2   1  0
		db $38,$21,$15,$02

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
PrintStr:
	ldy #0				;Set Y to zero
PrintStr_again:
	lda ($20),y			;Load a character from addr in $20+Y 
	
	cmp #255			;If we got 255, we're done
	beq PrintStr_Done
	
	jsr PrintChar		;Print Character
	iny					;Inc Y and repeat
	jmp PrintStr_again
PrintStr_Done:
	rts	

NewLine:
	lda #0
	sta Cursor_X	
	inc Cursor_Y
	rts
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
Font_DoBitplane:
	ldx #8			;8 bytes per tile bitplane
FontFillAgain_Plane1:	
	lda ($20),y
	sta $2007		;Write data to data-port 
	iny
	dex 
	bne FontFillAgain_Plane1
	rts

Bitmapfont:			;Chibiakumas bitmap font (1bpp B/W)
	incbin "\ResALL\Font96.FNT"	

		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
		
	
PrintChar:
	sec
	sbc #32			;No character below 32 in our font
	sta $26
	
	txa
	pha
	tya
	pha	
		;Address= $2000 + (Ypos*32) + Xpos
		lda Cursor_Y
		asl				;%00000111
		asl
		asl
		asl
		asl				;%11100000
		ora Cursor_X
		tay				;L Byte
		
		lda Cursor_Y
		lsr				;%11111000
		lsr
		lsr				;%00011111
		clc
		adc #$20		;Tilemap Base (Nametable) = $2000
		tax				;Hbyte
			
		jsr waitframe	;Can only Write to VRAM in Vblank
		
		stx $2006		;PPUADDR High byte
		sty $2006		;PPUADDR Low byte
		
		lda $26			;Write Tile Number to VRAM
		sta $2007		;PPUDATA
		
		;Need to reset scroll each write	
		lda #0			;Scroll X
		sta $2005		;PPUSCROLL
		lda #0-8		;Scroll y
		sta $2005		;PPUSCROLL
		
		inc Cursor_X	;Move across screen
		lda Cursor_X
		cmp #32			;At end of line?
		bne PrintChar_NotNextLine
		jsr NewLine
PrintChar_NotNextLine:	
	pla
	tay
	pla
	tax
	rts

	


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
waitframe:
	pha
		lda #$00
		sta vblanked		;Zero Vblanked
waitloop:
		lda vblanked		;Wait for the interrupt to change it
		beq waitloop
	pla
	rts
	
	
	
;Cartridge Footer	
	org $FFFA
	dw nmihandler			;FFFA - Interrupt handler
	dw ProgramStart			;FFFC - Entry point
	dw irqhandler			;FFFE - IRQ Handler
	
	
	
	
	
	
	