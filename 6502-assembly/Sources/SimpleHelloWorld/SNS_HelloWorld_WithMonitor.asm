

z_Regs 		equ $20
Cursor_X 	equ $40
Cursor_Y 	equ Cursor_X+1
  
sppage equ $0100
UserRam equ $0200

	include "\SrcAll\BasicMacros.asm" ;Basic macros for ASM tasks

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		
	org $8000		;Start of ROM
	SEI				;Stop interrupts
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		
;ScreenInit

		 ;aaaabbbb -aaa=base addr for BG2 bbb=base addr for BG1
	lda #%00010001
	sta $210B 		;BG1 & BG2 VRAM location register [BG12NBA]                    
	
	;     xxxxxxss 	- xxx=address… ss=SC size  00=32x32 01=64x32 10=32x64 11=64x64
	stz $2107		;BG1SC - BG1 Tilemap VRAM location
	
	; abcdefff - abcd=tile sizes e=pri fff=mode def
	lda #%00001001
	sta $2105		;BGMODE - Screen mode register
	
	;	  x000bbbb - x=screen disable (1=disable) bbbb=brightness (15=max)
	lda #%10000000	;Screen off
	sta $2100		;INIDISP - Screen display register

	
;PaletteDefs	

;Background (Color 0)
	stz $2121		;CGADD - Colour selection  (0=Back)
		 ;gggrrrrr 
	stz $2122		;CGDATA - Colour data register
		 ;?bbbbbgg 
	lda #%00111100	
	sta $2122		;CGDATA
;Font (Color 15)
	lda #15			;Color 15=Font
	sta $2121		;CGADD - Colour selection  (15=Font)
		 ;gggrrrrr 
	lda #%11111111	
	sta $2122		;CGDATA - Colour data register
		 ;?bbbbbgg 
	lda #%00000011	
	sta $2122		;CGDATA

;TileDefs	
	;	  i000abcd - I 1=inc on $2118 or $2139 0=$2119 or $213A… abcd=move size
	stz $2115 		;VMAIN - Video port control (Inc on write to $2118)
		

;load_font
	stz $2116		;VRAM MemL
	lda #$10
	sta $2117		;VRAM MemH
	
	lda #BitmapFont&255
	sta $20
	lda #BitmapFont/256
	sta $21
	
	ldx #3			;96 sprites * 8 lines = 768 
	ldy #0
fontchar_loopx:
	phx
fontchar_loop:
		phy
			jsr Font_DoBitplane	;Bitplane 0+1
		ply
		jsr Font_DoBitplane		;Bitplane 2+3
		tya
		bne fontchar_loop
		inc $21					;Inc to byte of address
	plx 
	dex
	bne fontchar_loopx
	
	
;Set Scroll position
	stz $210D  		;BG1HOFS BG1 horizontal scroll   
	stz $210D  		;BG1HOFS
	
	lda #-1
	sta $210E  		;BG1VOFS BG1 vertical scroll 
	stz $210E  		;BG1VOFS

;Clear Screen 		
	stz $2116		;MemL -Video port address [VMADDL/VMADDH]                            
	stz $2117		;MemH

	ldy #4			;Tilemap Size: 32*32 = 1024
	ldx #0
ClearTilemap:
	stz $2119		;Zero all Tiles in Tilemap
	stz $2118
	dex
	bne ClearTilemap
	dey
	bne ClearTilemap
	
	stz Cursor_X 	;Reset Curosr Pos
	stz Cursor_Y
	
;Turn on the screen	
		; ---S4321 - S=sprites 4-1=enable Bgx
	lda #%00000001	;Turn on BG1
	sta $212C 		;Main screen designation [TM]    
	
	;	  x000bbbb - x=screen disable (1=disable) bbbb=brightness (15=max)
	lda #%00001111	;Screen on
	sta $2100		;INIDISP - Screen display register
	
	
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
	
;Load in the address of the Message into the zero page
	lda #>HelloWorld
	sta $21				;H Byte
	lda #<HelloWorld
	sta $20				;L Byte
	
	jsr PrintStr		;Show to the screen
	
	jsr NewLine			;Start a new line
	
	lda #>HelloWorld
	sta $21				;H Byte
	lda #<HelloWorld
	sta $20				;L Byte
	
	jsr PrintStr		;Show to the screen
	
	jsr NewLine			;Start a new line
	
	
	
	jsr monitor 		;Show registers to screen
	
	jsr MemDump			;Show Some Ram to screen
    word $3000      		;Address to show
    byte $10         		;Lines
	
	
	jmp *				;Infinite Loop
	
	
	

HelloWorld:
	db "Hello World",255	
	
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		
	
	
Font_DoBitplane:
		ldx #8				;8 Lines 
fontchar_loopL:
		lda ($20),y
		sta $2119			;Write Word data to data-port
		sta $2118			
		iny
		dex 
		bne fontchar_loopL
		rts
	
Bitmapfont:					;Chibiakumas bitmap font
	incbin "\ResALL\Font96.FNT"
		

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

WaitVblank:
	pha
WaitVblank2:
		lda $4212 			;HVBJOY - Status 	
			; xy00000a		- x=vblank state y=hblank state a=joypad ready
		and #%10000000
		beq WaitVblank2		;Wait until we get nonzero - this means we're in VBLANK
	pla
	rts	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
  
PrintChar:
	sec
	sbc #32				;No Charactrers below 32 in our font
	phx
	phy
		pha
			ldx $21			;Backup $21
;Address= (Ypos*32) + Xpos			
			lda Cursor_Y 	
			sta $21			;%YYYYYYYY 00000000
			lda #0
			lsr $21			;%0YYYYYYY Y00000000
			ror 
			lsr $21			;%00YYYYYY YY00000000
			ror 
			lsr $21			;%000YYYYY YYY0000000
			ror 
			adc Cursor_X 	
			
			jsr WaitVblank	;Can only write to vram during Vblank
			sta $2116		;MemL -Video port address [VMADDL/VMADDH]                            
			lda $21
			sta $2117		;MemH			
		pla
		stz $2119		;Top Tile Byte (0)
		sta $2118		;Bottom Tile Byte (TileNum)
		
		inc Cursor_X
		lda Cursor_X
		cmp #32			;Tilemap is 32 tiles wide
		bne PrintChar_NotNextLine
		jsr NewLine
PrintChar_NotNextLine:
		stx $21			;Resotre $21
	ply
	plx
	rts
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
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
	stz Cursor_X		;Clear Xpos
	inc Cursor_Y		;Increase Ypos
	rts	
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	
	include "\SrcAll\BasicFunctions.asm"	;Basic commands for ASM tasks
	
	;Debugging tools
	include "\SrcAll\monitor.asm"	
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;	Footer
	
	org $FFC0
     ; "123456789012345678901"
	db "www.ChibiAkumas.com  "	; Program title (21 byte Ascii string)

	db $20		;Rom mode/speed (bits 7-4 = speed, bits 3-0 = map mode)
	db $00		;Rom type (bits 7-4 = co-processor, bits 3-0 = type)
	db $01 		;Rom size in banks (1bank=32k)
	db $00 		;Ram size (0=none)
	db $00		;Country/video refresh (ntsc 60hz, pal 50hz) (0=j 1=us/eu)
	db $00		;Developer id code
	db $00		;Rom version number
	db "cc"		;Complement check
	db "cs" 	;Checksum

;65816 mode vectors
	dw $0000 	;Reserved
	dw $0000 	;Reserved
	dw $0000 	;Cop vector   (cop opcode)
	dw $0000 	;Brk vector   (brk opcode)
	dw $0000 	;Abort vector (unused)
	dw $0000	;Vblank interrupt handler
	dw $0000 	;Reset vector (unused)
	dw $0000 	;Irq vector   (h/v-timer/external interrupt)

;6502 mode vectors
	dw $0000 	;Reserved
	dw $0000	;Reserved
	dw $0000 	;Cop vector   (cop opcode)
	dw $0000 	;Brk vector   (unused)
	dw $0000 	;Abort vector (unused)
	dw $0000	;Vblank interrupt handler
	dw $8000 	;Reset vector (cpu is always in 6502 mode on reset)
	dw $0000 	;Irq/brk vector
	
	
	