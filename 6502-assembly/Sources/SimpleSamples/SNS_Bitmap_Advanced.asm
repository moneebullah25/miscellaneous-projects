z_Regs 		equ $20

z_HL equ z_Regs
z_L  equ z_Regs
z_H  equ z_Regs+1

z_BC equ z_Regs+2
z_C  equ z_Regs+2
z_B  equ z_Regs+3

z_DE equ z_Regs+4
z_E  equ z_Regs+4
z_D  equ z_Regs+5


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
	stz $2122		;CGDATA

;Color 3
	lda #1		;Color 3
	sta $2121		;CGADD - Colour selection  (15=Font)
		 ;gggrrrrr 
	lda #%00001111	
	sta $2122		;CGDATA - Colour data register
		 ;?bbbbbgg 
	lda #%00111100
	sta $2122		;CGDATA

;Color 3
	lda #2		;Color 3
	sta $2121		;CGADD - Colour selection  (15=Font)
		 ;gggrrrrr 
	lda #%11100000	
	sta $2122		;CGDATA - Colour data register
		 ;?bbbbbgg 
	lda #%01111111
	sta $2122		;CGDATA
	
;Color 3
	lda #3		;Color 3
	sta $2121		;CGADD - Colour selection  (15=Font)
		 ;gggrrrrr 
	lda #%11111111	
	sta $2122		;CGDATA - Colour data register
		 ;?bbbbbgg 
	lda #%01111111
	sta $2122		;CGDATA

;TileDefs	
	;	  i000abcd - I 0=inc on $2118 or $2139 0=$2119 or $213A… abcd=move size
	stz $2115 		;VMAIN - Video port control (Inc on write to $2118)
		

	
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
	
;Turn on the screen	
		; ---S4321 - S=sprites 4-1=enable Bgx
	lda #%00000001	;Turn on BG1
	sta $212C 		;Main screen designation [TM]    
	
	;	  x000bbbb - x=screen disable (1=disable) bbbb=brightness (15=max)
	lda #%00001111	;Screen on
	sta $2100		;INIDISP - Screen display register
	
	
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
	
	
	
	lda #<Bitmap		;Source Bitmap Data
	sta z_L
	lda #>Bitmap
	sta z_H

	lda #<(BitmapEnd-Bitmap);Source Bitmap Data Length
	sta z_C
	lda #>(BitmapEnd-Bitmap)
	sta z_B

	lda #<$1800			;Snes patterns start at $1000
	sta z_E				;each adddress holds 1 word...  
	lda #>$1800			;so each 32 byte tile takes 16 addreses,
	sta z_D				;and tile 128 is at $1800
	jsr DefineTiles		;Define the tile patterns
	
	lda #3				;Start SX
	sta z_b
	lda #3				;Start SY
	sta z_c
	
	ldx #6				;Width in tiles
	ldy #6				;Height in tiles
		
	lda #128			;TileStart

	jsr FillAreaWithTiles ;Draw the tiles to screen
	
	jmp *				;Infinite Loop
	
	
Bitmap:
	incbin "\ResALL\Sprites\RawSNS.RAW"
BitmapEnd:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	


GetVDPScreenPos:	; BC=XYpos	
		lda z_c
		sta z_h			;32 tiles per Y line
		
		lda #0		
		lsr z_h
		ror 
		lsr z_h
		ror 
		lsr z_h
		ror 
		adc z_b 		;Add X line
		sta z_l
		jsr WaitVblank
		
		lda z_l
		sta $2116		;MemL -Video port address [VMADDL/VMADDH]                            
		lda z_h
		sta $2117		;VMDATAL - We're writing bytes in PAIRS!
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	
	;lda #3	;SX
	;sta z_b
	;lda #3	;SY
	;sta z_c
	
	;ldx #6	;WID
	;ldy #6	;HEI
	
	;lda #0	;TileStart
	
FillAreaWithTiles:	
	sta z_d
FillAreaWithTiles_Yagain:
	jsr GetVDPScreenPos
	phx
FillAreaWithTiles_Xagain:
		jsr WaitVblank
		;lda #$00	;vhoppptt
		stz $2119	;VMDATAH - Write first byte to VRAM
		
		lda z_d		;ttttttttt
		sta $2118	;VMDATAL - were set to Autoinc address
					;	on 2118 write
		inc z_d
		dex 
		bne FillAreaWithTiles_Xagain
		inc z_c
	plx
	dey
	bne FillAreaWithTiles_Yagain	
	rts
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
	
	;BC=Bytes
	;DE=Destination Ram
	;HL=Source Bytes	
	
DefineTiles:
	jsr prepareVram	;Get VRAM address
	
	ldx z_C					;B=High byte of count - X=Low byte
	ldy #0
DefineTilesAgain	
	jsr WaitVblank
	lda (z_HL),Y	
	sta $2119		;VMDATAH - Write first byte to VRAM
	iny	
	
	jsr WaitVblank	
	lda (z_HL),Y	
	sta $2118		;VMDATAL - were set to Autoinc address 
	iny
	bne DefineTilesAgainYok
	
	inc z_h				;INC High byte Y=low byte
DefineTilesAgainYok:		
	txa				;Is Low Byte Zero
	bne DefineTilesDecBC_C
	lda z_B				;Are We done
	beq DefineTilesAgainDone
	DEC z_B				;DEC high byte (X is low byte)
DefineTilesDecBC_C:	
	DEx					;Subtract 2 
	DEX					;Since we did 2 bytes
	jmp DefineTilesAgain
DefineTilesAgainDone:
	rts
	
	
prepareVram:			
	jsr WaitVblank
	lda z_e
	sta $2116		;VMADDL - Destination address in VRAM L
	lda z_d
	sta $2117		;VMADDH - Destination address in VRAM H
	rts
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
WaitVblank:
		lda $4212 			;HVBJOY - Status 	
			; xy00000a		- x=vblank state y=hblank state a=joypad ready
		and #%10000000
		beq WaitVblank		;Wait until we get nonzero - this means we're in VBLANK
	rts	
	
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
	
	
	