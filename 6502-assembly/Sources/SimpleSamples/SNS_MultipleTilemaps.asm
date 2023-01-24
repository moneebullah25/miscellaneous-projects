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


TilemapBase  equ z_Regs+6



	org $8000		;Start of ROM
	SEI				;Stop interrupts
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		
;ScreenInit

;Set patterns to VRAM $2000

		 ;aaaabbbb -aaa=base addr for BG2 bbb=base addr for BG1 
					;(* $2000 bytes / $1000 words) 
	lda #%00100010	;Patterns=$2000
	sta $210B 		;BG1 & BG2 Pattern VRAM location register [BG12NBA]                    
	sta $210C 		;BG3 & BG4 Pattern VRAM location register [BG12NBA]                    
	

;Lets define Tilemap addresses for BG1 / BG2 / BG3
	
	;     xxxxxxss 	- xxx=address *$800 bytes ($400 words) 
					 ;ss=SC size  00=32x32 01=64x32 10=32x64 11=64x64
	lda #%00000000	;Tilemap=$0000
	sta $2107		;BG1SC - BG1 Tilemap VRAM location
	lda #%00000100	;Tilemap=$0400
	sta $2108		;BG1SC - BG2 Tilemap VRAM location
	lda #%00001000 	;Tilemap=$0800
	sta $2109		;BG1SC - BG3 Tilemap VRAM location
	
	
	    ; abcdefff - abcd=tile sizes (BG1234) e=pri fff=mode def
	lda #%00001001
	sta $2105		;BGMODE - Screen mode register 
	;Mode 1= BG1/BG2 16 color ... BG3 4 color ... BG4 Not available
	;Only mode 0 has 4 background layers - but all are 4 color
	
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

;Color 1
	lda #1		;Color 1
	sta $2121		;CGADD - Colour selection  (15=Font)
		 ;gggrrrrr 
	lda #%00001111	
	sta $2122		;CGDATA - Colour data register
		 ;?bbbbbgg 
	lda #%00111100
	sta $2122		;CGDATA

;Color 2
	lda #2		;Color 2
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
		

	
;Set Scroll position (Must write each value twice First L byte, second H byte)
	stz $210D  		;BG1HOFS BG1 horizontal scroll   
	stz $210D
	lda #-1
	sta $210E  		;BG1VOFS BG1 vertical scroll 
	stz $210E
	
	stz $210F  		;BG2HOFS BG2 horizontal scroll   
	stz $210F
	lda #-1
	sta $2110  		;BG2VOFS BG2 vertical scroll 
	stz $2110
	
	stz $2111  		;BG3HOFS BG3 horizontal scroll   
	stz $2111
	lda #-1
	sta $2112  		;BG3VOFS BG3 vertical scroll 
	stz $2112

	
;Clear Screen  BG1 ($0000)
	ldx #$00
	ldy #$00
	jsr ClearTilemap	;clear $0000
;Clear Screen  BG2
	ldx #$04
	ldy #$00
	jsr ClearTilemap	;clear $0400
;Clear Screen  BG3
	ldx #$08
	ldy #$00
	jsr ClearTilemap	;clear $0800

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
	
	
;16 color Chibiko Bitmap ($2800=16 color Tile 128)
	lda #<Bitmap		;Source Bitmap Data
	sta z_L
	lda #>Bitmap
	sta z_H

	lda #<(BitmapEnd-Bitmap);Source Bitmap Data Length
	sta z_C
	lda #>(BitmapEnd-Bitmap)
	sta z_B

	lda #<$2800			;Snes patterns start at $1000
	sta z_E				;each adddress holds 1 word...  
	lda #>$2800			;so each 32 byte 8-color tile takes 16 addreses,
	sta z_D				;and tile 128 is at $2800
	jsr DefineTiles		;Define the tile patterns
	
;4 color Reida Bitmap ($2400=4 color Tile 128)
	lda #<Bitmap4Col		;Source Bitmap Data
	sta z_L
	lda #>Bitmap4Col
	sta z_H

	lda #<(Bitmap4ColEnd-Bitmap4Col);Source Bitmap Data Length
	sta z_C
	lda #>(Bitmap4ColEnd-Bitmap4Col)
	sta z_B

	lda #<$2400			;Snes patterns start at $2000
	sta z_E				;each adddress holds 1 word...  
	lda #>$2400			;so each 16 byte 4-color tile takes 8 addreses
	sta z_D				;and tile 128 is at $2400
	jsr DefineTiles		;Define the tile patterns
	

	
;Empty Tile 0	
	lda #<Blank		;Source Bitmap Data
	sta z_L
	lda #>Blank
	sta z_H

	lda #<(BlankEnd-Blank);Source Bitmap Data Length
	sta z_C
	lda #>(BlankEnd-Blank)
	sta z_B

	lda #<$2000			;Snes patterns start at $1000
	sta z_E				;each adddress holds 1 word...  
	lda #>$2000			;so each 32 byte tile takes 16 addreses,
	sta z_D				;and tile 128 is at $1800
	jsr DefineTiles		;Define the tile patterns
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
	
;Turn on the screen	
		; ---S4321 - S=sprites 4-1=enable Bgx
	lda #%00000111	;Turn on BG1
	sta $212C 		;Main screen designation [TM]    
	
	;	  x000bbbb - x=screen disable (1=disable) bbbb=brightness (15=max)
	lda #%00001111	;Screen on
	sta $2100		;INIDISP - Screen display register
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
;Draw Chibiko 1
	lda #$00			;Tilemap $0000
	sta TilemapBase	
	lda #3				;Start SX
	sta z_b
	lda #3				;Start SY
	sta z_c
	ldx #6				;Width in tiles
	ldy #6				;Height in tiles
	lda #128			;TileStart
	jsr FillAreaWithTiles ;Draw the tiles to screen
	
;Draw Chibiko 2
	lda #$04			;Tilemap $0400
	sta TilemapBase
	lda #13				;Start SX
	sta z_b
	lda #13				;Start SY
	sta z_c
	ldx #6				;Width in tiles
	ldy #6				;Height in tiles
	lda #128			;TileStart
	jsr FillAreaWithTiles ;Draw the tiles to screen
	
;Draw Reida (4 color)
	lda #$08			;Tilemap $0800
	sta TilemapBase
	lda #13				;Start SX
	sta z_b
	lda #03				;Start SY
	sta z_c
	ldx #6				;Width in tiles
	ldy #8				;Height in tiles
	lda #128			;TileStart
	jsr FillAreaWithTiles ;Draw the tiles to screen
	
InfLoop:	
	dex
	iny
	
	stx $210D				;BG1-Xpos L
	stz $210D				;BG1-Xpos H
	;sty $210E				;BG1-Ypos L
	;stz $210E				;BG1-Ypos H
	
	;stx $210F				;BG2-Xpos L
	;stz $210F				;BG2-Xpos H
	sty $2110				;BG2-Ypos L
	stz $2110				;BG2-Ypos H

	;stx $2111				;BG3-Xpos L
	;stz $2111				;BG3-Xpos H
	;sty $2112				;BG3-Ypos L
	;stz $2112				;BG3-Ypos H
	
	jsr WaitFrame
	jmp InfLoop				;Infinite Loop
		

Blank:
	ds 32
BlankEnd:
Bitmap:
	
	incbin "\ResALL\Sprites\RawSNS.RAW"
BitmapEnd:
Bitmap4Col:
	incbin "\ResALL\Sprites\BitmapSns4color.RAW"
Bitmap4ColEnd:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
ClearTilemap:		;Clear 32x32 from XY onwards
	sty $2116		;MemL -Video port address [VMADDL/VMADDH]                            
	stx $2117		;MemH

	ldy #4			;Tilemap Size: 32*32 = 1024
	ldx #0
ClearTilemapB:
	stz $2119		;Zero all Tiles in Tilemap
	stz $2118
	dex
	bne ClearTilemapB
	dey
	bne ClearTilemapB
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
		clc
		adc TilemapBase	;Screen base
		sta $2117		;VMDATAL - We're writing bytes in PAIRS!
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
WaitFrame:
		lda $4212 			;HVBJOY - Status 	
			; xy00000a		- x=vblank state y=hblank state a=joypad ready
		and #%10000000
		bne WaitFrame		;Wait until we get zero - this means we're our of VBLANK
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
	
	
	