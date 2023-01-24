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
	ldx #4
	ldy #0
	lda #>Palette
	sta z_h
	lda #<Palette
	sta z_l
	stz $2121		;CGADD - Colour selection  
PaletteAgain:
		 ;gggrrrrr 
	lda (z_hl),y
	sta $2122		;CGDATA - Colour data register
		 ;?bbbbbgg 
	iny
	lda (z_hl),y
	sta $2122		;CGDATA
	iny
	dex
	bne PaletteAgain
		
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
	lda #>$2800			;so each 32 byte tile takes 16 addreses,
	sta z_D				;and tile 128 is at $1800
	jsr DefineTiles		;Define the tile patterns
	
;4 color Chibiko Bitmap ($2400=4 color Tile 128)
	lda #<Bitmap4Col		;Source Bitmap Data
	sta z_L
	lda #>Bitmap4Col
	sta z_H

	lda #<(Bitmap4ColEnd-Bitmap4Col);Source Bitmap Data Length
	sta z_C
	lda #>(Bitmap4ColEnd-Bitmap4Col)
	sta z_B

	lda #<$2400			;Snes patterns start at $1000
	sta z_E				;each adddress holds 1 word...  
	lda #>$2400			;so each 32 byte tile takes 16 addreses,
	sta z_D				;and tile 128 is at $1800
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
	
;Set up color maths
	
		; ---S4321 - S=sprites 4-1=enable Bgx
	lda #%00000011	;Turn on BG1+2
	sta $212C 		;Main screen designation [TM]    
	
		; ---S4321 - S=sprites 4-1=enable Bgx
	lda #%00000100	;Turn on BG3
	sta $212D 		;Sub Screen Designation (W)
	
		; BBEE--SD	;BB=Back EE=Window? S=Subscreen? D=Directcolor (256col)
	lda #%00000010
	sta $2130 ;2130h - CGWSEL - Color Math Control Register A (W)
	
		 ;BGRIIIII -BGR / Intensity
	lda #%00011111 
	sta $2132 ;2132h - COLDATA - Color Math Sub Screen Backdrop Color (W)
	
		 ;ADBS4321
	lda #%00100011	;A=Add/Sub D=Divide result by 2 BS4321 Apply to layers
					;B=Back S=Sprite 4321=Layers
	sta $2131 ;2131h - CGADSUB - Color Math Control Register B (W)

	
; 2130h - CGWSEL - Color Math Control  Register A (W)
  ; 7-6  Force Main Screen Black (3=Always, 2=MathWindow, 1=NotMathWin, 0=Never)
  ; 5-4  Color Math Enable       (0=Always, 1=MathWindow, 2=NotMathWin, 3=Never)
  ; 3-2  Not used
  ; 1    Sub Screen BG/OBJ Enable    (0=No/Backdrop only, 1=Yes/Backdrop+BG+OBJ)
  ; 0    Direct Color (for 256-color BGs)  (0=Use Palette, 1=Direct Color)

 

; 2131h - CGADSUB - Color Math Control Register B (W)
  ; 7    Color Math Add/Subtract        (0=Add; Main+Sub, 1=Subtract; Main-Sub)
  ; 6    Color Math "Div2" Half Result  (0=No divide, 1=Divide result by 2)
  ; 5    Color Math when Main Screen = Backdrop        (0=Off, 1=On) ;\
  ; 4    Color Math when Main Screen = OBJ/Palette4..7 (0=Off, 1=On) ; OFF: Show
  ; -    Color Math when Main Screen = OBJ/Palette0..3 (Always=Off)  ; Raw Main,
  ; 3    Color Math when Main Screen = BG4             (0=Off, 1=On) ;   or
  ; 2    Color Math when Main Screen = BG3             (0=Off, 1=On) ; ON: Show
  ; 1    Color Math when Main Screen = BG2             (0=Off, 1=On) ; Main+/-Sub
  ; 0    Color Math when Main Screen = BG1             (0=Off, 1=On) ;/

 ; 2132h - COLDATA - Color Math Sub Screen Backdrop Color (W)
; This 8bit port allows to manipulate some (or all) bits of a 15bit RGB value. Examples: Black: write E0h (R,G,B=0), Cyan: write 20h (R=0) and DFh (G,B=1Fh).
  ; 7    Apply Blue  (0=No change, 1=Apply Intensity as Blue)
  ; 6    Apply Green (0=No change, 1=Apply Intensity as Green)
  ; 5    Apply Red   (0=No change, 1=Apply Intensity as Red)
  ; 4-0  Intensity   (0..31)

 

	
	;	  x000bbbb - x=screen disable (1=disable) bbbb=brightness (15=max)
	lda #%00001111	;Screen on
	sta $2100		;INIDISP - Screen display register
	

	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
	
	lda #$00
	sta TilemapBase
	
	lda #3				;Start SX
	sta z_b
	lda #3				;Start SY
	sta z_c
	
	ldx #6				;Width in tiles
	ldy #6				;Height in tiles
		
	lda #128			;TileStart

	jsr FillAreaWithTiles ;Draw the tiles to screen
	
	
	lda #$04
	sta TilemapBase
	
	lda #13				;Start SX
	sta z_b
	lda #13				;Start SY
	sta z_c
	
	ldx #6				;Width in tiles
	ldy #6				;Height in tiles
		
	lda #128			;TileStart

	jsr FillAreaWithTiles ;Draw the tiles to screen
	
	
	lda #$08
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
	stx $210D				;BG1-Xpos
	stz $210D
	;sty $210E				;BG1-Ypos
	;stz $210E
	
	;stx $210F				;BG2-Xpos
	;stz $210F
	sty $2110				;BG2-Ypos
	stz $2110

	;stx $2111				;BG3-Xpos
	;stz $2111
	;sty $2112				;BG3-Ypos
	;stz $2112
	
	
	jsr WaitFrame
	
	jmp InfLoop				;Infinite Loop
		
Palette:
	  ;%-BBBBBGGGGGRRRRR
    dw %0000000000000000; ;0  %-BBBBBGGGGGRRRRR
	dw %0011110000001111; ;1  %-BBBBBGGGGGRRRRR
	dw %0011110111100000; ;2  %-BBBBBGGGGGRRRRR
	dw %0011110111101111; ;3  %-BBBBBGGGGGRRRRR
	
    dw %0000000000010000; ;1  %-BBBBBGGGGGRRRRR
    dw %0000001000000000; ;2  %-BBBBBGGGGGRRRRR
    dw %0000001000010000; ;3  %-BBBBBGGGGGRRRRR
    dw %0100000000000000; ;4  %-BBBBBGGGGGRRRRR
    dw %0100000000010000; ;5  %-BBBBBGGGGGRRRRR
    dw %0100001000000000; ;6  %-BBBBBGGGGGRRRRR
    dw %0110001100011000; ;7  %-BBBBBGGGGGRRRRR
    dw %0100001000010000; ;8  %-BBBBBGGGGGRRRRR
    dw %0000000000011111; ;9  %-BBBBBGGGGGRRRRR
    dw %0000001111100000; ;10  %-BBBBBGGGGGRRRRR
    dw %0000001111111111; ;11  %-BBBBBGGGGGRRRRR
    dw %0111110000000000; ;12  %-BBBBBGGGGGRRRRR
    dw %0111110000011111; ;13  %-BBBBBGGGGGRRRRR
    dw %0111111111100000; ;14  %-BBBBBGGGGGRRRRR
    dw %0111111111111111; ;15  %-BBBBBGGGGGRRRRR



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
	
	
	