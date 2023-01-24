;Enable ONE of these below
;LineInterrupt equ 1		;Use Mapper 4 Line interrupt
UseSpr0Hit equ 1		;Use Sprite0Hit 

ScrollBothXY equ 1		;Allow X+Y Midscreen scroll (harder)

z_Regs 		equ $40

z_HL equ z_Regs
z_L  equ z_Regs
z_H  equ z_Regs+1

z_BC equ z_Regs+2
z_C  equ z_Regs+2
z_B  equ z_Regs+3

z_DE equ z_Regs+4
z_E  equ z_Regs+4
z_D  equ z_Regs+5

z_ixl  equ z_Regs+6
z_iyl  equ z_Regs+8



	ifdef UseSpr0Hit
hitcheck equ $96			;Have we found the Sprite0 yet this frame?
hitcheckEnabled equ $97		;Are we currently using the splitscreen?
	endif
	
	ifdef ScrollBothXY
s1 equ $90
s2 equ $91
s3 equ $92
s4 equ $93
	endif
sX equ $94
sY equ $95



	org $BFF0

	db "NES",$1a		;ID
	db $01				;Rom pages (16k each)
	db $0				;CHR-ROM pages
	db %01000010		;mmmmFTBM		mmmm = mapper no bottom 4 bits , Four screen vram layout, Trainer at &7000, Battery ram at &6000, Mirror (0=horiz, 1=vert)
	db %00000000		;mmmm--PV 		mapper (top 4 bits...  Pc10 arcade, Vs unisystem )
	db 0				;Ram pages
	db 0,0,0,0,0,0,0
						;We selected Mapper 4 - it has 8k VRAM , 8K Sram and 128k rom
	
vblanked 	equ $7F		;Zero page address of Vblank count

ProgramStart:
	sei					;Interrupts off
	cld					;Clear Decimal flag
	
	ldx #$ff			;Set up stack
	txs
	
;Palette
	lda #$3F		;Select Palette ram &3F00
	sta $2006		;PPUADDR H
	lda #0
	sta $2006		;PPUADDR L
	

;8 Palettes (4 tile / 4 sprite) + 1 back	
	ldx #4*8		
PaletteAgain
	lda Palette-1,x 
	sta $2007		;PPUDATA
	dex 
	bne PaletteAgain

;Turn ON the screen
;(Sprite enable/back enable/Sprite leftstrip / backleftstrip)
	;lda #%00011110 	
	;sta $2001		;PPUMASK
	
	;lda #$80		;NMI enable (Vblank)
	;sta $2000		;PPUCTRL - VPHB SINN

	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	ifdef UseSpr0Hit
		lda #0
		sta hitcheckEnabled
	endif
	
	lda #<Bitmap			;Source Bitmap Data
	sta z_L
	lda #>Bitmap
	sta z_H

	lda #<(BitmapEnd-Bitmap);Source Bitmap Data Length
	sta z_C
	lda #>(BitmapEnd-Bitmap)
	sta z_B
	
	lda #<$0800				;Tile 128 (16 bytes per tile)
	sta z_E
	lda #>$0800
	sta z_D
	
	jsr DefineTiles			;Define the tile patterns
	
	
	
		
	; lda #<Bitmap			;Source Bitmap Data
	; sta z_L
	; lda #>Bitmap
	; sta z_H

	; lda #<(BitmapEnd-Bitmap);Source Bitmap Data Length
	; sta z_C
	; lda #>(BitmapEnd-Bitmap)
	; sta z_B
	
	; lda #<$1800				;Sprite Tile 128 (16 bytes per tile)
	; sta z_E
	; lda #>$1800
	; sta z_D
	
	; jsr DefineTiles			;Define the tile patterns
	
	
	
	lda #0					;Start SX
	sta z_b
	lda #0					;Start SY
	sta z_c
	
	ldx #32					;Width in tiles
	ldy #30					;Height in tiles
	
	lda #128				;TileStart
	jsr FillAreaWithBlock	;Draw the tiles to screen
	
	
	lda #0					;Start SX
	sta z_b
	lda #0					;Start SY
	sta z_c
	
	ldx #6					;Width in tiles
	ldy #6					;Height in tiles
	
	lda #128				;TileStart
	jsr FillAreaWithTiles	;Draw the tiles to screen
	
	lda #9					;Start SX
	sta z_b
	lda #9					;Start SY
	sta z_c
	
	ldx #6					;Width in tiles
	ldy #6					;Height in tiles
	
	lda #128				;TileStart
	jsr FillAreaWithTiles	;Draw the tiles to screen
	
	lda #31-6					;Start SX
	sta z_b
	lda #29-6					;Start SY
	sta z_c
	
	ldx #6					;Width in tiles
	ldy #6					;Height in tiles
	
	lda #128				;TileStart
	jsr FillAreaWithTiles	;Draw the tiles to screen
	
	lda #0
	sta sx
	sta sy

	ifdef ScrollBothXY
		sta s1
		sta s2
		sta s3
		sta s4
	endif

	jsr NesDisableScreen

	ifdef UseSpr0Hit
;For Sprite0Hit to be set the Sprite must be visible,
;and a pixel of the sprite must overlap a pixel of the background
	
		lda #128		
		sta z_ixl
		lda #128
		sta z_iyl
		
		lda #142
		sta z_e
		lda #1
		sta z_l
		
		lda #0;1
		;A=Hardware Sprite No. ixl,iyl = X,Y , E = Source Tile, l=Palette etc
		jsr SetHardwareSprite
	endif
	

	jsr NesEnableScreen
	
	lda #%11000000	;Sound IRQ off
	sta $4017

	
	cli         			;Enable IRQs.

	ifdef LineInterrupt
	;Sprite address must be different to tile address or this will fail.
		lda #$80+$20		;NMI enable (Vblank)
		sta $2000		;PPUCTRL - VPHB SINN
	endif
	
	ifdef UseSpr0Hit
		lda #1
		sta hitcheckEnabled	;Enable check
		lda #0
		sta hitcheck		;Don't look until next frame
	endif
	
InfLoop:
	ifdef UseSpr0Hit
			lda hitcheck	;Do we still need to check? 
			beq NoHit		;(%01000000=Yes)
			and $2002		;PPUStatus - %VSL-----
				
			beq NoHit		;No Sprite 0 collision!
			lda #0
			sta hitcheck	;Mark Already found
		
			jsr DoScroll	;Perform Scroll
NoHit:	
	endif
	jmp InfLoop				;Infinite loop

	
nmihandler:		;This procuedure runs after each frame (See footer.asm)
	pha
	tya
	pha
	txa
	pha
		inc vblanked		;Alter Vblank Zero page entry
		
;Reset Scroll
		;lda #0
		;sta $2006	
		;sta $2006	

		;lda $2002
			
		lda #0+4			;Scroll X
		sta $2005
		lda #0-8+4			;Scroll y
		sta $2005

		;lda #$80+$08	;Page in Tiles 0-255
		;Sprite address must be different to tile address or this will fail
		;sta $2000		;PPUCTRL - VPHB SINN		
		
;Enable Line Interrupt

		ifdef LineInterrupt
			lda #116	;Line IRQ will occur (Split)
			sta $C000   ;Set Line interrupt (Mapper 3 function)	
			
			sta $C001   ;Reset counter (Mapper 3 function)
			sta $E001   ;Enable IRQ (Mapper 3 function)
		endif

;Reset Sprite 0 Hit
		
		ifdef UseSpr0Hit
			lda hitcheckEnabled
			beq hitcheckDisabled	;Is our Sprite0 Enabled?
		WaitForSprite0Clear:		
			lda $2002			;PPUStatus
			and #%01000000			;Sprite0Hit flag doesn't clear 
			bne WaitForSprite0Clear		;until after NMI ends
			
			lda #%01000000		;Now Sprite0Hit has cleared, we can
			sta hitcheck			;tell main routine it can start checking
		hitcheckDisabled:	
		endif
		
	pla
	tax
	pla
	tay
	pla
	rti
	
	
irqhandler:					;Used by Mapper 4 
	pha
		ifdef LineInterrupt
			sta $E000  		;Disable line interrupt
			
			jsr DoScroll
		endif
	pla
	rti
	
	
	
DoScroll:
		ifdef ScrollBothXY
		;Advanced Scroll (Horizontal+Vertical)
			lda s1		
			sta $2006	;[2006h.1st S1]=(X/256)*4 + (Y/240)*8
			lda s2
			sta $2005	;[2005h.2nd S2]=((Y MOD 240) AND C7h)
		
			lda s3	
			sta $2005	;[2005h.1st S3]=(X AND 07h)
			lda s4
			sta $2006	;[2006h.2nd S4]=(X AND F8h)/8 + ((Y MOD 240) AND 38h)*4
		else 
		  
		;Simple Scroll (Horizontal)
			lda sx			;Scroll X
			sta $2005
			lda sy			;Scroll y
			sta $2005
		endif
		
		inc sx
		inc sy
		ifdef ScrollBothXY
			jsr SetScroll	;Calculate S1-S4 for next time
		endif

	rts
	
	
	ifdef ScrollBothXY
SetScroll:		;Multiple vertical tilemaps	
		lda sx
		and #%00000111
		sta S3			;(X AND 07h)
		
		lda sx
		;and #%11111000	;F8h
		lsr
		lsr
		lsr
		sta s4			;(AND F8h)/8
		
		lda sy
		and #%00111000	;38h
		asl
		asl
		ora s4
		sta S4			;(X AND F8h)/8 + ((Y MOD 240) AND 38h)*4
		
		lda sy
		and #%11000111	;C7h
		sta s2			;((Y MOD 240) AND C7h)
		
		lda #0
		sta s1			;(X/256)*4 + (Y/240)*8
	rts
	endif
	
;Based on the 'EveryNes' Documentation at: https://problemkaputt.de/everynes.htm	
	
	;Full-screen and Mid-frame Scrolling
;Simple full-screen scrolling can be implemented by initializing the Reload value via Ports 2005h and 2000h. Many games change the scroll settings mid-frame to split the screen into a scrolled and non-scrolled area: The Horizontal bits can be changed by re-writing the Reload value via Ports 2005h and 2000h, the vertical bits by re-writing the Pointer value via Port 2006h. Changing both horizontal and vertical bits is possible by mixed writes to Port 2005h and 2006h, for example:
  ;[2006h.1st S1]=(X/256)*4 + (Y/240)*8
  ;[2005h.2nd S2]=((Y MOD 240) AND C7h)
  ;[2005h.1st S3]=(X AND 07h)
  ;[2006h.2nd S4]=(X AND F8h)/8 + ((Y MOD 240) AND 38h)*4

  
  ;[2006h.1st S1]=(X/256)*4 + (Y/%11110000)*8
  ;[2005h.2nd S2]=((Y MOD %11110000) AND %11000111)
  ;[2005h.1st S3]=(X AND %00000111)
  ;[2006h.2nd S4]=(X AND %11111000)/8 + ((Y MOD %11110000) AND %00111000)*4
 
;Notes: In that example, most bits are updated twice, once via 2006h and once via 2005h, above shows only the relevant bits, the other bits would be don't care (eg. writing unmasked values to 2005h would be faster, and wouldn't change the functionality). The 1st/2nd-write-flipflop is toggled on each of the four writes, so that above does <first> change 2005h-2nd-write, and <then> 2005h-1st-write.

;Pointer Increment/Reload during Rendering
;During rendering, A4-A0 is incremented per tile, with carry-out to A10, at end of HBlank A4-A0 and A10 are reset to the Reload value. "A14-A12" are used as LSBs of Tile Data address, these bits are incremented per scanline, with carry-out to tile row A9-A5, the tile row wraps from 29 to 0 with carry-out to A11.
;Note: Initializing the tile row to 30 or 31 will display garbage tiles (fetched from Attribute table area), in that case the row wraps from 31 to 0, but without carry-out to A11.

	
	
	ifdef ScrollBothXY
SetScrollAlt:		;Alt with Multiple vertical tilemaps 
	txa
	pha
	tya
	pha
		ldx #0
		lda sy
		tay
		
		sec
		sbc #240		;Divide by 240
		bcc Lt240
		tay
		inx
Lt240:	
		lda sx
		and #%00000111
		sta S3
		
		lda sx
		;and #%11111000
		lsr
		lsr
		lsr
		sta s4	;Temp
		
		tya
		and #%00111000
		asl
		asl
		ora s4
		sta S4
		
		tya
		and #%11000111
		sta s2
		
		
		txa
		asl
		asl
		asl
		sta s1
	pla 
	tya
	pla 
	tax
	rts
	endif

		
		
Palette:  
; 	Color   3   2   1  0
		db $3C,$21,$12,$0D	;Sprites 3 - Blues
		db $30,$23,$04,$0D	;Sprites 2 - Purples
		db $30,$16,$28,$0D	;Sprites 1 - Reds
		db $30,$2B,$13,$0D	;Sprites 0
		
		db $30,$2B,$13,$0D	;Tiles 3
		db $30,$2B,$13,$0D	;Tiles 2
		db $30,$2B,$13,$0D	;Tiles 1
		db $30,$2B,$13,$0D	;Tiles 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
Bitmap:
	incbin "\ResALL\Sprites\RawNES.RAW"
BitmapEnd:

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
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	


;A=Hardware Sprite No. ixl,iyl = X,Y , E = Source Tile, l=Palette etc
SetHardwareSprite:	

	
	pha
		asl			;4 bytes per sprite - 64 sprites total
		asl
		sta $2003		;Select OAM address of sprite
		
		lda z_iyl		;Ypos
		sta $2004
		
		lda z_E			;Tilenum
		sta $2004
		
		lda z_l			;Attribs VHB---PP Vflip  Hflip  
		sta $2004			;Background priority  Palette
		
		lda z_ixl		;Xpos
		sta $2004
		
		jsr ResetScroll	;Writing to OAM messes up scroll
	pla
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	

GetVDPScreenPos:			;BC=XYpos	
		lda z_c
		;and #%00000111		;Ypos * 32 tiles per line
		asl
		asl
		asl
		asl
		asl
		ora z_b				;Add Xpos
		sta z_l				;Store in L byte
		lda z_c
		and #%11111000		;Other bits of Ypos for H byte
		lsr
		lsr
		lsr
		clc
		adc #$20			;$2000 ofset for base of tilemap
		jsr waitframe		;Wait for Vblank
		sta $2006			;PPUADDR
		lda z_L
		sta $2006			;PPUADDR
	rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
	;BC=Bytes
	;DE=Destination Ram
	;HL=Source Bytes		
DefineTiles:				;Send Data to tile definitions
	ldx z_C					;B=High byte of count - X=Low byte
	ldy #0
	jsr NesDisableScreen
	jsr prepareVram			;Calculate destination address
		
DefineTilesAgain	
	lda (z_HL),Y		
	sta $2007 				;PPUDATA - Write data to data-port
	iny
	bne DefineTilesAgainYok
	
	inc z_h					;INC High byte Y=low byte
DefineTilesAgainYok:		
	txa						;Is Low Byte Zero
	bne DefineTilesDecBC_C
	lda z_B					;Is High Byte Zero (Are We done?)
	beq DefineTilesAgainDone
	DEC z_B					;DEC Count High byte (X is low byte)
DefineTilesDecBC_C:	
	DEx						;Decrease Count Low Byte
	jmp DefineTilesAgain
DefineTilesAgainDone:
	jmp NesEnableScreen
	
FillAreaWithBlock:
	sta z_d					;Backup tilenum
FillAreaWithBlock_Yagain:
	txa
	pha
		jsr GetVDPScreenPos	;Calculate Tilemap mempos
	
		lda z_d
FillAreaWithBlock_Xagain:
		eor #%00000100
		sta $2007			;PPUDATA - Save Tile selection to Vram
		
		dex 
		bne FillAreaWithBlock_Xagain
		eor #%00000100
		sta z_d
		inc z_c				;INC Ypos
	pla
	tax
	dey
	bne FillAreaWithBlock_Yagain
						
	jmp resetscroll			;Need to reset scroll after writing to VRAM

	
		
FillAreaWithTiles:
	sta z_d					;Backup tilenum
FillAreaWithTiles_Yagain:
	txa
	pha
		jsr GetVDPScreenPos	;Calculate Tilemap mempos
	
		lda z_d
FillAreaWithTiles_Xagain:
		sta $2007			;PPUDATA - Save Tile selection to Vram
		clc
		adc #1				;Move to next tile
		dex 
		bne FillAreaWithTiles_Xagain
		sta z_d
		inc z_c				;INC Ypos
	pla
	tax
	dey
	bne FillAreaWithTiles_Yagain
						
	jmp resetscroll			;Need to reset scroll after writing to VRAM

	
prepareVram:				;Select a destination address

		lda z_d				;MSB - DEST ADDR
		sta $2006			;PPUADDR
		lda z_e				;LSB - Dest ADDR
		sta $2006			;PPUADDR
	rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	


NesDisableScreen:			;Turn OFF the screen
;(Sprite enable/back enable/Sprite leftstrip / backleftstrip)
	lda #%00000000	
	sta $2001				;PPUMASK
	;lda #$00				;NMI disable (Vblank)
	sta $2000				;PPUCTRL - VPHB SINN
	rts	

		
NesEnableScreen:			;Turn ON the screen
;(Sprite enable/back enable/Sprite leftstrip / backleftstrip)
	lda #%00011110 	
	sta $2001				;PPUMASK

	lda #$80+$20			;NMI enable (Vblank)
	sta $2000				;PPUCTRL - VPHB SINN
	
	rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	


ResetScroll:	
	lda #0					;Scroll X
	sta $2005				;PPUSCROLL
	lda #0-8				;Scroll y
	sta $2005				;PPUSCROLL
	rts	
	
;Cartridge Footer	
	org $FFFA
	dw nmihandler			;FFFA - Interrupt handler
	dw ProgramStart			;FFFC - Entry point
	dw irqhandler			;FFFE - IRQ Handler
	
	
	
	
	
	
	