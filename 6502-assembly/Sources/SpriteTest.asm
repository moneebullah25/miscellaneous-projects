
Mode2Color equ 1	;For C64,Apple 2 & Atari

C64_ScrBase4000	equ 1 	;Define screen base at $4000

ScrWid256 equ 1

	ifdef BuildNES
UseNesBuffer equ 1
VDPBuffer equ UserRam
	endif
	
	ifdef BuildSNS
UseNesBuffer equ 1
SnesScreenBuffer equ UserRam+$400
	endif
	
	include "..\SrcALL\V1_Header.asm"
	include "\SrcAll\BasicMacros.asm"
	
;FourColor equ 1	


	SEI			;Stop interrupts

	

	jsr ScreenInit

	
	
	ifdef BuildVIC
		jsr Cls
	else
		jsr Cls
	endif
	jsr Monitor	
		

	lda #<MyText
	sta z_L
	lda #>MyText
	sta z_H
	jsr PrintString

	
	ldx #4
	ldy #8
	
	ifdef BuildAP2
bmpwidth equ 8
	else
	ifdef BuildLNX
bmpwidth equ 24
	else
	ifdef BuildC64
bmpwidth equ 3
	else
bmpwidth equ 6	
	endif

	endif
	endif
	
	
	
	
;BMPNORMAL equ 1
	ifdef BuildLNX
BMPNORMALQ equ 1
	endif
	ifdef BuildA52
BMPNORMALQ equ 1
	endif
	ifdef BuildA80
BMPNORMALQ equ 1
	endif
	ifdef BuildAP2
BMPNORMAL equ 1
	endif
	
	
	ifdef BuildBBC
BMPBBC equ 1		;also C64- do 8 lines in groups before next x tile
	endif
	ifdef BuildC64
BMPBBC equ 1		;also C64- do 8 lines in groups before next x tile
	endif
	
	
	ifdef BuildNES
BMPTILE equ 1
	endif
	ifdef BuildPCE
BMPTILE equ 1
	endif
	ifdef BuildSNS
BMPTILE equ 1
	endif
	ifdef BuildVIC
BMPTILE equ 1
	endif
	
	
	
	lda #<Bitmap					;Source Bitmap Data
	sta z_L
	lda #>Bitmap
	sta z_H

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
;											Tile Type
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
	ifdef BMPTILE
	
	lda #<(BitmapEnd-Bitmap)		;Source Bitmap Data Length
	sta z_C
	lda #>(BitmapEnd-Bitmap)
	sta z_B
	
	ifdef BuildPCE
		lda #<$1800					;Tile 384 (256+128 - 32 bytes per tile)
		sta z_E
		lda #>$1800
		sta z_D
	endif
	ifdef BuildNES
		lda #<$0800					;Tile 128 (16 bytes per tile)
		sta z_E
		lda #>$0800
		sta z_D
	endif
	ifdef BuildSNS
		lda #<$1800					;Snes patterns start at $1000
		sta z_E						; each adddress holds 1 word...  
		lda #>$1800					; so each 32 byte tile takes 16 addreses,
		sta z_D						; and tile 128 is at $1800
	endif
	ifdef BuildVIC
		lda #<$1C00					;Tile 0 in VIC Custom Characters
		sta z_E
		lda #>$1C00
		sta z_D
	endif
	jsr DefineTiles					;Define the tile patterns
	
	lda #3							;Start SX
	sta z_b
	lda #3							;Start SY
	sta z_c
	
	ldx #6							;Width in tiles
	ldy #6							;Height in tiles
	
	ifdef BuildVIC
		lda #0							;TileStart
	else
		lda #128						;TileStart
	endif
	jsr FillAreaWithTiles			;Draw the tiles to screen
	
	endif
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
;											BBC Type - 8 bytes down - then across
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	ifdef BMPBBC
	
	lda #0
NexBitmapNextStrip:
	pushall
		jsr GetScreenPos			;Get screen pos from XY into Z_DE
BitmapNextLine:
		pushall
			ldY #0					;Offset for bytes in this strip
BitmapNextByte:
			lda (z_hl),Y			;Load in a byte from source - offset with Y
			sta (z_de),Y			;Store it in screen ram - offset with Y
			
			
			;loadpair z_bc,$1000
			;jsr Pause
			
			inY						;INC the offset
			cpY #bmpwidth*8*2		;We draw 8 lines * bitmap width
			bne BitmapNextByte
			
			sty z_C					;ADD Y to Z_HL to move source down one strip 
			jsr addHL_0C			;Add Z_C to HL
		pullall
	pullall
	pha
		tya
			clc
			adc #8					;Move Y down 8 lines
		tay
	pla
	clc
	adc #1
	cmp #6					;NO of strips in Bitmap (Y) 8 rows per strip
	bne NexBitmapNextStrip
	
	endif	; BMPBBC
	

	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
;									Normal type - linear bmp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
		ifdef BMPNORMALQ			;Simple bitmap routine 
			
		lda #<Bitmap	;Bitmap source
		sta z_L
		lda #>Bitmap
		sta z_H
	
		ldx #6			;Xpos
		ldy #6			;Ypos
	
		jsr GetScreenPos
		ldx #0
BitmapNextLine:
		pushall
			ldY #0
			PushPair z_de			;Backup Mempos
BitmapNextByte:
				ldx #0
				lda (z_hl),Y		;Copy a byte from the source 
				sta (z_de),Y		;to the destination
					
				inY
				cpY #bmpwidth		;Repeat for next byte of line
				bne BitmapNextByte
				
				sty z_C				;ADD Y to Z_HL to move source down one strip 
				jsr addHL_0C		;Add Z_C to HL
				
			PullPair z_de			;Restore mempos
			jsr GetNextLine			;move mempos down a line
		pullall
		inx 
		cpx #8*6					;Check if we've done all the lines
		bne BitmapNextLine			;Repeat until we have

	endif 	;End of BMPNORMALQ	

	ifdef BMPNORMAL

		ldx #6			;Xpos
		ldy #8			;Ypos
		
		lda #0				
NexBitmapNextStrip:
		pushall
			jsr GetScreenPos
			ldx #0
BitmapNextLine:
			pushall
				ldY #0
				PushPair z_de			;Backup Mempos
BitmapNextByte:
					ldx #0
					lda (z_hl,X)
					
					sta (z_de),Y
					jsr IncHL
					
					inY
					cpY #bmpwidth				
					bne BitmapNextByte
				PullPair z_de			;Restore mempos
				jsr GetNextLine			;move mempos down a line
				
		;	loadpair z_bc,$0FFF	;Pause to allow redraw to be seen
		;
		jsr Pause
			pullall
			inx 
			cpx #8
			bne BitmapNextLine			;Some systems need a recalc every 8 lines
		pullall
		pha
			tya
			clc
			adc #8						;Move Y down 8 lines
			tay
		pla
		clc
		adc #1
		cmp #6							;See if we've got to the end of the bitmap
		bne NexBitmapNextStrip			;Every 8 lines we need to do a full recalc
	
	endif 	;End of BMPNORMAL	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
	;jmp *

	lda #>Palette		;Palette definitions
	sta z_d
	lda #<Palette
	sta z_e

	ldy #0
SetPaletteAgain:	
	lda (z_de),y		;Low byte of color
	sta z_l
	iny
	lda (z_de),y		;High byte of color
	sta z_h
	iny
	
	tya					;Halve Y
	clc
	ror
	sec					;Subtract 1
	sbc #1
	jsr SetPalette		;-GRB definition in Z_HL... 
							;A=palette entry (0=background)
	ifdef BuildNES
		cpy #32*2		;16 for back, 16 for sprite
	else
		cpy #4*2		;4 palette entries, 2 bytes each
	endif
	bne SetPaletteAgain

	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	

;			Hardware Sprites!
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	


	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
		;Start of Atari code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
		
		
	ifdef BuildA5280
		
		;     ---R45bb		;R=vertical Resolution 4=4players 5=Missiles
		lda #%00111110
		sta $D400			;DMA control (SDMCTL)
			
		lda #$18			;Sprites will be at $1800+$400 (or +$200 in low res mode)
		sta $D407			;Store player sprite base
		
		;	 ------L45		Latch Trigger / Enable 4 player / enable 5 (missiles)
		lda #%00000011
		sta GTIA+$1D		;Graphics Control (GRACTL)
		
			; ggCMpppp		C=multiColor M=Missile (player 5 color) pppp=priority setting (1=sprites in front 4=behind)
		lda #%00010001		;M=1 sprite 4 to use color 3 
		sta GTIA+$1B		;and put sprites in front of background
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;Player 0 Sprite

		;	  ------WW
		lda #%00000011		;Width of sprite (0-3)
		sta GTIA+$08		;Player Sprite 0
		
		lda #$40
		sta GTIA+$00		;Xpos Player 0 
		
		lda #$1F			
		sta GTIA+$12		;Color Player 0

		lda #<($1C00+$80)	;Define Player 0 Sprite
		sta z_E
		lda #>($1C00+$80)	;Player 0 Ypos=$80
		sta z_D
		
		lda #<Sprite		;Sprite Source
		sta z_L
		lda #>Sprite
		sta z_H
		ldy #(SpriteEnd-Sprite)+1	;Length
NextByteP1:
		dey
		lda (z_hl),y		;Copy Data
		sta (z_de),y
		tya
		bne NextByteP1
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
		;Missile / Player 5 Sprite
	
		;	  wwWWwwWW
		lda #%01010101		;Width of sprite (Need to set all 4 parts)
		sta GTIA+$000C
		
		lda #120
		sta GTIA+4			;player Missile Part 1 Xpos
		sec
		sbc #4
		sta GTIA+5			;player Missile Part 2 Xpos
		sbc #4
		sta GTIA+6			;player Missile Part 3 Xpos
		sbc #4
		sta GTIA+7			;player Missile Part 4 Xpos
		
		
		lda #88				;Missile Color
		sta GTIA+$19	
		;sta  GTIA+$12		;Missile Parts colors 
		;sta  GTIA+$13			(when split - bit4 of GTIA+$1B)
		;sta  GTIA+$14
		;sta  GTIA+$15
		
		lda #<($1B00+$40)	;Define Missile sprite 
		sta z_E
		lda #>($1B00+$40)	;Player 0 Ypos=$40
		sta z_D
		
		lda #<Sprite		;Sprite Source
		sta z_L
		lda #>Sprite
		sta z_H

		ldy #(SpriteEnd-Sprite)+1	;Length
NextByteMissile:
		dey
		lda (z_hl),y		;Copy Data
		sta (z_de),y
		tya
		bne NextByteMissile		
	endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
		;End of Atari code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
	
	

	
	
	ifdef BMPTILE
	
	lda #<Sprite					;Source Bitmap data
	sta z_L
	lda #>Sprite
	sta z_H

	lda #<(SpriteEnd-Sprite)		;Length of bitmap data
	sta z_C
	lda #>(SpriteEnd-Sprite)
	sta z_B
	
	
	

	ifdef BuildSNS
		lda #<($4000+16)			;Skip tile 0 
		sta z_E							;(for unused sprites to be hidden)
		lda #>($4000+16)		
		sta z_D
	endif
	
	ifdef BuildPCE
		lda #<$2000					;Destination in VRAM
		sta z_E
		lda #>$2000
		sta z_D
	endif
	
	ifdef BuildNES
		lda #<$0C00					;Pattern 192 in Vram
		sta z_E
		lda #>$0C00
		sta z_D
	endif
	
	jsr DefineTiles					;Send data to Vram
	
	endif
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	ifdef BuildSNS	
	
		
		lda #<32	;Xpos = 32
		sta z_ixl	;(Add 512 for doublesize sprite)
		lda #>32  	;(32+512)
		sta z_ixh
SnsAgain	
		lda #<100	;Ypos = 100
		sta z_iyl
		lda #>100
		sta z_iyh
		
		lda #1
		sta z_h		;Tile Pattern Num
		
			 ;YXPPCCCT - Y=yflip X=xflip P=priority compared to BG...
		lda #%00110000		; C=palette +128 T= Tile Pattern number
		sta z_l			
		
		lda #0		;Spritenum 0
		
		jsr SetHardwareSprite	;Draw One!	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		
		lda #8		;Xpos + 8
		clc
		adc z_ixl
		sta z_ixl
	
		inc z_h		;tile +1
		lda #1		;Spritenum
		
		jsr SetHardwareSprite	;Draw One!	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		
		lda #-8		;Xpos - 8
		clc
		adc z_ixl
		sta z_ixl
		
		lda #8		;Ypos + 8
		clc
		adc z_iyl
		sta z_iyl
		
		inc z_h		;tile +1
		lda #2		;Spritenum
		
		jsr SetHardwareSprite	;Draw One!	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		
		lda #8		;Xpos + 8
		clc
		adc z_ixl
		sta z_ixl
		
		inc z_h		;tile +1
		lda #3		;Spritenum
		
		jsr SetHardwareSprite	;Draw One!	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


		
		;Loop test
		lda #-8
		clc
		adc z_ixl
		sta z_ixl
		
		inc z_ixl
		
		jmp SnsAgain
		 
	                         
	endif
	ifdef BuildC64
		lda #<Sprite					;Source Bitmap data
		sta z_L
		lda #>Sprite
		sta z_H

		lda #<(SpriteEnd-Sprite)		;Length of bitmap data
		sta z_C
		lda #>(SpriteEnd-Sprite)
		sta z_B
		
		lda #<(ScrBase+$1000)		;Will only work when scrBase @ $4000 
		sta z_E							;Due to CHAR ROM
		lda #>(ScrBase+$1000)		
		sta z_D
		
		jsr LDIR					;Copy sprite to correct address in VRAM
	
		lda #<200			;Xpos
		sta z_ixl
		lda #>200
		sta z_ixh
		
		lda #<150			;Ypos
		sta z_iyl
		lda #>150
		sta z_iyh
		
		lda #($1000/64)		;Sprite Ram base 
		sta z_h					;relative to Screen Base
		
			; -XY4CCCC	4=4color mode C= sprite color X=doubleX Y=doubleY
		lda #%01111111	
		sta z_l
		
		lda #0				;Hardware Sprite Number
		jsr SetHardwareSprite
		
		
		
		lda #<100			;Xpos
		sta z_ixl
		lda #>100
		sta z_ixh
		
		lda #($1040/64)		;Sprite Ram base 
		sta z_h					;relative to Screen Base
		
			; -XY4CCCC	4=4color mode C= sprite color X=doubleX Y=doubleY
		lda #%00000011	
		sta z_l
		
		lda #1				;Hardware Sprite Number
		jsr SetHardwareSprite
		
	endif
	
	
	ifdef BuildNES
	
		;Common settings for our 16x16 Crosshair (4 sprites)
	
		lda #$40
		tax
		sta z_ixl		;Xpos $40
		
		lda #$40
		tay
		sta z_iyl		;Ypos $40
		
		lda #0			
		sta z_l			;Palette 0
		
		;Define the 4 parts of the sprite
		
		lda #192						
		sta z_e							;Tile 192
		
		lda #0							;Sprite 0
		
		jsr SetHardwareSprite			;Top Left
		
		txa
		clc
		adc #8							;Move X+8
		sta z_ixl
		
		inc z_e							;Tile 192
		lda #1							;Sprite 1
		
		jsr SetHardwareSprite			;Top Right
		
		txa
		sta z_ixl						;Reset X

		tya
		clc
		adc #8							;Move Y+8	
		sta z_iyl
		
		inc z_e							;Tile 193
		lda #2							;Sprite 2
		
		jsr SetHardwareSprite			;Bottom Left
		
		txa
		clc
		adc #8							;Move X+8
		sta z_ixl
		
		inc z_e							;Tile 194
		lda #3							;Sprite 3
		
		jsr SetHardwareSprite			;Bottom Right
		
	endif
	ifdef BuildPCE
		lda #0			;X - Pos
		sta z_ixh	
		lda #96
		sta z_ixl	
		
		lda #0
		sta z_iyh		;Y - Ypos
		lda #128
		sta z_iyl	
		
		;;Palette and options
		
			; Y-yyX--x	;Y= Yflip y=ysize / X=xflip x=xsize 
		lda #%00000000	;size allows tilling for up to 32x64
		sta z_h
			; F---PPPP - F=Foreground / P=Palette
		lda #%10000000
		sta z_l
		
		lda #>$2000>>5	;Sprite address (Top 11 bits - High Byte)
		sta z_d
		lda #<$2000>>5	;Sprite address (Top 11 bits - Low Byte)
		sta z_e
		lda #0
		jsr SetHardwareSprite
		
			
		;2nd test sprite
		lda #0
		sta z_iyh		;X - Pos
		lda #192
		sta z_iyl		;Y - Ypos
		lda #1
		jsr SetHardwareSprite
		
	endif
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	ifdef BuildLNX
		lda #>Sprite	;Sprite Ram Address
		sta z_h
		lda #<Sprite
		sta z_l
		
		lda #90
		sta z_ixl		;Xpos
		lda #30
		sta z_iyl		;Ypos
		
		jsr SetHardwareSprite
		
		
		lda #>Sprite	;Sprite Ram Address
		sta z_h
		lda #<Sprite
		sta z_l
		
		lda #60
		sta z_ixl		;Xpos
		lda #60
		sta z_iyl		;Ypos
		
		jsr SetHardwareSprite
		
	endif
	jmp *
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
Pause:
	jsr decbc
	lda z_b
	ora z_c
	bne Pause		;Pause for BC ticks
	rts
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	

Sprite:
	
	ifdef BuildLNX	
		;incbin "\ResAll\Sprites\SpriteLnxL.raw"
		incbin "\ResAll\Sprites\SpriteLnxR.raw"
		;incbin "\ResAll\Sprites\SpriteLnx2R.raw"
	endif
	ifdef BuildNES
		incbin "\ResALL\Sprites\SpriteNES.RAW"
	endif
	ifdef BuildPCE
		incbin "\ResALL\Sprites\SpritePCE.raw"
	endif
	ifdef BuildC64
		
		incbin "\ResAll\Sprites\SpriteC64-4col.raw"	
		incbin "\ResAll\Sprites\SpriteC64.raw"
	endif
	ifdef BuildSNS
		incbin "\ResALL\Sprites\SpriteSNS.RAW"
	endif
	
	ifdef BuildA5280
		incbin "\ResALL\Sprites\SpriteA52.raw"
	endif
	
SpriteEnd:
	
	
	include "\SrcAll\monitor.asm"
	include "\SrcAll\BasicFunctions.asm"
	
MyText
    db "Hello worlds!10000001235678!!!",  255
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Bitmap:
	Ifdef BuildAP2
		ifdef Mode2Color
			incbin "\ResAll\Sprites\RawAP2.RAW"
		else
			incbin "\ResAll\Sprites\RawAP2_4col.RAW"
		endif
	endif
	Ifdef BuildA52	
		ifdef Mode2Color
			incbin "\ResALL\Sprites\RawZX.RAW"
		else
			incbin "\ResALL\Sprites\RawA52.RAW"
		endif
	endif
	Ifdef BuildA80
		ifdef Mode2Color
			incbin "\ResALL\Sprites\RawZX.RAW"
		else
			incbin "\ResALL\Sprites\RawA52.RAW"
		endif
	endif
	ifdef BuildLNX
		incbin "\ResALL\Sprites\RawMSX.RAW"
	endif
	ifdef BuildBBC
		incbin "\ResALL\Sprites\RawBBC.RAW"
	endif
	ifdef BuildC64
		incbin "\ResALL\Sprites\RawC64-4col.RAW"
		;incbin "\ResALL\Sprites\RawC64-2col.RAW"
	endif
	ifdef BuildNES
		incbin "\ResALL\Sprites\RawNES.RAW"
	endif
	ifdef BuildPCE
		incbin "\ResALL\Sprites\RawPCE.RAW"
	endif
	ifdef BuildSNS
		incbin "\ResALL\Sprites\RawSNS.RAW"
	endif
	ifdef BuildVIC
		incbin "\ResAll\Sprites\RawVIC.raw"
	endif
BitmapEnd:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
BitmapFont:
	incbin "\ResALL\Font96.FNT"		;Not used by the VIC due to memory limitations

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	

Palette:
	;   -grb
	dw $0000	;0 - Background;
	dw $0099	;1
	dw $0E0F	;2
	dw $0FFF	;3 - Last color in 4 color modes
	dw $000F	;4;
	dw $004F	;5
	dw $008F	;6
	dw $00AF	;7
	dw $00FF	;8
	dw $04FF	;9
	dw $08FF	;10
	dw $0AFF	;11
	dw $0CCC	;12
	dw $0AAA	;13
	dw $0888	;14
	dw $0444	;15
	
	
	ifdef BuildNES	;Nes sprite colors
		dw $0000	;0 - Background;
		dw $0099	;1
		dw $0E0F	;2
		dw $0FF0	;3 - Last color in 4 color modes
		dw $000F	;4;
		dw $004F	;5
		dw $008F	;6
		dw $00AF	;7
		dw $00FF	;8
		dw $04FF	;9
		dw $08FF	;10
		dw $0AFF	;11
		dw $0CCC	;12
		dw $0AAA	;13
		dw $0888	;14
		dw $0444	;15
		dw $0FFF	;Border
	endif

	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		

	
	include "\SrcAll\V1_BitmapMemory.asm"
	include "\SrcALL\V1_Palette.asm"
	
	ifndef UseNesBuffer
		include "\SrcALL\V1_Functions.asm"		
		include "\SrcALL\V1_VdpMemory.asm"
	else
		ifdef BuildNES
			include "\SrcNes\Nes_V2_VdpMemory.asm"
			include "\SrcNes\V2_Functions.asm"
		endif
		ifdef BuildSNS
			include "\SrcSNS\SNS_V2_VdpMemory.asm"
			include "\SrcSNS\V2_Functions.asm"
		endif
	endif

	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		

	
	
	
		
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
		; ifdef BuildVIC
		; ifndef BuildVIC_Rom
			; org $1C00
			; db 0,0,0,0,0,0,0,0	;Set Char 0 to blank
			; incbin "\ResAll\Sprites\RawVIC.raw"
		; endif
		; endif
		
		include "..\SrcALL\V1_Footer.asm"
		
 