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

	include "\SrcAll\BasicMacros.asm"
	
	

	org $4000		;bank $0	
	setdp $2000			;Define the direct page as #$2000
	
ProgramStart:
	sei				;Disable interrupts
	csh				;Highspeed Mode
	cld				;Clear Decimal mode
	
	    ;      T12 - TIQ, IRQ1, IRQ2
	lda #%00000111
	sta $1402		;IRQ mask... 1=Off
	
	lda #$f8		;map in RAM
	tam #%00000010	;TAM1 (2000-3FFF)

	lda #$ff		;map in I/O (#$ff)
	tam #%00000001	;TAM0 (0000-1FFF)
	tax				
	txs				;Init stack pointer
		
	;Page in the banks of our cartridge
	lda #$00		;map in ROM
	tam #%00000100	;TAM1 (4000-5FFF)
	lda #$01		;map in ROM
	tam #%00001000	;TAM1 (6000-7FFF)
	lda #$02		;map in ROM
	tam #%00010000	;TAM1 (8000-9FFF)
	lda #$03		;map in ROM
	tam #%00100000	;TAM1 (A000-BFFF)
	lda #$04		;map in ROM
	tam #%01000000	;TAM1 (C000-DFFF)
	
	
	jmp Restart		;Jump to $4000
Restart:		
	;Page in last bank (We were running here before)
	lda #$05		;map in ROM
	tam #%10000000	;TAM1 (E000-FFFF)	
	

	
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
	
;Screen Shape
	st0 #10			;Horizontal  Sync  Register (HSR)
	st1 #$02
	st2 #$02
	
	st0 #11			;Horizontal Display Register (HDR)
	st1 #$1F
	st2 #$03
	
	st0 #12			;Vertical Sync Register  (VPR)
	st1 #$02
	st2 #$0F
	
	st0 #13			;Vertical Display Register (VDR)
	st1 #$EF
	st2 #$00
	
	st0 #14			;Vertical Display End Position Register (VCR)
	st1 #$03
	st2 #$00
	
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
	stz $0404			;GGRRRBBB	
	stz $0405			;-------G
	
	
	
;Color 1
	lda #1		
	sta $0402			;Palette address L
	stz $0403			;Palette address H
	lda #%00011011			
	sta $0404			;GGRRRBBB
	lda #%00000000
	sta $0405			;-------G

;Color 2
	lda #2		
	sta $0402			;Palette address L
	stz $0403			;Palette address H
	lda #%11000111			
	sta $0404			;GGRRRBBB
	lda #%00000001
	sta $0405			;-------G
		
;Color 3
	lda #3
	sta $0402			;Palette address L
	stz $0403			;Palette address H
	lda #%11111111			
	sta $0404			;GGRRRBBB
	lda #%00000001
	sta $0405			;-------G
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

	
	
	lda #<Bitmap		;Source Bitmap Data
	sta z_L
	lda #>Bitmap
	sta z_H
	
	lda #<(BitmapEnd-Bitmap);Source Bitmap Data Length
	sta z_C
	lda #>(BitmapEnd-Bitmap)
	sta z_B
	
	lda #<$1800			;Tile 384 (256+128 - 32 bytes per tile)
	sta z_E
	lda #>$1800
	sta z_D
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
		

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		
	
FillAreaWithTiles:	; z_b = SX... z_c = SY... X=Width...
					; Y= Height... A=start tile
	sta z_d
FillAreaWithTiles_Yagain:
	phx
		jsr GetVDPScreenPos	;Recalculate memory position
		st0 #2				;Set Write Register
		lda z_d
FillAreaWithTiles_Xagain:	;Save the TileNum to Vram
		sta $0102			;L Byte
		st2 #1				;H Byte - Tile 256+
		clc
		adc #1				;Increase Tile Number
		dex 
		bne FillAreaWithTiles_Xagain
		sta z_d
		inc z_c				;Inc Ypos
	plx
	dey						;Decrease Y count
	bne FillAreaWithTiles_Yagain
	rts
	

prepareVram:		;z_HL=VRAM address to select

	st0 #0				;Select Memory Write Reg
	lda z_e
	sta $0102 			;st1 - L address
	lda z_d
	sta $0103 			;st2 - H Address
	rts

GetVDPScreenPos:	; BC=XYpos	
		st0 #0			;Select Vram Write
		lda z_c
		;and #%00000111	;Multiply Ypos by 32 - low byte
		asl
		asl
		asl
		asl
		asl
		clc
		adc z_b			;Add Xpos
		sta $0102		;Send to Data-L
		
		lda z_c
		and #%11111000	;Multiply Ypos by 32 - low byte
		lsr
		lsr
		lsr
		sta $0103		;Send to Data-H
	rts
	
	
		
	;BC=Bytes
	;DE=Destination Ram
	;HL=Source Bytes
DefineTiles:							
	jsr prepareVram			;Select Ram address
	st0 #2					;Select Data reg
	ldx z_C					;B=High byte of count - X=Low byte
	ldy #0	
DefineTilesAgain:
		lda (z_HL),Y		;Load a byte
		sta $0102			;Store Low byte
		iny
		lda (z_HL),Y		;Load a byte
		sta $0103			;Store High Byte
		iny
		bne DefineTilesAgainYok
		inc z_h				;INC High byte Y=low byte
DefineTilesAgainYok:		
		txa					;Is Low Byte Zero?
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
	
Bitmap:
	incbin "\ResALL\Sprites\RawPCE.RAW"
BitmapEnd:
	

	org $5ffe
	dw $E000			;Reset Vector 

	
	org $ffff
	db $0			;End of cartridge
	