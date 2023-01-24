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

;Current player pos
PlayerX 	equ $60		;Position of next printed character
PlayerY 	equ PlayerX+1

;Last player pos (For clearing sprite)
PlayerX2 	equ PlayerX+2
PlayerY2 	equ PlayerX+3

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

	
	
	
	lda #3			;Start SX
	sta PlayerX
	lda #3			;Start SY
	sta PlayerY

	lda #0			;Fake No Keys on first run		
	sta z_h
	jmp StartDraw	;Force Draw of character first run
	
infloop:
	jsr Player_ReadControlsDual
	lda z_h
	beq infloop		;See if no keys are pressed

StartDraw:	
	ldx PlayerX		;Back up X
	stx PlayerX2
	
	ldy PlayerY		;Back up Y
	sty PlayerY2
	
	jsr BlankPlayer	;Remove old player sprite
	
	lda z_h
	and #%00000001	;RSBALDRU 
	bne JoyNotUp	;Jump if UP not presesd
	dey				;Move Y Up the screen
JoyNotUp:
	lda z_h
	and #%00000100	;RSBALDRU 
	bne JoyNotDown	;Jump if DOWN not presesd
	iny 			;Move Y Down the screen
JoyNotDown:
	lda z_h
	and #%00001000	;RSBALDRU 
	bne JoyNotLeft	;Move X Left 
	dex
JoyNotLeft:
	lda z_h
	and #%00000010	;RSBALDRU 
	bne JoyNotRight	;Move X Right
	inx
JoyNotRight:

	stx PlayerX		;Update X
	sty PlayerY		;Update Y
	
;X Boundary Check - if we go <0 we will end up back at 255
	cpx #32
	bcs PlayerReset
	
;Y Boundary Check - only need to check 1 byte
	cpy #28
	bcs PlayerReset
	
	jmp PlayerPosYOk ;Not Out of bounds
	
PlayerReset:
	ldx PlayerX2	;Reset Xpos	
	stx PlayerX
	
	ldy PlayerY2	;Reset Ypos
	sty PlayerY
	
PlayerPosYOk:
	jsr DrawPlayer	;Draw Player Sprite
	
	ldx #255
	ldy #200
	jsr PauseXY		;Wait a bit!	
		
	jmp infloop
	
BlankPlayer:
	lda #129		;Tile Num	
	jmp DrawSprite
DrawPlayer:
	lda #128		;Tile Num
DrawSprite:
	pha
		stx z_b
		sty z_c
		jsr GetVDPScreenPos	;Calculate Tilemap mempos
	pla
	st0 #2				;Set Write Register
						;Save the TileNum to Vram
	sta $0102			;L Byte
	st2 #1				;H Byte - Tile 256+
	rts

Bitmap:
;Smiley
	;   00000000  11111111  	- Bitplane 0/1
	DB %00111100,%00000000     ;  0
	DB %01111110,%00000000     ;  1
	DB %11111111,%00100100     ;  2
	DB %11111111,%00000000     ;  3
	DB %11111111,%00000000     ;  4
	DB %11011011,%00100100     ;  5
	DB %01100110,%00011000     ;  6
	DB %00111100,%00000000     ;  7
	;   22222222  33333333   	- Bitplane 2/3
	DB %00000000,%00000000     ;  0
	DB %00000000,%00000000     ;  1
	DB %00000000,%00000000     ;  2
	DB %00000000,%00000000     ;  3
	DB %00000000,%00000000     ;  4
	DB %00000000,%00000000     ;  5
	DB %00000000,%00000000     ;  6
	DB %00000000,%00000000     ;  7

;Empty Sprite
	ds 32,0
BitmapEnd:

PauseXY:
	dex
	bne PauseXY
	dey 
	bne PauseXY
	rts


	;R:      3210
	;W:		   CS			C=Clear S=Select key/dir
	
	;Reset the Multitap... following reads will read in 
		;from joysticks 1-5
		
Player_ReadControlsDual:
	ldx #%00000001			;Reset Multitap 1
	jsr JoypadSendCommand
	ldx #%00000011			;Reset Multitap 2
	jsr JoypadSendCommand

	ldx #%00000001				
	jsr JoypadSendCommand	;----LDRU (Left/Down/Right/Up)
	jsr JoypadShiftFourBits
	dex
	jsr JoypadSendCommand	;---RSBA (Run/Start/B/A)
	jsr JoypadShiftFourBits
	rts

JoypadShiftFourBits:		;Shift RSBA in to z_as
	ldy #4
JoypadShiftFourBitsB:
	ror
	ror z_h
	dey
	bne JoypadShiftFourBitsB
	rts
	
JoypadSendCommand:
	stx $1000			;Set option from X
	PHA 				;Delay
	PLA 
	NOP 
	NOP
	lda $1000			;Load result
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		
	

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
	
	
		
prepareVram:		;z_HL=VRAM address to select

	st0 #0				;Select Memory Write Reg
	lda z_e
	sta $0102 			;st1 - L address
	lda z_d
	sta $0103 			;st2 - H Address
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
	
	
	org $fffe
	dw ProgramStart			;Reset Vector 
	