
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


PlayerX 	equ $40
PlayerY 	equ $41

PlayerX2 	equ $42
PlayerY2 	equ $43

FrameNo 	equ $44		; 0-1
FrameBase 	equ $45		; 0/2/4 L/R/U

SPpage equ $0100

*=$0401
	db $0e,$04,$0a,$00,$9e,$20,$28, $31,$30,$34,$30,$29,$00,$00,$00
	
	
	SEI						;Stop interrupts
	jsr Cls					;Clear the screen
			
	lda #0
	sta FrameNo
	sta FrameBase
	
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
	cmp #%11111111
	beq infloop		;See if no keys are pressed
	pha
StartDraw:	
		ldx PlayerX		;Back up X
		stx PlayerX2
		
		ldy PlayerY		;Back up Y
		sty PlayerY2
		
		jsr BlankPlayer	;Remove old player sprite
		
		ldx PlayerX		;Back up X
		ldy PlayerY		;Back up Y
		
	pla
	sta z_h
	and #%00000001	;---FRLDU
	bne JoyNotUp	;Jump if UP not presesd
	cpy #0
	beq JoyNotUp	;At top of screen?
	dey
	lda #4
	sta FrameBase	;Up/Down Frames
JoyNotUp:
	lda z_h
	and #%00000010	;---FRLDU
	bne JoyNotDown	;Jump if DOWN not presesd
	cpy #25-4
	beq JoyNotDown ;At top of screen?
	iny
	lda #4
	sta FrameBase	;Up/Down Frames
JoyNotDown:
	lda z_h
	and #%00000100	;---FRLDU
	bne JoyNotLeft	;Move X Left 
	cpx #0
	beq JoyNotLeft	;At bottom of screen?
	dex
	lda #2
	sta FrameBase	;Left Frames
JoyNotLeft:
	lda z_h
	and #%00001000	;---FRLDU
	bne JoyNotRight	;Move X Right
	cpx #40-4
	beq JoyNotRight	;At left of screen?
	inx
	lda #0
	sta FrameBase	;Right Frames
JoyNotRight:
	
	stx PlayerX		;Update X
	sty PlayerY		;Update Y
	
	lda FrameNo
	eor #%00000001	;Loop 4 frames
	sta FrameNo
	
	jsr DrawPlayer	;Draw Player Sprite
	
	ldx #255
	ldy #60
PauseXY:
	dex
	bne PauseXY
	dey 
	bne PauseXY
	
	jmp infloop
	
	
	
	
	
	
	
BlankPlayer:		
	lda #<BlankBitmap
	sta z_l
	lda #>BlankBitmap
	sta z_h
	
	jmp DrawBoth
DrawPlayer:		
	
	lda FrameNo		;Frame of anim
	clc
	adc FrameBase	;Anim bank
	asl
	asl
	asl
	asl				;16 bytes per frame
	clc
	adc #<Bitmap	;Add address of bitmaps
	sta z_l
	
	lda #>Bitmap
	adc #0			;Add carry from low byte
	sta z_h
	
DrawBoth:
	ldx PlayerX
	ldy PlayerY
	jsr GetVDPScreenPos		;Get Vram Address in z_DE
	
	lda #4
	sta z_b		;Wid
	lda #4
	sta z_c		;Hei
	
ShowBitmap:	;Show Zero terminated 'bitmap' from z_HL to z_DE
				;Size (W,H) z_B,z_C 
					
	ldy z_b				;Width
ShowBitmapH:
	dey
	lda (z_hl),y		;Transfer one char
	sta (z_de),y
	cpy #0				;Transfer bytes until char 0
	bne ShowBitmapH		;next Char
	
	jsr GetNextLine		;Next Dest line
	
	clc
	lda z_b				;next line of source data.
	adc z_l
	sta z_l
	bcc NextSpriteLine
	inc z_h				;Inc next line of top byte
NextSpriteLine:
	dec z_c
	bne ShowBitmap		;Next line
	rts	
	
	
	
GetVDPScreenPos:	; Get Screen address BC=XYpos in z_DE
	;$8000 + Ypos *40 + Xpos		Ypos*40=32+8
	lda #0
	sta z_d
	
	tya
	asl
	asl
	asl
	sta z_e			;Ypos *8
	asl
	rol z_d
	asl
	rol z_d
	adc z_e
	sta z_e			;Ypos * 32 
	
	lda z_d
	adc #$80			;Screen base is $8000
	sta z_d
	
	clc
	txa
	adc z_e
	sta z_e
	bcc GetVDPScreenPos_Done
	inc z_d
	
GetVDPScreenPos_Done:
	rts
	
	
GetNextLine:			;Move z_DE down a line
	clc
	lda #40				;Move Dest down 1 line
	adc z_e
	sta z_e				;Update Low byte
	bcc GetNextLineDone
	inc z_d				;Update High byte with carry
GetNextLineDone:
	rts	
	

	
Bitmap:					;4x4 - 2 frames, 3 types
	incbin "\ResAll\Sprites\AnimPET.RAW"
BlankBitmap:
	db 32,32,32,32		;Empty 4x4
	db 32,32,32,32
	db 32,32,32,32
	db 32,32,32,32
	
	
Player_ReadControlsDual:	;Returns %--21RLDU
	lda #255
	sta z_h				;Cursor buildup
	sta z_l				;Unused 
	
	lda #6				;Line 6
	ldx #%00100000		;Fire 2 (Enter)
	jsr TestCursorBit
	
	lda #9				;Line 9
	ldx #%00000100		;Fire 1 (Space)
	jsr TestCursorBit
	
	lda #4				;Line 4
	ldx #%10000000		;Right (Numpad 6)
	jsr TestCursorBit
	
	;lda #4				;Line 4
	ldx #%01000000		;Left (Numpad 4)
	jsr TestCursorBit
	
	lda #7				;Line 7
	;ldx #%01000000		;Down (Numpad 2)
	jsr TestCursorBit
	
	lda #3				;Line 3
	;ldx #%01000000		;Up	(Numpad 8)
	;jsr TestCursorBit
	;rts
	
TestCursorBit:				;A=Line X=Mask
	pha
		sta $E810			;Select line
		txa
		and $E812			;test key
		clc					;Clear carry (Pressed)
		beq TestCursorBitB		
		sec					;Set carry (not pressed)
TestCursorBitB:	
		rol z_h				;shift the key into the buildup
	pla
	rts
	

Cls:
	lda #$80		;$8000 screen base
	sta z_h
	lda #0
	sta z_l
	ldx #4			;$400 bytes total
	ldy #0
	
	lda #32			;Space
ClsAgain:
	sta (z_hl),y
	iny
	bne ClsAgain
	inc z_h
	dex 
	bne ClsAgain
	rts
	