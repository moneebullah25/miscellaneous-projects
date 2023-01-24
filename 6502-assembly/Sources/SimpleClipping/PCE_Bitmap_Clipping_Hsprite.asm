
VscreenMinX equ 64-8		;Top left of visible screen in logical co-ordinates
VscreenMinY equ 72-8

;VscreenWid equ 24		;Visible Screen Size in logical units
;VscreenHei equ 24

;LIMITATION.. The Virtual screen cannot be smaller than the sprite or 
;the crop will malfunction! (It can be the same size)

VscreenWid equ 128+8		;Visible Screen Size in logical units
VscreenHei equ 112+8

VscreenWidClip equ 0
VscreenHeiClip equ 0

	 
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


z_ixl equ z_Regs+8
z_ixh equ z_Regs+9
z_ix equ z_Regs+8

z_iyl equ z_Regs+10
z_iyh equ z_Regs+11
z_iy  equ z_Regs+10


;Current player pos
PlayerX 	equ $60		;Position of next printed character
PlayerY 	equ PlayerX+1

HspriteNum equ PlayerX+3
spritehclip equ PlayerX+4

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
	st1 #%11000000		;Background ON, Sprites On
	st2 #0
	
	st0 #9			
		; 0BBB0000
	st1 #%00000000		;BACKGROUND Tilemap size (32x32)
	st2 #0
	
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


;Sprite Color
;Color 0
	lda #0		
	sta $0402			;Palette address L
	lda #1		
	sta $0403			;Palette address H
	lda #%00011011			
	sta $0404			;GGRRRBBB
	lda #%00000000
	sta $0405			;-------G
	
;Color 1
	lda #1		
	sta $0402			;Palette address L
	lda #1		
	sta $0403			;Palette address H
	lda #%00011011			
	sta $0404			;GGRRRBBB
	lda #%00000000
	sta $0405			;-------G
;Color 2
	lda #2		
	sta $0402			;Palette address L
	lda #1		
	sta $0403			;Palette address H
	lda #%11000111			
	sta $0404			;GGRRRBBB
	lda #%00000001
	sta $0405			;-------G
;Color 3
	lda #3
	sta $0402			;Palette address L
	lda #1		
	sta $0403			;Palette address H
	lda #%11111111			
	sta $0404			;GGRRRBBB
	lda #%00000001
	sta $0405			;-------G
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	

	jsr cls
	
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

	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
	
	ldx #VscreenMinX			;Start SX
	stx PlayerX
	ldy #VscreenMinY			;Start SY
	sty PlayerY
	

	lda #0			;Fake No Keys on first run		
	sta z_h
	jmp StartDraw	;Force Draw of character first run
	
infloop:
	jsr Player_ReadControlsDual
	lda z_h
	beq infloop		;See if no keys are pressed

StartDraw:	
	ldx PlayerX		;Back up X
	ldy PlayerY		;Back up Y
	
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
	
PlayerPosYOk:
	stz HspriteNum		;Zero Hardware Sprite Number

	jsr DrawPlayer		;Draw Player Sprite
	
	jsr ClearUnsusedSprites	;Zero unneeded hardware sprite
	
	jsr UpdateSprites	;Send Sprite data to STAB
	
	ldx #255
	ldy #200
	jsr PauseXY		;Wait a bit!
		
	jmp infloop
	
DrawPlayer:
	lda #$C0			;First Sprite Tile num
	sta z_iyl
	stz z_iyh
		
	stx z_b				;Xpos
	sty z_c				;Ypos
	
	lda #24				;Width in units (2px)
	sta z_h
	lda #24				;Height in units (2px)
	sta z_l
FillAreaWithSprites:	; z_b = SX... z_c = SY... X=Width...
							; Y= Height... A=start tile
	
	jsr DoCrop			;Crop the sprite BC=XY pos HL=WidthHeight
	bcc DoDraw				;IY=source data
		rts
DoDraw:				
		
FillAreaWithTiles_Yagain:
	ldx #0				;Sprite Horizontal count
FillAreaWithTiles_Xagain:	
		lda HspriteNum	;Hsprite Number
		
		st0 #0	 		;Select Write Address ($00)		
		sta $0102 		;Low Byte of address  (Sprite x 4)
		st2 #$7F  		;High Byte of address ($7Fxx)
		
		clc
		adc #4
		sta HspriteNum	;Move to next H-Sprite
		
		st0 #2	 		;Data Write ($02)
		
			
		stz z_ixh		;Ypos H
		
		lda z_c			;Ypos (64 is visible top left corner)
		clc
		adc #24			;(64-16)/2 - sprite height 16
		asl
		sta z_ixl		;Ypos L
		bcc FillAreaWithTiles_YHok
			inc z_ixh
FillAreaWithTiles_YHok:					
		lda z_ixl		;Sprite Y-pos
		sta $0102		;Set Ypos L
		lda z_ixh	
		sta $0103 		;Set Ypos H
		
		stz z_ixh
		txa				;X-Sprite  (32 is visible top left corner)
		asl				;Each sprite is 16 pixels
		rol z_ixh		;Xpos * 2
		asl
		rol z_ixh		;Xpos * 4
		asl
		rol z_ixh		;Xpos * 8
		clc
		adc z_b			;Add X-Pos
		bcc FillAreaWithTiles_XHok
			inc z_ixh
FillAreaWithTiles_XHok:
		asl				;Xpos * 16 
		rol z_ixh
		clc
		adc #16			;32-16 - sprite width 16
		sta z_ixl
		bcc FillAreaWithTiles_XHok2
			inc z_ixh
FillAreaWithTiles_XHok2:
	
		lda z_ixl		;Sprite X-pos
		sta $0102 		;Set Xpos L
		lda z_ixh
		sta $0103 		;Set Xpos H
		
		lda z_iyl		;Sprite VRAM Address >>5
		sta $0102		;Set Tilenum L
		stz $0103 		;Set Tilenum H
		
						;Sprite Attributes
		stz $0102 		;Set Tile Attrib L
		stz $0103 		;Set Tile Attrib H
		
		inc z_iyl		;Increase Tile Number
		inc z_iyl		;Increase Tile Number
		
		inx 
		cpx z_h			;Done all Horizontal hardware sprites?
		bne FillAreaWithTiles_Xagain
		
	lda z_c				;Inc Ypos (8 logucal units=16 pixels)
	clc
	adc #8
	sta z_c

	
	lda spritehclip		;Skip any cropped patterns
	beq NoSpriteClip
	clc
	adc z_iyl
	sta z_iyl
	bcc NoSpriteClip
	inc z_iyh
NoSpriteClip:
		
	dec z_l	;Decrease Y count
	beq FillAreaWithTilesDone
		jmp	FillAreaWithTiles_Yagain
FillAreaWithTilesDone	
	rts
	

ClearUnsusedSprites:
	lda HspriteNum
	st0 #0	  ;sta $0100	;Select Write Address ($00)	
	sta $0102 ;st1 			;Low Byte of address  (Sprite x 4)
	st2 #$7F  ;sta $0103	;High Byte of address ($7Fxx)
	
	st0 #2	  ;sta $0100	;Data Write ($02)
	
	stz $0102 ;st1 	;Ypos (64 is visible top left corner)
	stz $0103 ;st2

	stz $0102 ;st1 	;Xpos  (32 is visible top left corner)
	stz $0103 ;st2 

	stz $0102 ;st1	;Sprite Address >>5
	stz $0103 ;st2 

	stz $0102 ;st1	;Sprite Attributes
	stz $0103 ;st2 
	
	lda HspriteNum
	clc
	adc #4
	sta HspriteNum
	bne ClearUnsusedSprites
	rts


UpdateSprites:
;Update the SATB address to force a copy to the graphics hardware
		st0 #$13  ;sta $0100	;VRAM-SATB Block Transfer Source
		st1 #$00  ;sta $0102
		st2 #$7F  ;sta $0103
	rts
	
Bitmap:
	incbin "\ResALL\Sprites\RawPCE_Sprite.RAW"
BitmapEnd:

PauseXY:
	nop
	nop
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
	
	
		
prepareVram:		;z_HL=VRAM address to select

	st0 #0				;Select Memory Write Reg
	lda z_e
	sta $0102 			;st1 - L address
	lda z_d
	sta $0103 			;st2 - H Address
	rts

	
cls:	
	st0 #0			;VDP reg 0 (address)
	st1 #$00		;L - Start of tilemap $0000
	st2 #$00		;H
	
	st0 #2			;Select VDP Reg2 (data)	
	
	ldx #4
	cly			;1024 tiles total (32x32)
ClsAgain:	
	st1 #0			;Fill the entire area with our "Space tile"
	st2 #%00000001		;(tile 256)
	dey
	bne ClsAgain
	dex 
	bne ClsAgain
	rts
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;x,y pos = bc / width+height = hl

docrop_alloffscreen:
	sec		;set carry = nothing to draw
	rts
	
docrop:
	ldy #0			;Y=0 throughout this routine
	sty spritehclip
	sty z_d
	sty z_e			;e=top d=bottom crop

;crop top side
	lda z_c
	sec
	sbc #vscreenminy
	bcs notcrop		;nc=nothing needs cropping
	jsr neg
	cmp z_l			
	bcs docrop_alloffscreen	;all offscreen
	sta z_e				;top crop
	and #%00000111
	eor #%00000111		;Sprite Y shift 
	inc
notcrop:
	sta z_c				;draw ypos
	
;crop bottom hand side
	clc
	adc z_l				;add height
	sec
	sbc #vscreenhei-vscreenheiclip	;logical height of screen
	bcc nobcrop			;c=nothing needs cropping
	and #%11111000
	cmp z_l				;no pixels onscreen?
	bcs docrop_alloffscreen	;all offscreen
	sta z_d				;bottom crop
nobcrop:

;Calculate new height
	lda z_e			;units to remove from top
	clc				;units to remove from bottom
	adc z_d
	beq novclip	;nothing to remove?
	jsr neg
	clc
	adc z_l			;subtract from old height
	and #%11111000
	beq docrop_alloffscreen
	sta z_l			;new height


;remove lines from source bitmap (z_iy)	
	lda z_e				;lines to remove from top?
	beq novclip			;any lines to remove from the top?
	clc
	adc #8				;Shift by 16 pixels for hidden sprite
	lsr
	lsr
	lsr					;16 pixels (8 logical units) per sprite
	beq novclip			;any lines to remove from the top?
	tax					;Count
	
	lda z_h				;calc bytes per 2 lines 
	lsr						;(2 physical lines per logical y co-ord)
	lsr
	sta z_e
	
	lda z_iyl
	clc
movedownaline:
	;remove lines from the top (start pos of source data)
	adc z_e			;Add E to L
	bcc movedownalineB
	inc z_iyh
	clc
movedownalineB:		
	dex
	bne movedownaline
	sta z_iyl
novclip:


	;ldy #0		;Y=0 throughout this routine
	sty z_d
	sty z_e		;e=top d=bottom crop

;crop left hand side
	lda z_b
	sec
	sbc #vscreenminx 		;remove left virtual border
	bcs nolcrop		;nc=nothing needs cropping
	jsr neg
	clc
	adc #7				;Shift half sprite
	cmp z_h				;no pixels onscreen?
	bcc cropc
	jmp docrop_alloffscreen	;all offscreen
cropc
	sta z_e				;left crop
	and #%00000111			
	eor #%00000111		;Sprite X shift 
nolcrop:
	sta z_b				;draw xpos
	
;crop right hand side
	clc
	adc z_h				;add width
	sec
	sbc #vscreenwid-vscreenwidclip	;logical width of screen
	bcc norcrop			;c=nothing needs cropping
	cmp z_h				;no pixels onscreen?
	bcc  cropd
		jmp docrop_alloffscreen	;all offscreen
cropd:
	sta z_d				;right crop
norcrop:

;Calculate new width
	lda z_d				;units to remove from left
	clc
	adc z_e				;units to remove from right
	and #%11111000
	lsr 
	lsr
	beq nohclip			;nothing to crop?

	sta spritehclip	;number of horizontal bytes to skip
						;after each line
	asl
	asl
	jsr neg
	clc
	adc z_h
	bne cropb
		jmp docrop_alloffscreen	;nothing to draw?
cropb:
	sta z_h				;width

	lda z_e				;amount to subtract from left
	lsr 
	lsr					;2 patterns per tile
	clc
	adc z_iyl
	sta z_iyl	;move across horizontal sprite.
	bcc nohclip
	inc z_iyh
nohclip:
	
	lsr z_h				;1/8th Height (16 pixel blocks)
	lsr z_h
	lsr z_h

	lsr z_l				;1/8th Height (16 pixel blocks)
	lsr z_l
	lsr z_l

	clc 			;clear carry = Draw OK
	rts 
	
	

	
		
neg:				;Negate a 
	eor #255
	clc
	adc #1
	rts
	
	
	
	

	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	org $fffe
	dw ProgramStart			;Reset Vector 
	
