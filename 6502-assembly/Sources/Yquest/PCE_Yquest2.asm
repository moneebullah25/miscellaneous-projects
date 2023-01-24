PlayerHsprite equ 1
;Screen Size
ScreenWidth32 equ 1
ScreenWidth equ 32
ScreenHeight equ 28
ScreenObjWidth equ 128-4
ScreenObjHeight equ 224-8

;Masks for un-drawable co-ordinates 


	include "\SrcAll\BasicMacros.asm"

z_Regs 	equ $60			;Fake Registers
UserRam equ $2200		;Game Vars

SPpage equ $2100
ZPpage equ $0000
	

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
	st1 #%11000000		;Background ON, Sprites On
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
	
;	Init Palette	
	ldx #0
	ldy #0
	loadpair z_hl,Palette
	stz $0402			;Palette address L
	stz $0403			;Palette address H
PaletteAgain:
	lda (z_hl),y
	sta $0404		;GGRRRBBB
	iny
	lda (z_hl),y
	sta $0405		;-------G
	iny
	inx
	cpx #16
	bne PaletteAgain
	
; Sprite Palette	
	ldx #0
	ldy #0
	loadpair z_hl,Palette
	stz $0402			;Palette address L
	lda #1
	sta $0403			;Palette address H
PaletteAgain2:
	lda (z_hl),y
	sta $0404		;GGRRRBBB
	iny
	lda (z_hl),y
	sta $0405		;-------G
	iny
	inx
	cpx #16
	bne PaletteAgain2
	
	
;Set up tile patterns
	loadpair z_hl,Bitmap	;Source Bitmap Data
	loadpair z_bc,(BitmapEnd-Bitmap);Source Bitmap Data Length
	loadpair z_de,$1000		;Tile 384 (256+128 - 32 bytes per tile)	
	jsr DefineTiles		;Define the tile patterns
	
;Set up Sprite patterns
	loadpair z_hl,Sprite	;Source Bitmap Data
	loadpair z_bc,(SpriteEnd-Sprite);Source Bitmap Data Length
	loadpair z_de,$2000
	jsr DefineTiles		;Define the tile patterns
	
	
;Clear Game Data
	loadpair z_hl,UserRam	;Start
	loadpair z_bc,$800		;Bytes
	jsr cldir0				;Zero Range
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
ShowTitle:
;Init Game Defaults
	lda #3
	sta lives			;Life count
	lda #0
	sta level  			;Level number
	sta PlayerObject	;Player Sprite
	jsr ChibiSound		;Mute sound

	jsr cls				;Clear Screen

	
;Show Title Screen	
	loadpair z_hl,titlepic	;Title Image
	ldy #0
titlepixnexty:
	ldx #0
titlepixnextx:
	pushpair z_hl
	pushxy
		ldx #0
		lda (z_hl,x)	  ;Sprite number
		beq titlenosprite	
		jsr GetSpriteAddr ;Get Sprite Ram Addr
	pullxy
	pushxy
		iny					;Move down two lines
		iny
		jsr showsprite		;Show the sprite
titlenosprite:
	pullxy
	pullpair z_hl
	jsr inchl
	inx
	cpx #ScreenWidth		;Screen Width
	bne titlepixnextx
	iny
	cpy #24					;Screen Height
	bne titlepixnexty

	
	
	ldx #$0D
	ldy #$10+2
	loadpair z_hl,txtFire	;Show Press Fire
	jsr LocateAndPrintString

	ldx #$10
	ldy #$00+2
	loadpair z_hl,TxtHiScore		
	jsr LocateAndPrintString
	loadpair z_de,HiScore	;Show the highscore
	ldx #4
	jsr BCD_Show
	
	LoadXY $1204
	loadpair z_hl,txtUrl	;Show URL
	jsr LocateAndPrintString	
	
startlevel:
	jsr waitforfire	
	jsr cls
	jsr ResetPlayer			;Center Player
	jsr levelinit			;Set up enemies
	

	lda #2
	sta z_c					;H Sprite number
	loadpair z_ix,BulletArray;First Object
	lda #BulletCount		
	sta z_b					;Sprite Count
	jsr SetHardwareSprites	;Define Sprites
	
	loadpair z_ix,EnemyBulletArray	;First Object
	lda #BulletCount
	sta z_b					;Sprite Count
	jsr SetHardwareSprites	;Define Sprites
	
	loadpair z_ix,ObjectArray	;First Object
	lda #Enemies
	sta z_b					;Sprite Count
	jsr SetHardwareSprites	;Define Sprites
	
	
	
	
infloop:					;Main loop
	ldx #255				;Keypresses
	ldy #255				;Delay
PauseY
	lda #6					;Delay2
	sta z_b
PauseB
	Pushxy
		jsr Player_ReadControlsDual	;Get Keypresses
	pullxy
	lda z_h
	cmp #255				;Key Pressed?
	beq NoButton
	tax						;Yes - store for later
NoButton:
	dec z_b
	bne PauseB
	dey 
	bne PauseY
	
	
	txa
	pha
		jsr drawui		;Show User Interface
	
		loadpair z_ix,PlayerObject
		jsr BlankSprite	;Remove old player sprite
	pla
	sta z_h
	
	lda KeyTimeout	;ignore UDLR during key timeout
	beq ProcessKeys
	dec KeyTimeout
	jmp JoySkip			;skip player input
	
ProcessKeys:
	ldx #0			;Key Timeout
	ldy #O_Yacc
	lda z_h
	and #%00000001	;RSBALDRU 
	bne JoyNotUp	;Jump if UP not presesd
	jsr DEC_IX_Y
	ldx #5
JoyNotUp:
	lda z_h
	and #%00000100	;RSBALDRU 
	bne JoyNotDown	;Jump if DOWN not presesd
	jsr INC_IX_Y
	ldx #5
JoyNotDown:
	ldy #O_Xacc
	lda z_h
	and #%00001000	;RSBALDRU 
	bne JoyNotLeft 	;Jump if LEFT not presesd
	jsr DEC_IX_Y
	ldx #5
JoyNotLeft:
	lda z_h
	and #%00000010	;RSBALDRU 
	bne JoyNotRight	;Jump if RIGHT not presesd
	jsr INC_IX_Y
	ldx #5
JoyNotRight:
	lda z_h
	and #%00010000	;RSBALDRU 
	bne JoyNotFire	;Jump if Fire not presesd
	pushX
		jsr PlayerFirebullet	;Fire a bullet
	pullX
JoyNotFire: 
	lda z_h
	and #%00100000	;RSBALDRU 
	bne JoyNotFire2	;Jump if Fire not presesd
	lda #0
	sta PlayerAccX	;Stop movement
	sta PlayerAccY
JoyNotFire2
	stx KeyTimeout	;Update KeyTimeout
JoySkip: 
	
	jsr drawandmove	;Draw Player Sprite
		
	jmp infloop


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

cls:		;Clear screen with Space sprite
	ldy #0
clsnexty:
	ldx #0
clsnextx:
	pushxy
		lda #0
		sta z_h
		jsr showsprite		;Show the sprute
	pullxy
	inx
	cpx #ScreenWidth		;Screen Width
	bne clsnextx
	iny
	cpy #ScreenHeight+4		;Screen Height
	bne clsnexty
	
	lda #255				;Position all the sprites offscreen
	sta z_ixh
	sta z_ixl
	sta z_iyh
	sta z_iyl
	lda #0
Cls_NextSprite:	
	pha
	jsr SetHardwareSprite	;A=Hardware Sprite No. IX = X , IY=Y
	pla
	clc
	adc #1
	cmp #64
	bne Cls_NextSprite
	rts	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PrintChar:
	sec
	sbc #32				;No char below space
	sta z_as
	pushxy
	pushpair z_hl
	pushpair z_bc
		lda z_as
		sta z_h
		
		ldx CursorX		
		ldy CursorY		
		
		jsr showsprite	;Show the character
		inc CursorX
	pullpair z_bc
	pullpair z_hl
	pullxy
	rts

BlankSprite:
	ldy #O_CollProg
	lda (z_IX),y
	cmp #250			;Don't show object if unused
	bcc DoBlank
	rts
DoBlank:
	ldy #O_HSprNum
	lda (z_IX),y
	beq DoBlank2
	sec
	sbc #1
	cmp #128
	bcs DoBlank2
;Blank Hardware Sprite
	sta z_as
	pushpair z_bc
	pushpair z_ix
	pushpair z_iy
		loadpair z_ix,0
		loadpair z_iy,0
		jmp DoGetHSpriteObj2
DoBlank2:
	lda #0
	sta z_H
	jmp DrawBoth


			

GetSpriteAddr:
	clc 
	adc #96				;First 96 sprites are font
	sta z_h
	lda SpriteFrame
	asl					;16 sprites per bank
	asl
	asl
	asl
	clc
	adc z_h
	sta z_h
	rts
	
DoGetSpriteObj:		;Get Settings from Object IX
		ldy #0
		lda (z_ix),y	;Spr
		jsr GetSpriteAddr
DrawBoth:
		ldy #2
		lda (z_ix),y	;object Xpos / 4
		lsr
		lsr
		tax
		
		iny
		lda (z_ix),y	;object Ypos /8
		lsr
		lsr
		lsr
		tay
		
showsprite:
	lda z_h
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
	
	
DoGetHSpriteObj:
	sta z_as				;Hardware sprite number
	pushpair z_bc
	pushpair z_ix
	pushpair z_iy
			lda SpriteFrame	
			asl				;SpriteFrame*16 (16 sprites per bank)
			asl
			asl
			asl
			sta z_h
			ldy #O_SprNum
			lda (z_IX),y	;Sprite Source Num
			clc
			adc z_h			
			asl
			sta z_e			;Store in Z_E
			lda #$1 
			sta z_d			;D,E = Source pattern
			
			ldy #O_Ypos		;Ypos->IYL
			lda (z_IX),y
			sta z_iyl
			ldy #O_Xpos		;Xpos->IXL
			lda (z_IX),y	
			asl 
			sta z_ixl		
			
			lda #0
			sta z_iyh		;High bytes of XY
			sta z_ixh
;32,64 is first Top left visible pixel
			addpair z_ix,32		
			addpair z_iy,64+8
DoGetHSpriteObj2:			
				; Y-yyX--x	;Y= Yflip y=ysize / X=xflip x=xsize 
			lda #%00000000	;size allows tilling for up to 32x64
			sta z_h
				; F---PPPP - F=Foreground / P=Palette
			lda #%10000000
			sta z_l			;HL=Palette etc
DrawBothHard:
		lda z_as
		jsr SetHardwareSprite
	pullpair z_iy
	pullpair z_ix
	pullpair z_bc
	rts	
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

GetVDPScreenPos:	; BC=XYpos	
		st0 #0			;Select Vram Write
		lda z_c
		ifdef ScrWid256	;256x192
			clc
			adc #2
		endif
		asl
		asl
		asl
		asl
		asl
		clc
		adc z_b			;Add Xpos
		sta $0102		;Send to Data-L
		
		lda z_c
		ifdef ScrWid256	;256x192
			clc
			adc #2
		endif
		and #%11111000	;Multiply Ypos by 32 - low byte
		lsr
		lsr
		lsr
		sta $0103		;Send to Data-H
	rts
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			
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
	
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
waitforfire:

	jsr dorandom				;reseed random numbers
	jsr Player_ReadControlsDual	;RLDUSsBA 
	and #%00010000	;RSBALDRU 
	bne waitforfire

waitforfireb:
	jsr dorandom				;reseed random numbers

	jsr Player_ReadControlsDual	;RLDUSsBA 
	and #%00010000	;RSBALDRU 
	beq waitforfireb
	rts
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
	lda z_h
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

	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SetHardwareSprite:		;A=Hardware Sprite No. IX = X , IY=Y , 
	pha		 				;D,E = Source Data, HL=Palette etc
		asl						;4 bytes per sprite
		asl
;$7Fxx Sprite attribute table buffer(copy in ram) 4 bytes per sprite x 64 sprites
		st0 #0	  ;sta $0100	;Select Write Address ($00)		
		sta $0102 ;st1 			;Low Byte of address  (Sprite x 4)
		st2 #$7F  ;sta $0103	;High Byte of address ($7Fxx)
		
		st0 #2	  ;sta $0100	;Data Write ($02)
		
		lda z_iyl				;Ypos (64 is visible top left corner)
		sta $0102 ;st1 
		lda z_iyh
		sta $0103 ;st2
		
		lda z_ixl				;Xpos  (32 is visible top left corner)
		sta $0102 ;st1 
		lda z_ixh
		sta $0103 ;st2 
		
		lda z_e					;Sprite VRAM Address >>5
		sta $0102 ;st1
		lda z_d
		sta $0103 ;st2 
		
		lda z_l					;Sprite Attributes
		sta $0102 ;st1
		lda z_h
		sta $0103 ;st2 
	
;Update the STAB address to force a copy to the graphics hardware
		st0 #$13  ;sta $0100	;VRAM-SATB Block Transfer Source
		st1 #$00  ;sta $0102
		st2 #$7F  ;sta $0103
	pla
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Bitmap:
	incbin "\ResAll\Yquest\FontPCE.raw"
	
	incbin "\ResAll\Yquest\PCE_YQuest.RAW"
	incbin "\ResAll\Yquest\PCE_YQuest2.RAW"
	incbin "\ResAll\Yquest\PCE_YQuest3.RAW"
	incbin "\ResAll\Yquest\PCE_YQuest4.RAW"
BitmapEnd
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
;Cartridge footer (must be in first bank)

	org $5ffe
	dw $E000			;Reset Vector 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Sprite:
	;incbin "\ResALL\Sprites\SpritePCE.raw"
	incbin "\ResAll\Yquest\PCE_YquestSprites.RAW"
	incbin "\ResAll\Yquest\PCE_YquestSprites2.RAW"
	incbin "\ResAll\Yquest\PCE_YquestSprites3.RAW"
	incbin "\ResAll\Yquest\PCE_YquestSprites4.RAW"
SpriteEnd:
		
	include "\SrcAll\V1_ChibiSound.asm"
	include "\SrcAll\BasicFunctions.asm"
	include "\SrcAll\BCD.asm"
	include "\srcALL\MultiPlatform_ShowDecimal.asm"

	include "YQ_Multiplatform2.asm"
	include "YQ_DataDefs.asm"
	include "YQ_RamDefs.asm"
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

palette:
	dw %0000000000000000; ;0  %-------GGGRRRBBB
    dw %0000000111000000; ;1  %-------GGGRRRBBB
    dw %0000000010010010; ;2  %-------GGGRRRBBB
    dw %0000000101101101; ;3  %-------GGGRRRBBB
    dw %0000000111111111; ;4  %-------GGGRRRBBB
    dw %0000000100001011; ;5  %-------GGGRRRBBB
    dw %0000000110001001; ;6  %-------GGGRRRBBB
    dw %0000000001111001; ;7  %-------GGGRRRBBB
    dw %0000000011111011; ;8  %-------GGGRRRBBB
    dw %0000000101111010; ;9  %-------GGGRRRBBB
    dw %0000000111111010; ;10  %-------GGGRRRBBB
    dw %0000000001101101; ;11  %-------GGGRRRBBB
    dw %0000000000111111; ;12  %-------GGGRRRBBB
    dw %0000000001000110; ;13  %-------GGGRRRBBB
    dw %0000000011001101; ;14  %-------GGGRRRBBB
    dw %0000000110000111; ;15  %-------GGGRRRBBB

	org $ffff
	db $0			;End of cartridge
	
	
	