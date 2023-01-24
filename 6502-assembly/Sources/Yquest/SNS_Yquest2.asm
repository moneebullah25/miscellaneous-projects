PlayerHsprite equ 1

;Mask collision detection routines
;CollisionMaskY equ %11111000 
;CollisionMaskX equ %11111100	

ScreenWidth32 equ 1
ScreenWidth equ 32
ScreenHeight equ 24
ScreenObjWidth equ 128-8
ScreenObjHeight equ 192-8

ScrWid256 equ 1			;Snes Centering
z_Regs 	equ $20			;Temp Vars

SnesScreenBuffer equ $0200 	;Tilemap buffer at $0200+ ($800 bytes)
SnesSpriteBuffer equ $0A00 	;Sprite buffer at $0A00+ ($220 bytes)
UserRam equ 		 $1000	;Game Vars
	include "\SrcAll\BasicMacros.asm"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		
	org $8000		;Start of ROM
	SEI				;Stop interrupts
  
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
	ldx #16
	loadpair z_hl,Palette
	stz $2121		;CGADD - Colour selection  
	jsr SetPalette
	
	
		
;Set Scroll position
	stz $210D  		;BG1HOFS BG1 horizontal scroll   
	stz $210D  		;BG1HOFS
	
	lda #-1
	sta $210E  		;BG1VOFS BG1 vertical scroll 
	stz $210E  		;BG1VOFS
	
;Clear Buffer
	loadpair z_hl,SnesScreenBuffer
	loadpair z_bc,$800		;;32x32x2= $800
	jsr cldir0				;Zero Range

	
	
;TileDefs	
	;  i000abcd - I 0=inc on $2118 or $2139 1=$2119 or $213A… abcd=move size
	stz $2115 		;VMAIN - Video port control (Inc on write to $2118)
	
	loadpair z_hl,Bitmap	;Source Bitmap Data
	loadpair z_bc,(BitmapEnd-Bitmap);Bitmap Data Length
	loadpair z_de,$1000	;Tile 0 (16 bytes per tile)	
	jsr DefineTiles		;Define the tile patterns
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Set Sprite palette
	ldx #16
	loadpair z_hl,Palette
	lda #128
	sta $2121		;CGADD - Colour selection  
	jsr SetPalette	
	
;Sprite Ram Pos	
	lda #%00000010	;Set Sprite pos to $4000
	sta $2101		;OAM settings
		
;Define Sprite Bitmaps
	loadpair z_hl,Sprites
	loadpair z_bc,(SpritesEnd-Sprites)
	loadpair z_de,($4000)
	jsr DefineTiles		
	

;Clear Sprite Buffer
	loadpair z_hl,SnesSpriteBuffer
	loadpair z_bc,$220		;;32x32x2= $800
	jsr cldir0				;Zero Range
	
;Turn on the screen	+ Sprites
		; ---S4321 - S=sprites 4-1=enable Bgx
	lda #%00010001	;Turn on BG1 + SPRITES 
	sta $212C 		;Main screen designation [TM]    


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				
	
	;	  x000bbbb - x=screen disable (1=disable) bbbb=brightness (15=max)
	lda #%00001111	;Screen on
	sta $2100		;INIDISP - Screen display register

	jsr ChibiSound_INIT				;init SPC700 code
	
	lda #%10000000					;Turn on interrupts
	sta $4200

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
	loadpair z_hl,titlepic	;TitlePicture source
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
		jsr showsprite		;Show the sprute
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
	ldy #$10
	loadpair z_hl,txtFire		;Show Press Fire
	jsr LocateAndPrintString

	ldx #$10
	ldy #$00
	loadpair z_hl,TxtHiScore		
	jsr LocateAndPrintString

	loadpair z_de,HiScore		;Show the highscore
	ldx #4
	jsr BCD_Show
	
	LoadXY $1202
	loadpair z_hl,txtUrl		;Show URL
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
	lda #3
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
	and #%00010000	;RLDUSsBA 
	beq JoyNotUp	;Jump if UP not presesd
	jsr DEC_IX_Y
	ldx #5
JoyNotUp:
	lda z_h
	and #%00100000	;RLDUSsBA 
	beq JoyNotDown	;Jump if DOWN not presesd
	jsr INC_IX_Y
	ldx #5
JoyNotDown:
	ldy #O_Xacc
	lda z_h
	and #%01000000	;RLDUSsBA 
	beq JoyNotLeft 	;Jump if LEFT not presesd
	jsr DEC_IX_Y
	ldx #5
JoyNotLeft:
	lda z_h
	and #%10000000	;RLDUSsBA 
	beq JoyNotRight	;Jump if RIGHT not presesd
	jsr INC_IX_Y
	ldx #5
JoyNotRight:
	lda z_h
	and #%00000001	;RLDUSsBA 
	beq JoyNotFire	;Jump if Fire not presesd
	pushX
		jsr PlayerFirebullet	;Fire a bullet
	pullX
JoyNotFire: 
	lda z_h
	and #%00000010	;RLDUSsBA 
	beq JoyNotFire2	;Jump if Fire not presesd
	lda #0
	sta PlayerAccX	;Stop movement
	sta PlayerAccY
JoyNotFire2
	stx KeyTimeout	;Update KeyTimeout
JoySkip: 
	
	jsr drawandmove	;Draw Player Sprite

	jmp infloop


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
cls:
;Clear Tilemap buffer
	loadpair z_hl,SnesScreenBuffer
	loadpair z_bc,$800		;Bytes
	jsr cldir0				;Zero Range
	
;Clear Sprite Buffer
	loadpair z_hl,SnesSpriteBuffer
	loadpair z_bc,$220		;Bytes
	jsr cldir0				;Zero Range
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
		
		jsr showsprite
		inc CursorX
	pullpair z_bc
	pullpair z_hl
	pullxy
	rts

BlankSprite:
	ldy #O_CollProg
	lda (z_IX),y
	cmp #250
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
		loadpair z_ix,255
		loadpair z_iy,255
		stz z_h
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
		lda (z_ix),y	;object Xpos * 2
		lsr
		lsr
		tax
		
		iny
		lda (z_ix),y	;object Ypos (ignore 3 bits)
		and #%11111000
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
	ldy #0
	sta (z_hl),Y 	;$2118 VMDATAL
	iny
	lda #$00		;vhoppptt
	sta (z_hl),Y 	;$2119 VMDATAH Video port data   
	rts

		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		
DoGetHSpriteObj:
	sta z_as
	pushpair z_bc
	pushpair z_ix
	pushpair z_iy
			lda SpriteFrame	;Show Sprite of Objec XY
			asl				;SpriteFrame*16 (16 sprites per bank)
			asl
			asl
			asl
			sta z_h
			ldy #O_SprNum
			lda (z_IX),y	;Sprite Source
			clc
			adc #1			;First sprite is blank
			adc z_h
			sta z_h
				 ;YXPPCCCT - Y=yflip X=xflip P=priority compared to BG...
			lda #%00110000		; C=palette +128 T= Tile Pattern number
			sta z_l	
			
			ldy #O_Ypos		;Get Ypos->IYL
			lda (z_IX),y
			sta z_iyl
			
			ldy #O_Xpos		;Get Xpos->IXL
			lda (z_IX),y	
			asl 
			sta z_ixl
			
			stz z_iyh
			stz z_ixh
			addpair z_iy,16	;Move sprites down 16 lines
DoGetHSpriteObj2:			
		lda z_as
		jsr SetHardwareSprite
	pullpair z_iy
	pullpair z_ix
	pullpair z_bc
	rts	
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

GetVDPScreenPos:	; BC=XYpos	
	lda z_c
	ifdef ScrWid256	;256x192
		clc
		adc #2
	endif
	sta z_h			;32 tiles per Y line - 2 bytes per tile
	lda #0
	clc
	ror z_h
	ror 
	ror z_h
	ror 
	
	adc z_b 		;2 Bytes per X tile
	adc z_b 	
	sta z_l
	
	lda z_h
	adc #0
	sta z_h
								
	lda z_h
	adc #>SnesScreenBuffer	;Must be byte aligned $??00
	sta z_h
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
	;BC=Bytes
	;DE=Destination Ram
	;HL=Source Bytes
DefineTiles:
	jsr prepareVram			;Select Address DE
	
	ldy #0
DefineTilesAgain	
	jsr WaitVblank			;Wait for vblank
	lda (z_HL),Y	
	sta $2119				;Transfer First Byte
	jsr DecBC
	jsr incHL
		
	jsr WaitVblank			;Wait for vblank
	lda (z_HL),Y	
	sta $2118				;Transfer 2nd byte (+autoinc)
	jsr DecBC
	jsr incHL
		
	lda z_b					;Repeat until z_bc=0
	ora z_c
	bne DefineTilesAgain
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
	
prepareVram:	;Select VRAM address z_de		
	jsr WaitVblank
	lda z_e
	sta $2116	;VMADDL - Destination address in VRAM L
	lda z_d
	sta $2117	;VMADDH - Destination address in VRAM H
	rts
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

WaitVblank:
	lda $4212 		;HVBJOY - Status 	
		; xy00000a	- x=vblank state y=hblank state a=joypad ready
	and #%10000000
	beq WaitVblank	;Wait until we get nonzero 
	rts					;This means we're in VBLANK
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
SetHardwareSprite:			;Write sprite data into cache
	tay
	pushpair z_de
	tya	
;1st part
			sta z_e			;Address L
			lda #0
			asl z_e
			rol 
			asl z_e			;4 Bytes per sprite
			rol 
			clc
			adc #>SnesSpriteBuffer
			sta z_d			;Address H
			lda z_ixl
			ldx #0
			sta (z_de,x) 	;X
			jsr IncDE
			lda z_iyl
			sta (z_de,x)  	;Y
			jsr IncDE
			lda z_h
			sta (z_de,x) 	;Tile
			jsr IncDE
			lda z_l
			sta (z_de,x)  	;Attribs
			jsr IncDE
;2nd part
		tya
		and %11111100
		lsr					;1 byte per 4 sprites
		lsr
		sta z_e				;Address L
		
		lda #(>SnesSpriteBuffer+2)			
		sta z_d				;Address H
		ldx #0
		lda (z_de,x)
		sta z_as			;Get current attr2
		 
		tya
		and #%00000011		;4 sprites per byte of attr2
		tax
		 
		lda #%11111100		;Prep the mask
		sta z_b
		lda z_ixh
		and #%00000011		;2 bits of new sprite settings setting
		 
		cpx #0				;Shift bits ------sx into correct position
		beq SpriteSkipShift
SpriteShiftAgain:
		asl
		asl					;Shift new val
		sec
		rol z_b
		sec
		rol z_b				;Shift mask
		dex
		bne SpriteShiftAgain
SpriteSkipShift: 
		pha
			lda z_as		;Get back current value
			and z_b			;Apply mask
			sta z_as		;Get back current value
		pla
		ora z_as
		ldx #0
		sta (z_de,x)
	pullpair z_de
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
SetPalette:		;Set X bytes of palette from z_hl
	ldy #0
SetPaletteAgain:
	lda (z_hl),y	;gggrrrrr 
	sta $2122		;CGDATA - Colour data register
	iny 		
	lda (z_hl),y	;?bbbbbgg 
	sta $2122		;CGDATA
	iny
	dex
	bne SetPaletteAgain
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
CustomNmihandler:		;Vblank interrupt handler
	php
	pha
;Copy Sprites (OAM) Via DMA
		stz $2102
		stz $2103           ; Point to start of OAM

		lda #0				;Write mode 000=one byte address repeating 
		sta $4300
		lda #04
		sta $4301			;Destination $21xx= $2104
		
		lda #<SnesSpriteBuffer
		sta $4302			;Source (24 bit - Little endian)
		lda #>SnesSpriteBuffer
		sta $4303
		lda #0				;bits 16-23
		sta $4304
			
		lda #<$220
		sta $4305	
		lda #>$220			;No of bytes (24 bit - Little endian
		sta $4306
		lda #0
		sta $4307
		
		lda #%00000001		
		sta $420B			;enable DMA 0 (bit0=1)	
	
;Copy Tilemap Via DMA
		lda #0
		sta $2116		;MemL -Video port address [VMADDL/VMADDH]                            
		sta $2117		;MemH
		
		lda #128
		sta $2115			;Inc address on write to $2119
			 
		lda #%00000001		;Write mode 001=two bytes alternate
		sta $4300
		
		lda #$18
		sta $4301			;Destination $21xx= 2118
		
		lda #<SnesScreenBuffer
		sta $4302			;Source (24 bit - Little endian)
		lda #>SnesScreenBuffer
		sta $4303
		lda #0				;bits 16-23
		sta $4304
		
		lda #<(32*32*2)
		sta $4305			;No of bytes (24 bit - Little endian
		lda #>(32*32*2)			;(only 1st 16 bits used?)
		sta $4306
		lda #0
		sta $4307
		
		lda #0
		sta $420C			;Disable H-DMA transfer 
		lda #%00000001		
		sta $420B			;enable DMA 0 (bit0=1)
		
		
;Dma's finished 
		lda #0
		sta $2115			;Inc address on write to $2118
	pla
	plp
	rti

	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Player_ReadControlsDual:
	txa
	pha
		;Strobe joysticks to reset them
		ldx #$01		;Send a 1 to joysticks (strobe reset)
		stx $4016		;JOYPAD1 port
		
		dex 			;Send a 0 to joysticks (read data)
		stx $4016		;JOYPAD1 port

		ldx #8			;Read in 8 bits from each joystick
Player_ReadControlsDualloop:
		lda $4016		;JOYPAD1
		lsr 	   		; bit0 -> Carry
		ror z_h  		;Add carry to Joy1 data
		dex 
		bne Player_ReadControlsDualloop
	pla 
	tax
	lda z_h
	rts
  
; $4016/$4017 - 1=Pressed / 0=NotPressed

; Read  1 - A
; Read  2 - B
; Read  3 - Select
; Read  4 - Start
; Read  5 - Up
; Read  6 - Down
; Read  7 - Left
; Read  8 - Right

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
waitforfire:

	jsr dorandom				;reseed random numbers
	jsr Player_ReadControlsDual	;RLDUSsBA 
	and #%00000001
	bne waitforfire

waitforfireb:
	jsr dorandom				;reseed random numbers

	jsr Player_ReadControlsDual	;RLDUSsBA 
	and #%00000001
	beq waitforfireb
	rts
	
	


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	
	include "\SrcAll\V1_ChibiSound.asm"
	include "\SrcAll\BasicFunctions.asm"
	include "\SrcAll\BCD.asm"
	include "\srcALL\MultiPlatform_ShowDecimal.asm"

	include "YQ_Multiplatform2.asm"
	include "YQ_DataDefs.asm"
	include "YQ_RamDefs.asm"

; $4016/$4017 - 1=Pressed / 0=NotPressed

; Read  1 - A
; Read  2 - B
; Read  3 - Select
; Read  4 - Start
; Read  5 - Up
; Read  6 - Down
; Read  7 - Left
; Read  8 - Right

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		
Bitmap:
	incbin "\ResAll\Yquest\FontSNS.raw"
	incbin "\ResAll\Yquest\SNS_YQuest.RAW"
	incbin "\ResAll\Yquest\SNS_YQuest2.RAW"
	incbin "\ResAll\Yquest\SNS_YQuest3.RAW"
	incbin "\ResAll\Yquest\SNS_YQuest4.RAW"
BitmapEnd:

Sprites:	
	ds 32
	incbin "\ResAll\Yquest\SNS_YQuest.RAW"
	incbin "\ResAll\Yquest\SNS_YQuest2.RAW"
	incbin "\ResAll\Yquest\SNS_YQuest3.RAW"
	incbin "\ResAll\Yquest\SNS_YQuest4.RAW"
SpritesEnd:

Palette:
	dw %0000000000000000; ;0  %-BBBBBGGGGGRRRRR
	dw %0000011111100010; ;1  %-BBBBBGGGGGRRRRR
	dw %0010110101101011; ;2  %-BBBBBGGGGGRRRRR
	dw %0101001010010100; ;3  %-BBBBBGGGGGRRRRR
	dw %0111111111111111; ;4  %-BBBBBGGGGGRRRRR
	dw %0011001000000101; ;5  %-BBBBBGGGGGRRRRR
	dw %0001101101000110; ;6  %-BBBBBGGGGGRRRRR
	dw %0001100011011100; ;7  %-BBBBBGGGGGRRRRR
	dw %0011000111011100; ;8  %-BBBBBGGGGGRRRRR
	dw %0010101010111100; ;9  %-BBBBBGGGGGRRRRR
	dw %0010011111111111; ;10  %-BBBBBGGGGGRRRRR
	dw %0101000010110100; ;11  %-BBBBBGGGGGRRRRR
	dw %0111110000011111; ;12  %-BBBBBGGGGGRRRRR
	dw %0110100011000000; ;13  %-BBBBBGGGGGRRRRR
	dw %0101100110000111; ;14  %-BBBBBGGGGGRRRRR
	dw %0111111101000000; ;15  %-BBBBBGGGGGRRRRR



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
;	Footer
	
	org $FFC0
     ; "123456789012345678901"
	db "Y-Quest: LearnAsm.net"	; Program title (21 byte Ascii string)

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
	dw CustomNmihandler	;Vblank interrupt handler
	dw $0000 	;Reset vector (unused)
	dw $0000 	;Irq vector   (h/v-timer/external interrupt)

;6502 mode vectors
	dw $0000 	;Reserved
	dw $0000	;Reserved
	dw $0000 	;Cop vector   (cop opcode)
	dw $0000 	;Brk vector   (unused)
	dw $0000 	;Abort vector (unused)
	dw CustomNmihandler	;Vblank interrupt handler
	dw $8000 	;Reset vector (cpu is always in 6502 mode on reset)
	dw $0000 	;Irq/brk vector
	
	
	