
sppage equ $0100


ScrWid256 equ 1			;Snes Centering
z_Regs 	equ $20			;Temp Vars

UserRam equ $200		;Game Vars

;WRAM is 128k at address $7E0000-$7FFFFF

;We're using WRAM for our buffer, this can be Read or written with 4 ports:
;2180h - WMDATA - WRAM Data Read/Write (R/W)
;2181h - WMADDL - WRAM Address (lower 8bit) (W)
;2182h - WMADDM - WRAM Address (middle 8bit) (W)
;2183h - WMADDH - WRAM Address (upper 1bit) (W)



;We can only update 1/4 of the screen per NMI ($0800 of $2000)
;Bufferbank is the offset to that 1/4 in ram 
bufferbank equ $80		;Zero page address for bufferbank

ScreenWidth32 equ 1
ScreenWidth equ 256
ScreenHeight equ 224

Color4 equ 1		;We're only using 2 bitplanes,
Color3 equ 3		;This halves the amount of data
Color2 equ 2		;We have to transfer
Color1 equ 1


	include "\SrcAll\BasicMacros.asm"

	org $8000		;Start of ROM
	SEI				;Stop interrupts
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		
;ScreenInit

		 ;aaaabbbb -aaa=base addr for BG2 bbb=base addr for BG1
	lda #%00010001
	sta $210B 		;BG1 & BG2 VRAM location register [BG12NBA]                    
	
	;     xxxxxxss 	- xxx=address… ss=SC size  00=32x32 01=64x32 10=32x64 11=64x64
	stz $2107		;BG1SC - BG1 Tilemap VRAM location
	
	
;4 color Tilemap	
	; abcdefff - abcd=tile sizes e=pri fff=mode def
	lda #%00001000
	sta $2105		;BGMODE - Screen mode register
	
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
	lda #%11100000
	sta $2122		;CGDATA - Colour data register
		 ;?bbbbbgg 
	lda #%11111111
	sta $2122		;CGDATA

;Color 2
	lda #2		;Color 2
	sta $2121		;CGADD - Colour selection  (15=Font)
		 ;gggrrrrr 
	lda #%00011111
	sta $2122		;CGDATA - Colour data register
		 ;?bbbbbgg 
	lda #%01111100
	sta $2122		;CGDATA
	
;Color 3
	lda #3		;Color 3
	sta $2121		;CGADD - Colour selection  (15=Font)
		 ;gggrrrrr 
	lda #%11100000
	sta $2122		;CGDATA - Colour data register
		 ;?bbbbbgg 
	lda #%00000011
	sta $2122		;CGDATA
	
;Color 4
	lda #4		;Color 4
	sta $2121		;CGADD - Colour selection  (15=Font)
		 ;gggrrrrr 
	lda #%11111111	
	sta $2122		;CGDATA - Colour data register
		 ;?bbbbbgg 
	lda #%00000011
	sta $2122		;CGDATA	
	
	;	  i000abcd - I 0=inc on $2118 or $2139 0=$2119 or $213A… abcd=move size
	stz $2115 		;VMAIN - Video port control (Inc on write to $2118)
		
;Set Scroll position
	stz $210D  		;BG1HOFS BG1 horizontal scroll   
	stz $210D  		;BG1HOFS
	
	lda #-1
	sta $210E  		;BG1VOFS BG1 vertical scroll 
	stz $210E  		;BG1VOFS
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
;Fill Tilemap with concecutive tiles (0+)	
	stz z_b				;Start SX
	stz z_c				;Start SY
	ldx #32				;Width in tiles
	ldy #32				;Height in tiles
		
	loadpair z_de,0		;TileStart (Tile 0)
	jsr FillAreaWithTiles ;Draw the tiles to screen
	
;Turn on the screen	
		; ---S4321 - S=sprites 4-1=enable Bgx
	lda #%00000001		;Turn on BG1
	sta $212C 			;Main screen designation [TM]    
	
	;	  x000bbbb - x=screen disable (1=disable) bbbb=brightness (15=max)
	lda #%00001111		;Screen on
	sta $2100			;INIDISP - Screen display register
	
	lda #0
	stz Bufferbank		;Set Buffer Start
	
	lda #%10000000		;Turn on interrupts
	sta $4200
	
	loadpair z_hl,userram
	loadpair z_bc,256
	jsr CLDIR0			;Clear Game Ram
		
	jsr MainMenu		

	
infloop:					;main loop
	lda tick
	clc
	adc #1
	and #%00000001
	sta tick

	loadpair z_bc,200		;slow down delay

	lda boost
	bne boostoff
	loadpair z_bc,80		;boost - no delay 
								;(compensate for font draw)
boostoff:
	lda #0
	sta z_d					;key buffer
pausebc:
	pushpair z_bc
		pushpair z_de
			jsr Player_ReadControlsDual	
		pullpair z_de

		lda z_h
		cmp #0				;Key Pressed?
		beq pausenokey

		sta z_d
		jmp keysdown		;store any pressed joystick buttons
pausenokey:
		stz keytimeout		;released - nuke key, and relese keypress
		stz z_d				;Clear key
keysdown:
	pullpair z_bc
	jsr decbc

	lda z_b
	ora z_c
	bne pausebc				;See if BC=0

	
	lda keytimeout			;See if Keytimeout is set
	bne joyskip				
	
	lda #1					;Reset Boost
	sta boost
processkeys:
	loadpair z_ix,playerxacc ;point ix to player accelerations 

	lda z_d
	and #%01000000			;RLDUSsBA - L
	beq joynotleft
	
	dec playerdirection
	jsr setplayerdirection

	lda #1
	sta keytimeout

joynotleft:
	lda z_d
	and #%10000000			;RLDUSsBA - R
	beq joynotright
	
	inc playerdirection
	jsr setplayerdirection
	
	lda #1					;ignore keypresses		
	sta keytimeout

joynotright:
	lda z_d
	and #%00000010			;RLDUSsBA - Fire
	beq joynotfire

	lda boostpower			;check if boost power remains
	beq joynotfire

	lda #0
	sta boost
joynotfire:
joyskip:
	jsr handleplayer		;draw and update player
	jsr handlecpu			;draw and update cpu
	jmp infloop


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
pset:			;x=ab y=c color=d
	pushpair z_bc
		loadpair z_hl,PixelMask
		lda z_b
		and #%00000111
		tay
		lda (z_hl),y
		pha
			jsr CalcVramAddr			
			ldx $2180		;Wram DATA - Bitplane 0
			ldy $2180		;Wram DATA - Bitplane 1

			lda z_l		;Reset WRAM address for Writes
			sta $2181		;WMADDL 
			lda z_h
			sta $2182		;WMADDM 
			stz $2183		;WMADDH 
		pla
		sta z_b				;Pixel mask
		eor #255
		sta z_c				;Background Mask
		
		txa
		and z_c
		ror z_d				;Test Bit 0
		bcc PsetSkip1
		ora z_b
PsetSkip1:
		sta $2180			;Update Bitplane 0
		
		tya
		and z_c
		ror z_d				;Test Bit 1
		bcc PsetSkip2
		ora z_b
PsetSkip2:
		sta $2180			;Update Bitplane 1
	pullpair z_bc
	rts
	
	align 4
PixelMask:
	db %10000000,%01000000,%00100000,%00010000
	db %00001000,%00000100,%00000010,%00000001
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
point:
	pushpair z_bc
		loadpair z_hl,PixelMask
		lda z_b
		and #%00000111
		tay
		lda (z_hl),y
		pha
			jsr CalcVramAddr
			ldx $2180		;Wram DATA - Bitplane 0
			ldy $2180		;Wram DATA - Bitplane 1
		pla
		sta z_b
		lda #0
		sta z_d
		
		tya
		and z_b				;Bitplane 0
		beq PointSkip1
		sec
		rol z_d
PointSkip1:
		txa
		and z_b				;Bitplane 1
		beq PointSkip2
		sec
		rol z_d
PointSkip2:
	pullpair z_bc
	lda z_d
	rts
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
CalcVramAddr:
	pushpair z_de
		loadpair z_hl,$2000		;using WRAM Base $7E2000
		loadpair z_de,$0000
		
		lda z_b
		and #%11111000			;Xpos * 16
		asl
		rol z_d
		sta z_e
		jsr addhl_de
		
		stz z_d
		lda z_c
		and #%00000111			;Ypos *2
		asl
		rol z_d
		sta z_e
		jsr addhl_de
		
		stz z_e
		lda z_c
		and #%11111000			;*512 (32*2*8)
		lsr						;Width*Lines*Bitplanes
		ror z_e
		lsr
		ror z_e
		sta z_d
		jsr addhl_de
		
		lda z_l
		sta $2181	;WMADDL 
		lda z_h
		sta $2182	;WMADDM 
		stz $2183	;WMADDH 
	pullpair z_de
	rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
cls:			;Clear pattern cache

	stz $2181	;WMADDL 
	lda #$20
	sta $2182	;WMADDM 
	stz $2183	;WMADDH 
	
	ldy #$40	;Transfer $4000 bytes
	ldx #0
	
FillB:	
	stz $2180	;WMDATA 
	dex
	bne FillB
	dey
	bne FillB
	rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	


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
		clc
		adc z_b 		;Add X line
		sta z_l
		jsr WaitVblank
		
		lda z_l
		sta $2116		;MemL -Video port address [VMADDL/VMADDH]                            
		lda z_h
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
FillAreaWithTiles_Yagain:
	jsr GetVDPScreenPos
	phx
FillAreaWithTiles_Xagain:
		jsr WaitVblank
		lda z_d
		sta $2119	;VMDATAH - Write first byte to VRAM
		
		lda z_e		;ttttttttt
		sta $2118	;VMDATAL - were set to Autoinc address
					;	on 2118 write
		jsr IncDE
		dex 
		bne FillAreaWithTiles_Xagain
		inc z_c
	plx
	dey
	bne FillAreaWithTiles_Yagain	
	rts
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
	
prepareVram:			
	jsr WaitVblank
	lda z_e
	sta $2116		;VMADDL - Destination address in VRAM L
	lda z_d
	sta $2117		;VMADDH - Destination address in VRAM H
	rts
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
WaitVblank:
		lda $4212 			;HVBJOY - Status 	
			; xy00000a		- x=vblank state y=hblank state a=joypad ready
		and #%10000000
		beq WaitVblank		;Wait until we get nonzero - this means we're in VBLANK
	rts	
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
CustomNmihandler:			;Copy 1/4 of Patterns Via DMA each NMI
	php
	pha
;Set DMA Settings
		lda #128			;Inc address on write to $2119
		sta $2115			
		lda #%00000001		;Write mode 001=two bytes alternate
		sta $4300	
		lda #$18			;Destination $21xx= 2118
		sta $4301		
;Set DMA Dest (VRAM $1000)
		stz $2116			;MemL -Video port address [VMADDL/VMADDH]                            
		lda Bufferbank
		clc
		adc #$10	
		sta $2117			;MemH
;Set DMA Source ($7E2000+)
		stz $4302			;Low Byte
		lda Bufferbank
		rol					;2 bytes per Vram Address
		clc
		adc #$20
		sta $4303			;Mid Byte
		lda #$7E			
		sta $4304			;Upper Byte
;Bytecount
		lda #<($1000)
		sta $4305			;No of bytes (24 bit - Little endian
		lda #>($1000)			;(only 1st 16 bits used?)
		sta $4306
		lda #0
		sta $4307
;Do the transfer
		lda #0
		sta $420C			;Disable H-DMA transfer 
		lda #%00000001		
		sta $420B			;enable DMA 0 (bit0=1)
;Update pointer for next NMI
		lda Bufferbank
		clc 
		adc #$08			;Move to next quarter
		and #$18
		sta Bufferbank
;Dma's finished, set things bck to normal
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

	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


	include "\SrcAll\MultiPlatform_MultDiv.asm"
	include "\SrcAll\BasicFunctions.asm"
	
	include "PH_Title.asm"
	include "PH_RamDefs.asm"
	include "PH_DataDefs.asm"
	include "PH_Multiplatform.asm"
	include "\ResAll\Vector\VectorFont.asm"
	include "\srcALL\MultiPlatform_ShowDecimal.asm"
	include "PH_Vector.asm"
	
	
	include "\SrcAll\monitor.asm"			;Debugging tools	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	

;	Footer
	
	org $FFC0
     ; "123456789012345678901"
	db "Y-Quest: LearnAsm.net"	; Program title (21 byte Ascii string)

	db $20		;Rom mode/speed (bits 7-4 = speed, bits 3-0 = map mode)
	db $00		;Rom type (bits 7-4 = co-processor, bits 3-0 = type)
	db $00 		;Rom size in banks (1bank=32k)
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
	
	
	