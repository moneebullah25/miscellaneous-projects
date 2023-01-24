	include "\SrcAll\BasicMacros.asm"
	
	
z_Regs 		equ $20

SPpage equ $2100

UserRam equ $2200		;Game Vars

ScreenWidth32 equ 1
ScreenWidth equ 256
ScreenHeight equ 224

Color4 equ 4			;16 Color defs
Color3 equ 3
Color2 equ 2
Color1 equ 1

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
	
	
	st0 #9			
		; 0BBB0000
	st1 #%00000000		;BACKGROUND Tilemap size (32x32)
	st2 #0
	
	
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
	
	st0 #5				;RegSelect 5
		 ;BSXXIIII	Backgroundon Spriteon eXtendedsync Interruptenable
	st1 #%10000000		;Background ON, Sprites On
	st2 #0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
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
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		

;Fill the tilemap with concecutive tiles	
	lda #0				;Start SX
	sta z_b
	lda #0				;Start SY
	sta z_c
	
	ldx #32				;Width in tiles
	ldy #32			;Height in tiles
	
	loadpair z_de,256 	;TileStart 256+ Vram:$1000+

	jsr FillAreaWithTiles ;Draw the tiles to screen
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	;start=ix,iy... dest hl,de=yoffset 
	loadpair z_hl,userram
	loadpair z_bc,256
	jsr CLDIR0
		
	jsr MainMenu
	
infloop:
	lda tick				;main loop
	clc
	adc #1
	and #%00000001
	sta tick

	loadpair z_bc,500		;slow down delay

	lda boost
	bne boostoff
	loadpair z_bc,100		;boost - no delay 
								;(compensate for font draw)

boostoff:
	lda #255
	sta z_d					;key buffer
pausebc:
	pushpair z_bc
		pushpair z_de

			jsr Player_ReadControlsDual	
		pullpair z_de

		lda z_h
		cmp #255			;Key Pressed?
		beq pausenokey

		sta z_d
		jmp keysdown		;store any pressed joystick buttons
pausenokey:
		lda #0
		sta keytimeout		;released - nuke key, and relese keypress
	
		lda #255
		sta z_d
keysdown:
	pullpair z_bc
	jsr decbc
	lda z_b
	ora z_c
	bne pausebc				;Repeat loop

startdraw:
	lda keytimeout
	bne joyskip

	lda #1
	sta boost
	
processkeys:
	loadpair z_ix,playerxacc ;point ix to player accelerations 

	lda z_d
	and #%00001000			;RSBALDRU	- L
	bne joynotleft
	
	dec playerdirection
	jsr setplayerdirection

	lda #1
	sta keytimeout

joynotleft:
	lda z_d
	and #%00000010			;RSBALDRU - R
	bne joynotright
	
	inc playerdirection
	jsr setplayerdirection
	
	lda #1					;ignore keypresses		
	sta keytimeout
	
joynotright:
	lda z_d
	and #%00010000			;RSBALDRU	-;	Fire
	bne joynotfire

	lda boostpower			;check if boost power remains
	beq joynotfire

	lda #0
	sta boost
joynotfire:
joyskip:
	jsr handleplayer		;draw and update player
	jsr handlecpu			;draw and update cpu
	jmp infloop


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		
	
FillAreaWithTiles:	; z_b = SX... z_c = SY... X=Width...
					; Y= Height... A=start tile
	
FillAreaWithTiles_Yagain:
	phx
		jsr GetVDPScreenPos	;Recalculate memory position
		st0 #2				;Set Write Register
		
FillAreaWithTiles_Xagain:	;Save the TileNum to Vram
		lda z_e
		sta $0102			;L Byte
		lda z_d
		sta $0103			;H Byte
		
		jsr IncDE
		
		dex 
		bne FillAreaWithTiles_Xagain
		
		inc z_c				;Inc Ypos
	plx
	dey						;Decrease Y count
	bne FillAreaWithTiles_Yagain
	rts
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		
	
pset:			;x=ab y=c color=d
	pushpair z_bc
		loadpair z_hl,PixelMask
		lda z_b
		and #%00000111
		tay
		lda (z_hl),y
		pha
			jsr CalcVramAddr	;Calc memory address into HL
			
			st0 #1			;Select Vram Read
			
			lda z_l
			sta $0102		;Send to Data-L
			lda z_h
			sta $0103		;Send to Data-H
			st0 #2				;Select Memory Read Reg
			
			ldx $0102		;Bitplane 0
			ldy $0103		;Bitplane 1

			st0 #0			;Select Vram Write		
			lda z_l
			sta $0102		;Send to Data-L
			lda z_h
			sta $0103		;Send to Data-H
			st0 #2			;Select Memory Write Reg
		pla
		sta z_b
		eor #255
		sta z_c
		
		txa	
		and z_c				;Bitplane 0
		ror z_d				;Get a color bit
		bcc PsetSkip1
		ora z_b
PsetSkip1:
		sta $0102			;Send to Data-L
		
		tya
		and z_c				;Bitplane 1
		ror z_d				;Get a color bit
		bcc PsetSkip2
		ora z_b
PsetSkip2:
		sta $0103			;Send to Data-H
		
		jsr incHL8			;Add 8 to HL
		
		st0 #1			;Select Vram Write		
		lda z_l
		sta $0102		;Send to Data-L
		lda z_h
		sta $0103		;Send to Data-H
		st0 #2				;Select Memory Write Reg
		
		ldx $0102		;Bitplane 2
		ldy $0103		;Bitplane 3
					
		st0 #0			;Select Vram Write		
		lda z_l
		sta $0102		;Send to Data-L
		lda z_h
		sta $0103		;Send to Data-H
		st0 #2				;Select Memory Write Reg	
		
		txa
		and z_c			;Bitplane 2
		ror z_d			;Get a color bit
		bcc PsetSkip3
		ora z_b
PsetSkip3:
		sta $0102		;Send to Data-L
		
		tya
		and z_c			;Bitplane 3
		ror z_d			;Get a color bit
		bcc PsetSkip4
		ora z_b
PsetSkip4:
		sta $0103		;Send to Data-H	
	pullpair z_bc
	rts
	
	align 4
PixelMask:	;Bitplane pixel lookup
	db %10000000,%01000000,%00100000,%00010000
	db %00001000,%00000100,%00000010,%00000001
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
Point:			;x=ab y=c color=d
	stz z_d
	pushpair z_bc
		loadpair z_hl,PixelMask
		lda z_b
		and #%00000111
		tay
		lda (z_hl),y
		pha
			jsr CalcVramAddr
			st0 #1			;Select Vram Read
				
			lda z_l
			sta $0102		;Send to Data-L
			lda z_h
			sta $0103		;Send to Data-H
			st0 #2			;Select Memory Read Reg
			
			ldx $0102		;Bitplane 0
			ldy $0103		;Bitplane 1
		pla
		sta z_b
		txa	
		and z_b
		beq PointSkip1		;Bitplane 0
		sec
PointSkip1:
		ror z_d
		tya
		and z_b
		beq PointSkip2		;Bitplane 1
		sec
PointSkip2:
		ror z_d
		jsr incHL8		;Add 8 to HL
	
		st0 #1			;Select Vram Write
		lda z_l
		sta $0102		;Send to Data-L
		lda z_h
		sta $0103		;Send to Data-H
		st0 #2			;Select Memory Write Reg
	
		lda $0102		;Bitplane 2
		ldy $0103		;Bitplane 3
		
		and z_b			;Bitplane 2
		beq PointSkip3
		sec
PointSkip3:
		ror z_d
		tya
		and z_b			;Bitplane 3
		beq PointSkip4
		sec
PointSkip4:
		ror z_d
	pullpair z_bc
	lda z_d
	ror
	ror
	ror
	ror
	rts
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Select Vram $1000=Tile 256+ pattern
CalcVramAddr:
	pushpair z_de
		loadpair z_hl,$1000	;Addresses in words
		loadpair z_de,$0000
		
		lda z_b
		and #%11111000		;*16
		asl
		rol z_d
		sta z_e
		jsr addhl_de
		
		lda #0
		sta z_d
		
		lda z_c
		and #%00000111 	;Ypos *1 (Points to bitplane 0,1)
		sta z_e			;    (bitplane 2,3 are at Vram+8)
		
		jsr addhl_de
		lda #0
		sta z_d
		sta z_e
		
		lda z_c
		and #%11111000	;*512	(8*2*32)
		lsr
		ror z_e
		lsr
		ror z_e
		sta z_d			
		jsr addhl_de
	pullpair z_de
	rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
incHL8:			;Skip 8 lines
	lda #8
	clc
	adc z_l
	sta z_l
	lda z_h
	adc #0
	sta z_h
	rts
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
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
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
		
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Clear Vram $1000=Tile 256+ pattern
cls:			
	st0 #0			;Select Vram Write		
	
	lda #0
	sta $0102		;Send to Data-L
	lda #10
	sta $0103		;Send to Data-H		
	st0 #2			;Data Port
	
	ldy #$40		;Clear $4000 bytes
	ldx #0
ClsZero:	
	stz $0102
	stz $0103
	dex
	bne ClsZero
	dey
	bne ClsZero
	rts	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
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
	

palette:
	dw %0000000000000000; ;0  %-------GGGRRRBBB
    dw %0000000111000111; ;1  %-------GGGRRRBBB
    dw %0000000000111111; ;2  %-------GGGRRRBBB
    dw %0000000111000000; ;3  %-------GGGRRRBBB
    dw %0000000111111000; ;4  %-------GGGRRRBBB
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
	
	
	
	org $fffe
	dw ProgramStart			;Reset Vector 
	