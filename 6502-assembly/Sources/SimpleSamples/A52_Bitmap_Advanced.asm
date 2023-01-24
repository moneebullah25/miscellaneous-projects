FourColor equ 1


	 
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


	ifdef BuildA80		;Atari 800 settings
GTIA equ $D000			;GTIA address
	org     $A000       ;Start of cartridge area
	
	else				;Atari 5200 settings
GTIA equ $C000			;GTIA address
	org     $4000       ;Start of cartridge area	
	endif
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	INIT

ProgramStart:        
	sei             ;Disable interrupts

	;Zero GTIA area and Zero Page
    ldx #$00
    txa
ClearLoop    
    sta $00,x   	;Clear zero page
    sta GTIA,x      ;Clear GTIA
    dex
    bne ClearLoop
		
	
	
	lda #<DisplayList
	sta $D402 		;DLISTL - Display list lo
	lda #>DisplayList
	sta $D403 		;DLISTH - Display list hi
	
	
	lda #%00100010   	
	sta $D400 		;DMACTL - DMA Control (screen on)

	ifdef FourColor
		lda #$98      	;Set color PF1 (foreground) (CYAN)
		sta GTIA+ $17 	;COLPF1 equ 
		
		lda #$0F       	;Set color PF2 (background) (White)
		sta GTIA+ $18	;COLPF2 

		lda #$68        ;Set color PF0 (Purple)
		sta GTIA+ $16
		
		lda #$00       ;Set color PF0 (Black)
		sta GTIA+ $1A
	else
		lda #$0F
		sta GTIA+ $17 	;COLPF1 equ 		
		
		lda #$00        ;2 color mode only uses the brightness of color1
		sta  GTIA+ $16
		sta  GTIA+ $18
		sta  GTIA+ $1A	
	endif

	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	Your Code Goes Here		


	lda #<Bitmap	;Bitmap source
	sta z_L
	lda #>Bitmap
	sta z_H

	ldx #6			;Xpos
	ldy #6			;Ypos

	jsr GetScreenPos
	
	ldx #48			;Height
BitmapNextLine:
	ldY #0			;Line Byte Count
BitmapNextByte:
	lda (z_hl),Y 	;Copy a byte from the source 
	sta (z_de),Y 	;to the destination
		
	inY
	cpY #6 			;WIDTH in bytes
	bne BitmapNextByte

	clc
	tya				;Add Y to HL
	adc z_l
	sta z_l
	lda z_h
	adc #0
	sta z_h				

	lda z_e
	adc #$28	;Move down a line (40 Bytes / $0028)
	sta z_e
	lda z_d
	adc #$00			
	sta z_d		
	
	dex			;Next Y Line
	bne BitmapNextLine		
	
	jmp *
	
	;40 bytes per line = * %00000000 00101000
	;We shift our Ypos 3 to the right, add it, then another 2, eg
	
	;%00000000 00101000	=40 (32+8)
	;%YYYYYYYY 00000000
	;%000YYYYY YYY00000	= Y*32
	;%00000YYY YYYYY000 = Y*8
GetScreenPos:
	txa	
	sta z_e			;Store X pos in E
	
	tya 			;Get Ypos - store in B
	sta z_b			;%YYYYYYYY 00000000		
	
	lda #0
	
	lsr z_b			;Shift 3 Bits
	ror 			;%0YYYYYYY Y0000000
	lsr z_b
	ror 			;%00YYYYYY YY000000
	lsr z_b
	ror 			;%000YYYYY YYY00000 = Y*32
	tax	
		clc
		adc z_e		;Update Low Byte
		sta z_e
		
		lda #0 		;Update High Byte
		adc z_b
		sta z_d
	txa
	lsr z_b			;Shift 2 bits
	ror 			;%0000YYYY YYYY0000
	lsr z_b
	ror 			;%00000YYY YYYYY000 = Y*8 
		
	adc z_e			;Add to Update Low Byte
	sta z_e
	
	lda z_b			;Add to Update High Byte
	adc z_d
	sta z_d	

	clc
	lda z_e
	adc #$60
	sta z_e
	lda z_d			;Add Screen Base ($2060)
	adc #$20
	sta z_d		
	
	rts
		
Bitmap:
	ifdef FourColor	
		incbin "\ResALL\Sprites\RawA52.RAW"
	else
		incbin "\ResALL\Sprites\RawZX.RAW"		
	endif
BitmapEnd:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	Display List
  
    org $b000
	ifdef FourColor	
Smode Equ $0E	;E=4 color.... F=2 color
	else
Smode Equ $0F	;E=4 color.... F=2 color
	endif

DisplayList:				;Display list data
	db $70,$70,$70;$70 7= 8 blank lines 0= blank lines

		db $40+Smode,$60,$20	;Strange start ($2060) to safely step over the boundary
		
		db	         Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		db	   Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		db	   Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		db	   Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		db	   Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		
		db	   Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		db	   Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		db	   Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		db	   Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		db	   Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		
		db $40+Smode,$00,$30	;Have to manually step over the 4k boundary ($3000)
		db	         Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		db	   Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		db	   Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		db	   Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		db	   Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		
		db	   Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		db	   Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		db	   Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		db	   Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		db	   Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode,Smode
		
	db $41					;Loop
	dw DisplayList
		

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	Rom Header
        org $bffd
        db $FF         ;Disable Atari Logo
        dw ProgramStart;program Start
		
		
		