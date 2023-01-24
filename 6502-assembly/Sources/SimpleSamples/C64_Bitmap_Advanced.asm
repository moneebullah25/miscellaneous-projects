;FourColor equ 1

	 
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


;Init Routine
*=$0801
	db $0E,$08,$0A,$00,$9E,$20,$28,$32,$30,$36,$34,$29,$00,$00,$00  
*=$0810	;Start at $0810

	;	  LXMSHVVV - L=Cur Line X=extended BG M=mode 
				;(Txt/Bmp) S=screen on H=height V=Vert scroll
		lda #%00111011	;turn on graphics mode
        sta $D011

		
		;     ---MWHHH - M=Multicolor W=scr width H=horiz scroll
		ifdef FourColor
			lda #%11011000  ;1=Multicolor 4 coor 
		else
			lda #%11001000  ;0=standard 2 color 
		endif
	    sta $D016

		;     SSSSTTT- - T=Text/Bmp screen address S=Screen (color) address
		lda #%00011000  ;T=1 Screen at $2000 
								;(Other bits have no function in bitmap mode)
        sta $D018		
		
		;	  ----CCCC
		lda #%00000000
		sta $D021		;Background color (only bits #0-#3).	
		sta $D020		;Border 

		

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
;Draw Bitmap Data
	lda #<Bitmap					;Source Bitmap Data
	sta z_L
	lda #>Bitmap
	sta z_H
	
bmpwidth equ 6
BmpHeight equ 6
xpos equ 4
ypos equ 8	
	
	ldx #xpos
	ldy #ypos
	jsr GetScreenPos	;Get screen pos from XY into Z_DE

	ldx #bmpwidth
NexBitmapNextStrip:
	lda z_d
	pha
	lda z_e
	pha
BitmapNextLine:
		ldY #0			;Offset for bytes in this strip
BitmapNextByte:
		lda (z_hl),Y	;Load in a byte from source - offset with Y
		sta (z_de),Y	;Store it in screen ram - offset with Y
		
		inY				;INC the offset
		cpY #bmpwidth *8 		;We draw 8 lines * bitmap width (12 bytes)
		bne BitmapNextByte
		clc
		tya				;Add Y to HL
		adc z_l
		sta z_l
		lda z_h
		adc #0
		sta z_h				
	pla ;z_e
	clc
	adc #$40	;Move down one strip (+$0140)
	sta z_e
	pla ;z_d
	adc #$01
	sta z_d	
	dex			;NO of Y-strips in Bitmap 8 rows per strip
	bne NexBitmapNextStrip

;Fill Color Data
	ldx #xpos
	ldy #ypos
	jsr GetColMemPos	
	
	ldx #BmpHeight
FillColMemAgainY:	
	lda z_d
	pha
	lda z_e
	pha
		ldy #0
FillColMemAgainX:
		ifdef FourColor
			lda #$43				;Color
			sta (z_de),Y			;%22221111 Color 1,2
			lda #01
			sta (z_bc),Y			;%----3333 Color 3 (White)
		else
			lda #$40				;Color
			sta (z_de),Y			;%11110000 Color 1,2
		endif		
		iny 
		cpy #bmpwidth
		bne FillColMemAgainX
	pla ;z_e
	clc
	adc #$28	;Move down one strip (+40)
	sta z_e
	sta z_c
	pla ;z_d
	adc #0		;Color offset $0400
	sta z_d	
	adc #$D8-4	;Color Offset $D800
	sta z_b
	dex			;NO of Y-strips in Bitmap 8 rows per strip
	bne FillColMemAgainY
	
	jmp *
	
	
	
	
;Address= (X * 8) + (Top5BitsOfY * 40) + $2000
GetScreenPos:
	lda #0
	sta z_b
	sta z_d
	txa				;Multiple X by 8
	asl				;-------- XXXXXXXX
	rol z_d
	asl
	rol z_d
	asl
	rol z_d			;-----XXX XXXXX---
	sta z_e

;40 bytes per Yline =00000000 00101000
	tya
	and #%11111000	;00000000 YYYYYyyy
	asl
	rol z_b
	asl
	rol z_b
	asl				;00000000 00101000
	rol z_b			;00000YYY YYyyy000
	tax 
		adc z_e		;Add part to total L
		sta z_e
		lda z_b		;Add part to total H
		adc z_d
		sta z_d
	txa 
	asl
	rol z_b
	asl				;00000000 00101000
	rol z_b			;000YYYYY yyy00000
	
	adc z_e			;Add part to total L
	sta z_e
	lda z_b			;Add part to total H
	adc z_d
	adc #$20+0		;Screen Base $2000
	sta z_d
	rts
	
	
;Color Ram data at $D800 & $400
;Address = $0400+ Y Strip * 40 + Xpos

GetColMemPos:
	lda #0
	sta z_d
	txa
	sta z_e			;Xpos
;40 bytes per Yline =00000000 00101000
	tya				;Need to multiply by 40 (%00101000)
	and #%11111000	;One color per 8x8 square
	tay 
		clc
		adc z_e		;Add Ypos part to Xpos 
		sta z_e		;Save %00-01000 part
	tya 
	asl
	rol z_d	
	asl				;00000000 00101000
	rol z_d			;000YYYYY yyy00000
	clc
	adc z_e			;Add Ypos part to total
	sta z_e
	sta z_c
	
	lda z_d
	adc #$04+0		;Color Offset $0400
	sta z_d
	
	adc #$D8-4		;Color Offset $D800
	sta z_b
	rts				;z_DE = $0400-07FF byte  
				;z_BC = $D800-DBFF byte (for 4 color)

		
		
Bitmap:
	ifdef FourColor
		incbin "\ResALL\Sprites\RawC64-4col.RAW"
	else
		incbin "\ResALL\Sprites\RawC64-2col.RAW"
	endif	
BitmapEnd:

