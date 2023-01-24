;Color Ram  11011000 - 11011011
;Color Regs 00000100 - 00000111

;BG $D020
;Border $D021
	ifdef C64_ScrBase4000
ScrBase equ $4000
ScrBaseB equ $40
	endif
	ifdef C64_ScrBase8000
ScrBase equ $8000
ScrBaseB equ $80
	endif
	
	ifndef ScrBase
ScrBase equ $0	
ScrBaseB equ $0	
	endif
ScreenInit:
		;	  LXMSHVVV - L=Cur Line X=extended BG M=mode 
				;(Txt/Bmp) S=screen on H=height V=Vert scroll
		lda #%00111011	;turn on graphics mode
        sta $D011

		ifdef C64_ScrBase4000
			lda $DD00
			and #%11111100
			ora #%00000010	;Screen base at %4000 range
			sta $DD00
		endif
		
		ifdef C64_ScrBase8000
			lda $DD00
			and #%11111100
			ora #%00000001	;Screen base at %8000 range
			sta $DD00
				 
			lda #%00110110	;Kernal on ($C000+), Basic Rom off
			sta $01			;Turn off rom
		endif
		
		;     ---MWHHH - M=Multicolor W=scr width H=horiz scroll
	ifndef Mode2Color
        lda #%11011000  ;1=Multicolor 4 color ;0=standard 2 color        
	else
		lda #%11001000  ;1=Multicolor 4 color ;0=standard 2 color        
	endif
        sta $D016

		;     SSSSTTT- - T=Text/Bmp screen address S=Screen (color) address
		lda #%00011000  ;T=1 Screen at $2000 
								;(Other bits have no function in bitmap mode)
        sta $D018		
		
		;	  ----CCCC
		lda #%00000110
		sta $D021		;Background color (only bits #0-#3).	
	rts
	

GetScreenPos:
	;(X * 8) + (Top5BitsOfY * 40) + (Bottom3BitsOfY) + $2000
	pushpair z_bc
		lda #0
		sta z_d
		txa				;Multiple X by 8
		clc				;-------- XXXXXXXX
		rol
		rol z_d
		rol
		rol z_d
		rol
		rol z_d			;-----XXX XXXXX---
		ifdef FourColor
			rol
			rol z_d
		endif
		sta z_e
	
		;40 bytes per line = 00000000 00101000
		lda #0
		sta z_b
		tya
		and #%11111000	;00000000 YYYYYyyy
		clc				
		rol				
		rol z_b
		rol
		rol z_b
		rol				;00000000 00101000
		rol z_b			;00000YYY YYyyy000
		tax 
			adc z_e
			sta z_e
			lda z_b
			
			adc z_d
			sta z_d
		txa 
		rol
		rol z_b
		rol				;00000000 00101000
		rol z_b			;000YYYYY yyy00000
		adc z_e
		sta z_e
		
		lda z_b
		adc z_d
		sta z_d
	
		lda #$20+ScrBaseB		;Screen Offset
		sta z_b
		
		tya				;Add bottom 3 bits of Y
		and #%00000111
		ifdef ScrWid256
			clc
			adc #$04*8	;Offset for 256x192 screen
		endif
		sta z_c
		jsr AddDE_BC
	pullpair z_bc
	rts
	
	
	
	
	
	
	
	
	
	
GetNextLine:
	jmp incde				;within 8x8 boundary we just do an INC to move down
	
GetColMemPos:
	lda #0
	sta z_d
	sta z_h
	txa
	sta z_e

	tya
	and #%11111000
	
	tax 
		adc z_e
		sta z_e
		lda z_h
		
		adc z_d
		sta z_d
	
	txa ;lda z_c
	rol
	rol z_h
	rol
	rol z_h
	
	ifdef ScrWid256
		adc #$04
	endif
	
		adc z_e
		sta z_e
		lda z_h
		adc #$04+ScrBaseB	;Screen Offset $0400
		adc z_d
		sta z_d
	rts
