;                                      showdecimal:
showdecimal:
drawtext_decimal:
	pha
		lda #<$640a
		sta z_l
		lda #>$640a
		sta z_h
	pla
	sta z_b

	cmp z_h
	bcs decthreedigit
	jsr printspace
	cmp z_l
	bcs skipdigit100
	jsr printspace
	jmp skipdigit10
decthreedigit:
;                                      

;                                      	call drawtextdecimalsub
	jsr drawtextdecimalsub
;                                      skipdigit100:
skipdigit100:;                                      	ld h,l
		lda z_l
		sta z_h
	
;                                      	call drawtextdecimalsub
	jsr drawtextdecimalsub
;                                      

;                                      skipdigit10:
skipdigit10:
;                                      	ld a,b
	lda z_b
;                                      drawtext_charsprite48:
drawtext_charsprite48:;                                      	add 48
	clc
	adc #48
;                                      drawtext_charspriteprotectbc:
drawtext_charspriteprotectbc:;                                      	jp printchar; draw char
	jmp printchar
;                                      

;                                      drawtextdecimalsub:
drawtextdecimalsub:
;                                      	ld a,b
	lda #0
	sta z_c
	lda z_b
;                                      	ld c,0
	
;                                      drawtext_decimalsubagain:
drawtext_decimalsubagain:;                                      	cp h
	cmp z_h
;                                      	jr c,drawtext_decimallessthan	;devide by 100
	bcc drawtext_decimallessthan
;                                      	inc c
	inc z_c
;                                      	sub h
	sec
	sbc z_h
;                                      	jr drawtext_decimalsubagain
	jmp drawtext_decimalsubagain
;                                      drawtext_decimallessthan:
drawtext_decimallessthan:;                                      	ld b,a
	sta z_b
;                                      	ld a,c
	lda z_c
;                                      	or a		;we're going to do a compare as soon as we return
	;nop
;                                      	jr drawtext_charsprite48
	jmp drawtext_charsprite48
;                                      	

;                                      

