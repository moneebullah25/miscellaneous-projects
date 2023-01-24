
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	Draw Line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	
	
;start=ix,iy... dest hl,de=yoffset 
drawline:
	lda z_ixh					;source x
	sta z_b
	sta xpos24+1+1

	
	lda z_ixl
	sta z_c
	sta xpos24+1

	jsr SubHL_BC		;calculate difference x
	
	
	lda z_iyh					;dest y
	sta z_b
	sta ypos24+1+1
	
	lda z_iyl
	sta z_c
	sta ypos24+1
	
	jsr Subde_BC				;calculate difference y
		

drawlinerelative:				;hl=xoffset de=yoffset 

	lda z_h
	ora z_l
	ora z_d
	ora z_e
	bne drawlinerelativeok

	inc z_e								;draw at least 1 dot
	inc z_l

drawlinerelativeok:

	loadpair z_ix,xposdir
	ldx #0
	txa
	sta (z_ix,x)

	lda z_h
	and #%10000000
	
	beq NoFlipHL	;xpos is negative

;Flip HL
	ldx #0
	lda #1
	sta (z_ix,x)						;update flip marker
	
	dex
	txa				;X=255
	
	eor z_h								;$0001 -> $ffff
	sta z_h
	txa
	eor z_l
	sta z_l
	jsr inchl
	
	inx			;x=0
	
NoFlipHL:
	loadpair z_ix,yposdir
	txa
	sta (z_ix,x)

	lda z_d
	and #%10000000
	beq noflipde
	
;Flip DE
	ldx #0
	lda #1
	sta (z_ix,x)						;update flip marker
	
	dex
	txa				;X=255
	
	eor z_d								;$0001 -> $ffff
	sta z_d
	txa
	eor z_e
	sta z_e
	jsr incde
	
	inx			;x=0
noflipde:

	jsr SubHL_DE

	bcs hlbigger	;x length > ylength
	jsr addhl_de
	pushpair z_de
	
		lda z_l
		sta z_h						;hl=l/e
		lda z_e


		jsr div8zerol				;hl=source	a=divider (hl=hl/a a=rem)
		
	pullpair z_bc

	ldx #1	
	stx z_d			;loadpair z_de,$0100
	dex
	stx z_e			;1 vpixel per draw
	
	jmp debigger

hlbigger:

	jsr addhl_de				;fix hl

	pushpair z_hl
		lda z_e
		sta z_h		;hl=e/l
		lda z_l

		jsr div8zerol	;hl=source	a=divider (hl=hl/a a=rem)
		jsr ExDe_Hl
	pullpair z_bc

	ldx #1
	stx z_h			;loadpair z_hl,$0100
	dex
	stx z_l			;1 hpixel per draw
	
debigger:

	inc z_b 		;make sure outer B loop works!
	
	
	lda z_d								;ymove
	sta z_iyh
	lda z_e
	sta z_iyl

	lda z_h								;xmove
	sta z_ixh
	lda z_l
	sta z_ixl

lineagain:

	pushpair z_bc
		lda ypos24+1
		sta z_c
	
		lda linecolor
		sta z_d
	
		lda xpos24+1
		sta z_b
		
		lda xpos24+1+1
	
		jsr pset	;x=ab   y=c   color=d
		
		lda z_ixh			;Xmove
		sta z_d
		lda z_ixl
		sta z_e
		
		loadpair z_hl,xposdir
		ldy #0
		jsr add24
		
		lda z_iyh			;Ymove
		sta z_d
		lda z_iyl
		sta z_e
		
		iny
		jsr add24
;                                      	pop bc
	pullpair z_bc
	dec z_c
	bne lineagain
	dec z_b
	bne lineagain
	rts

	
	
;                    add24:	;add de to (hl) 24 bit uhl little endian (dlhu)

;ldy #0	-Must Reset Y!

add24:
	lda (z_hl),y			;first byte is direction marker (d)
	bne sub24

	iny
	
	lda (z_hl),y		;(hl)=(hl)+$00 $dd $ee

	clc
	adc z_e

	sta (z_hl),y		;L
	iny
	
	lda (z_hl),y

	adc z_d
	sta (z_hl),y	;H
	bcc add24Done
	iny
	
	lda (z_hl),y
	clc 
	adc #1
	sta (z_hl),y		;U
	rts
add24Done
	iny
	rts
	
	

sub24:
	iny		
	lda (z_hl),y		;(hl)=(hl)-$00 $dd $ee

	sec
	sbc z_e
	sta (z_hl),y		;L
	iny
	
	lda (z_hl),y
	sbc z_d
	sta (z_hl),y		;H
	bcs sub24Done
	iny
	
	lda (z_hl),y
	sec
	sbc #1
	sta (z_hl),y
	rts
sub24Done:
	iny
	rts
  	


	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	Font Driver
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;                                      printcharnum:
printcharnum:
;                                      		sub '!'	
	sec
	sbc #'!'
;                                      			ld hl,vnumset
	loadpairsafe z_hl,vnumset
;                                      			jr printcharactersb
	jmp printcharactersb
;                                      printspace:
printspace:
;                                      	ld a,' '
	lda #' '
printchar:
	sta z_as
	pha
	tya 
	pha
	pushpair z_bc
	pushpair z_de
	pushpair z_hl
		lda z_as
		cmp #' '
;                                      		jr z,printcharspace		;space
		beq printcharspace
;                                      		cp ':'+1
		cmp #':'+1
;                                      		jr c,printcharnum		;numbers and symbols
		bcc printcharnum
;                                      
		
;                                      		and %11011111			;convert upper-> lower
		and #%11011111
		sec
		sbc #'A'
		
		loadpairsafe z_hl,vcharset
printcharactersb:
		
		asl					;2 bytes per address
		tay
		
		
		lda (z_hl),y			;get address of cpacket for char
	
		sta z_ixl
		iny
		lda (z_hl),y
		sta z_ixh
		jsr drawcpacket			;Draw it
	
printcharspaceb:
		;loadpairfrom z_hl,xpos24+1
		loadpair z_bc,12
		jsr doscale
		
		
		clc
		lda z_c
		adc xpos24+1
		sta xpos24+1
		lda z_b
		adc xpos24+2
		sta xpos24+2
		
		;jsr AddHl_Bc
		;savepairto z_hl,xpos24+1
	pullpair z_hl
	pullpair z_de
	pullpair z_bc

	pla
	tay
	pla

	rts
;                                      printcharspace:				;print a space
printcharspace:
;                                      	ld hl,(xpos24+1)
	loadpairfromsafe z_hl,xpos24+1
;                                      	jr printcharspaceb		;move right 1 char
	jmp printcharspaceb
;                                      

;                                      		

;(hl,de) = (x,y)
locate:

	savepairto z_hl,xpos24+1		;postition drawing cursor
	savepairto z_de,ypos24+1
	
	;set low byte of xy to zero
	jmp zerolow24byte


NewLine:
	lda #0
	sta xpos24+1
	sta xpos24+2
	
	loadpair z_bc,16
	jsr doscale
	
	
		clc
		lda z_c
		adc ypos24+1
		sta ypos24+1
		lda z_b
		adc ypos24+2
		sta ypos24+2
		
	;loadpairfrom z_hl,ypos24+1
	;jsr AddHl_Bc
		
	;savepairto z_hl,ypos24+1
	rts
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	Draw Vectrex Packet format
;	$CC,$YY,$XX ... CC 00=Move FF=Line 01=Done
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	
	
	
	
	
	
	
	
	
	

drawpacket:
		
	lda xpos24+1
	pha
	lda xpos24+2
	pha
	

		lda ypos24+1
		pha
		lda ypos24+2
		pha		
		
		
		
;                                      vtxagain:
vtxagain:
;                                      			ld a,(hl)		;check command
	ldx #0
	lda (z_hl,x)
			
;                                      			inc hl
	pha
		jsr inchl
	pla
;                                      			or a			;$00 = move 
	;nop
;                                      			jr z,vtxmove
	
	beq vtxmove		;0
	
;                                      			inc a			;$ff = draw line
	cmp #255
;                                      			jr z,vtxline
	beq vtxline		;255
;                                      			cp 2			;$01 = line done
	cmp #1
;                                      			jr z,vtxdone	;update pos $ return
	bne vtxline
		jmp	vtxdone
;                                      vtxline:

vtxline:
;                                      			ld a,(hl)		;get ypos
	ldx #0
	lda (z_hl,x)
;                                      			neg
	jsr neg
;                                      			ld c,a
	sta z_c
;                                      			inc hl	
	jsr inchl
;                                      			call sexbc
	jsr sexbc
;                                      			ld d,b
	
	lda z_b
	sta z_d
	
;                                      			ld e,c
	
	lda z_c
	sta z_e
	
;                                      			ld c,(hl)		;get xpos
	ldx #0
	
	lda (z_hl,x)
	sta z_c
	
;                                      			inc hl	
	jsr inchl
;                                      			call sexbc
	jsr sexbc
;                                      			push hl
	pushpair z_hl
;                                      				ld h,b
		lda z_b
		sta z_h
;                                      				ld l,c
		lda z_c
		sta z_l

;                                      				call drawlinerelative	;hl=xoffset de=yoffset 
		jsr drawlinerelative
;                                      			pop hl
	pullpair z_hl
;                                      			jr vtxagain
	jmp vtxagain
;                                      vtxmove:
vtxmove:
	
;                                      			ld a,(hl)			;ypos
	ldx #0
	lda (z_hl,x)
;                                      			neg
	jsr neg
;                                      			ld c,a 
	sta z_c
;                                      			inc hl	
	jsr inchl
;                                      			call sexbc
	jsr sexbc
;                                      			ex de,hl
;	jsr ExDe_Hl
;                                      				ld hl,(ypos24+1) ;move ypos
		;loadpairfrom z_hl,ypos24+1
;                                      				add hl,bc
		;jsr AddHL_BC
;                                      				ld (ypos24+1),hl
		;SavePairTo z_hl,ypos24+1
		
		clc
		lda z_c
		adc ypos24+1
		sta ypos24+1
		lda z_b
		adc ypos24+2
		sta ypos24+2
		

;                                      			ex de,hl
	;jsr ExDe_Hl
;                                      			ld c,(hl)			;xpos
	
	ldx #0
	lda (z_hl,x)
	sta z_c
	
;                                      			inc hl	
	jsr inchl
;                                      			call sexbc
	jsr sexbc
;                                      			ex de,hl
	;jsr ExDe_Hl
;                                      				ld hl,(xpos24+1) ;move xpos
		;loadpairfrom z_hl,xpos24+1
;                                      				add hl,bc
		;jsr AddHL_BC
;                                      				ld (xpos24+1),hl
		;SavePairTo z_hl,xpos24+1
;                                      			ex de,hl
	;jsr ExDe_Hl
	
		clc
		lda z_c
		adc xpos24+1
		sta xpos24+1
		lda z_b
		adc xpos24+2
		sta xpos24+2
;                                      			jr vtxagain
	jmp vtxagain
;                                      


IncIX:
		INC z_IXL
		BNE	IncIX_Done
		INC	z_IXH
IncIX_Done:
		rts

		

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	Draw CPacket format - (7 bit per axis - 2 command bits)
;	%CYYYYYYY,%DXXXXXXX ... %CD %?0=Move %?1=Line %1?=End
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;                                      drawcpacket:		;draw cpacket ix
drawcpacket:
	lda xpos24+1
	pha
	lda xpos24+2
	pha
	

		lda ypos24+1
		pha
		lda ypos24+2
		pha
		
;                                      			jr drawcpacketstart
		jmp drawcpacketstart
;                                      drawcpacketagain:
drawcpacketagain:
;                                      			inc ix				;move to next cmd
			jsr IncIX
			jsr IncIX
;                                      			

;                                      drawcpacketstart:				;cmd check
drawcpacketstart:
;                                      			bit 7,(ix+1)		;%cd=%?0 = move type
			ldy #1
			lda (z_ix),y
			and #%10000000
;                                      			jr z,cpacketmove
	beq cpacketmove
;                                      cpacketline:					;%cd=%?1 = line type
cpacketline:
;                                      			ld a,(ix+0)			
			ldy #0
			lda (z_ix),y
;                                      			neg
	jsr neg
;                                      			ld c,a				;ypos
	sta z_c
;                                      			call sexbc7bit
	jsr sexbc7bit
;                                      			ld d,b
	
	lda z_b
	sta z_d
	
;                                      			ld e,c
	
	lda z_c
	sta z_e
	
;                                      			ld c,(ix+1)			;xpos
	ldy #1
	lda (z_ix),y
	sta z_c
;                                      			call sexbc7bit
	jsr sexbc7bit
;                                      			push ix
	pushpair z_ix
;                                      				ld h,b
		lda z_b
		sta z_h
;                                      				ld l,c
		lda z_c
		sta z_l
	
		jsr drawlinerelative			;hl=xoffset de=yoffset 
;                                      			pop ix
	pullpair z_ix
;                                      			bit 7,(ix)			;%cd=%1? = drawing done
	ldy #0
	lda (z_ix),y
	and #%10000000
;                                     			jr z,drawcpacketagain
	beq drawcpacketagain
;                                      vtxdone:
vtxdone:
	
	pla
	sta ypos24+2
	pla
	sta ypos24+1					;restore ypos

	pla
	sta xpos24+2					;restore xpos
	pla
	sta xpos24+1
	

	
;                                      zerolow24byte:
zerolow24byte:
;                                      	xor a
	lda #0

	sta xpos24							;Zero bottom byte of 24 bits
	sta ypos24
	rts

cpacketmove:

	ldy #0								;get ypos
	lda (z_ix),y
	jsr neg
	sta z_c
	jsr sexbc7bit
	;loadpairfrom z_hl,ypos24+1

	;jsr AddHL_BC						;move ypos
	;SavePairTo z_hl,ypos24+1
	
	
		clc
		lda z_c
		adc ypos24+1
		sta ypos24+1
		lda z_b
		adc ypos24+2
		sta ypos24+2
	

	iny				;Y=1
	lda (z_ix),y						;get xpos
	sta z_c
;                                      	call sexbc7bit
	jsr sexbc7bit
	
	;loadpairfrom z_hl,xpos24+1

	;jsr AddHL_BC						;move xpos
	;SavePairTo z_hl,xpos24+1
	
	
		clc
		lda z_c
		adc xpos24+1
		sta xpos24+1
		lda z_b
		adc xpos24+2
		sta xpos24+2

	jmp drawcpacketagain				;back to start of cpacket processor


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; scaler



	;sign extend 7bit c-> bc
sexbc7bit:
	lda z_c
	and #%01111111
	sta z_c
	
	bit LookupBits+6
	beq sexbc
	ora #%10000000
	sta z_c

sexbc:			

	lda #255
	sta z_b
	
	lda z_c
	and #%10000000

	bne doscale

	inc z_b			;255+1 = b=0


doscale:			;scale bc 
	ldx scale

doscaleagain:
	bne lbl5003			;0= full size
	rts
lbl5003
	cpx #128
	bcs scalenegative

scalepositive:			;>0 x2 x4 x8
	asl z_c
	rol z_b
	dex
	jmp doscaleagain

scalenegative:			;<0 /2 /4 /8
	clc
	lda z_b
	BPL scalenegativeP
	SEC		;Top bit 1
scalenegativeP:
	ror z_b
	ror z_c
		
	inx
	jmp doscaleagain
;                                      

