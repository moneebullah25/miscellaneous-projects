
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Random Number Generator (WORD)

doquickrandomword:
	pushpair z_bc
	pushpair z_de
		loadpairfrom z_bc,randomseed
		jsr incbc
		savepairto z_bc,randomseed		
		jsr dorandomword
	pullpair z_de
	pullpair z_bc
	rts


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;DoRandom Byte	
	

DoRandom:					;RND outputs to A (no input)
	pushpair z_hl
	pushpair z_bc
	pushpair z_de
		loadpairfrom z_bc,RandomSeed
		jsr incbc			;Get and update Random Seed
		savepairto z_bc,RandomSeed
		jsr DoRandomWord
		lda z_l
		eor z_h
		sta z_as
	pullpair z_de
	pullpair z_bc
	pullpair z_hl
	lda z_as
	rts
	
DoRandomWord:				;Return Random pair in HL from Seed BC
	jsr DoRandomByte1		;Get 1st byte
	sta z_h
	pushpair z_hl
	pushpair z_bc
		jsr DoRandomByte2	;Get 2nd byte
		sta z_as
	pullpair z_bc
	pullpair z_hl
	jsr incbc
	
	lda z_as
	sta z_l
	
	rts

DoRandomByte1:
	lda z_c					;Get 1st sed
DoRandomByte1b:
	rrca					;Rotate Right
	rrca
	eor z_c					;Xor 1st Seed
	rrca					;Rotate Right
	rrca				
	eor z_b					;Xor 2nd Seed
	rrca					;Rotate Right
 	eor #%10011101			;Xor Constant 
	eor z_c					;Xor 1st seed
	rts


DoRandomByte2:
	loadpair z_hl,Randoms1
		lda z_b
		eor #%10101011
		and #%00001111		;Convert 2nd seed low nibble to Lookup
		clc
		adc z_l
		sta z_l
		ldx #0
		lda (z_hl,x)	;Get Byte from LUT 1
		sta z_d
		
	jsr DoRandomByte1
	and #%00001111		;Convert random number from 1st geneerator to Lookup
	pha
		loadpair z_hl,Randoms2
	pla
	clc
	adc z_l
	sta z_l
	lda (z_hl,x)		;Get Byte from LUT2
	eor z_d				;Xor 1st lookup
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Ranged Ramdom


dorangedrandomagain:
	pullpair z_hl
	
;return hl value between bc and de (using mask ix)
dorangedrandom:
	pushpair z_bc
		pushpair z_hl
			jsr doquickrandomword	;get a 16 bit value
		pullpair z_bc
		lda z_h
		eor z_b	
		and z_ixh					;mask h byte
		sta z_h

		lda z_l
	
		and z_ixl					;mask l byte
		eor z_c
		sta z_l
	pullpair z_bc
	pushpair z_hl
		jsr subhl_bc				;check if<bc
		bcc dorangedrandomagain
		jsr addhl_bc				;Undo check sub
		
		jsr subhl_de				;check if>de
		bcs dorangedrandomagain
	pullpair z_hl
	rts


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;object on horizontal plane
doxlineobj:
	loadpair z_bc,8
	loadpair z_de,(screenwidth-8)		;full vertical range
	loadpair z_ix,$1ff					;ramdom mask
	
	jsr dorangedrandom					;return value between bc and de
	
	pushpair z_hl
		ifdef screenwidth20
			loadpair z_bc,32
			loadpair z_de,(screenheight-32)	;narrow horizontal range
		else
			loadpair z_bc,64
			
			loadpair z_de,(screenheight-64)	;narrow horizontal range
		endif
		jsr dorangedrandom					;return value between bc and de
		jsr exde_hl
	pullpair z_hl
	jsr locate

	rts

;object on vertical plane
doylineobj:
	ifdef screenwidth20
		loadpair z_bc,32
		loadpair z_de,(screenwidth-32)		;narrow vertical range
	else

		loadpair z_bc,64
		loadpair z_de,(screenwidth-64)		;narrow vertical range
	endif
		loadpair z_ix,$1ff

		jsr dorangedrandom					;return value between bc and de

	pushpair z_hl
		loadpair z_bc,8
	
		loadpair z_de,(screenheight-8)		;full horizontal range
		
		jsr dorangedrandom					;return value between bc and de
		jsr exde_hl
	pullpair z_hl
	jsr locate								;Set Draw pos
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


mainmenu:
	
;reset game settings
	lda #1
	sta level

	lda #4
	sta lives

	jsr cls

	jsr dotitlescreen	;show title
	jsr waitforfire
restart:

	jsr levelinit		;setup level
	jmp infloop


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	
dotitlescreen:							;draw title graphics

	loadpair z_hl,(screenwidth/2)		;screen center
	loadpair z_de,(screenheight/2)
	jsr locate

	ifdef screenwidth20
		lda #0
	else
		lda #1

	endif

	sta scale			;set scale

	lda #color2
	sta linecolor

	loadpair z_hl,vectitleb		;title 3d depth
	jsr drawpacket
	
	loadpair z_hl,vecball		;ball torso
	jsr drawpacket
	
	loadpair z_hl,vechands		;ball hands
	jsr drawpacket

	loadpair z_hl,veceyes		;ball eyes
	jsr drawpacket

	loadpair z_hl,vecmouth		;ball mouth

	jsr drawpacket

	lda #color3
	sta linecolor

	loadpair z_hl,vectitlef		;title main
	jsr drawpacket

	lda #color1
	sta linecolor

	loadpair z_hl,vectoung		;ball toung
	jsr drawpacket

	ifdef screenwidth20
		lda #1
	else
		lda #2
	endif
	sta scale
	
	loadpair z_ix,vectitlezoom	;speed lines of ball
	jsr drawcpacket

	lda #color3
	sta linecolor

	loadpair z_ix,vectitlewall			;wall
	jsr drawcpacket

	
;draw title text

	lda #-1
	sta scale

	lda #color4
	sta linecolor

;title split on small screen
	ifdef screenwidth20
		loadpair z_hl,24
		ifdef BuildLNX
			loadpair z_de,30
		else
			loadpair z_de,8
		endif
		
		pushpair z_de
			jsr locate
		
			loadpair z_hl,ttitle1
			jsr printstring
			loadpair z_hl,32
		pullpair z_de
		addpair z_de,8
		jsr locate

		loadpair z_hl,ttitle2
		jsr printstring

	else

		ifdef screenwidth32
			loadpair z_hl,15
		else
			loadpair z_hl,50
		endif
		loadpair z_de,55
		jsr locate
		
		loadpair z_hl,ttitle
		jsr printstring
	endif

	lda #color1
	sta linecolor

	lda #-1
	sta scale

	loadpair z_hl,30
	ifdef BuildLNX
		loadpair z_de,(screenheight-8)
	else
		loadpair z_de,(screenheight-16)
	endif
	jsr locate

	loadpair z_hl,tbestlevel
	jsr printstring
	
;'high score'
	lda bestlevel
	jsr showdecimal
	rts


	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	
levelinit:
	
;check current level
	lda level
	and #%00000111
	cmp #%00000111						;extra life every 8 levels
	bne noextralife
	inc lives
noextralife:

	ifdef screenwidth20
		lda #49*2+1
	else
		lda #99*2+1
	endif

	sta boostpower		;recharge boost power

	lda level			;calculate cpu ai
	lsr
	lsr
	sta z_b

	lda #7				;stupid=7 smart=0
	sec
	sbc z_b
	bcs cpuaiok

	lda #0
cpuaiok:
	sta cpuai

	;reset level settings 
	loadpair z_hl,userrambak
	loadpair z_de,userram
		
	loadpair z_bc,(userrambakend-userrambak)
		
	jsr ldir

;draw new level objects

	jsr cls				;clear screen
		
	lda level			;calculate no of pairs of objects
	clc
	adc #4				;4 pair min
	sta z_ixh

	lda #10				;max 10 pairs of square objects
	sta z_ixl

	ifdef screenwidth20
		lda #0
	else
		lda #1
	endif
	sta scale

	lda #color4
	sta linecolor

moreobject1:
	pushpair z_ix
		jsr doxlineobj

		loadpair z_ix,object1	;square object h

		jsr drawcpacket
		jsr doylineobj

		loadpair z_ix,object1	;square object v

		jsr drawcpacket
	pullpair z_ix

	dec z_ixl
	beq doobject2

	dec z_ixh
	bne moreobject1	;decrease obj count

doobject2:
	lda z_ixh						;remainder are hollow objects
	beq objectsdone

moreobject2:
	pushpair z_ix
		jsr doxlineobj

		loadpair z_ix,object2		;hollow object h
		jsr drawcpacket

		jsr doylineobj

		loadpair z_ix,object2		;hollow object v
		jsr drawcpacket
	pullpair z_ix
	dec z_ixh
	bne moreobject2

objectsdone:
	lda #color1
	sta linecolor
	
;draw screen borders

	loadpair z_de,(screenwidth-1)
		
hlineagain:
	loadpair z_hl,0

	lda #color1
	jsr psethlde
	
	loadpair z_hl,(screenheight-1)

	lda #color1
	jsr psethlde	;de=xpos hl=ypos a=color
	
	jsr decde

	lda z_d
	cmp #255
	bne hlineagain

	loadpair z_hl,(screenheight-1)

vlineagain:
	loadpair z_de,0
	
	lda #color1

	jsr psethlde	;de=xpos hl=ypos a=color
	
	loadpair z_de,(screenwidth-1)
		

	lda #color1

	jsr psethlde						;de=xpos hl=ypos a=color

	jsr dechl

	lda z_h
	ora z_l
	bne vlineagain

;draw text 	

	ifdef screenwidth20
		lda #-1
	else
		lda #0
	endif
	sta scale


	loadpair z_hl,(0-4)
	
	ifdef screenwidth20
		loadpair z_de,4	
	else
		loadpair z_de,10
	endif
	
	jsr locate

	lda lives
	jsr showdecimal

	ifdef screenwidth20
		loadpair z_hl,(screenwidth-19)
		loadpair z_de,(screenheight-8)
	else
		loadpair z_hl,(screenwidth-38)
		loadpair z_de,(screenheight-16)
	endif
	jsr locate

	lda level
	jsr showdecimal

	ifdef screenwidth20
		loadpair z_hl,(screenwidth-19)
	else
		loadpair z_hl,(screenwidth-38)
	endif

	ifdef screenwidth20
		loadpair z_de,4
	else
		loadpair z_de,10
	endif

	jsr locate

	lda cpuai				;ai=topright
	jsr showdecimal
	
	lda boostpower
	jsr showboostpower		;boost=bottomleft

	ifdef screenwidth20
		loadpair z_ix,0
		loadpair z_iy,12

		loadpair z_hl,16
		loadpair z_de,12		
	else
		loadpair z_ix,0
		loadpair z_iy,24

		loadpair z_hl,32
		loadpair z_de,24
	endif

	jsr drawline			;start=ix,iy... dest hl,de=yoffset 

	loadpairsafe z_hl,0
	ifdef screenwidth20
		loadpair z_de,(-12)
	else
		loadpair z_de,(-24)
	endif
	jsr drawlinerelative
	
	ifdef screenwidth20
		loadpair z_ix,(screenwidth-17)
		loadpair z_iy,(screenheight-1)

		loadpair z_hl,(screenwidth-17)
		loadpair z_de,(screenheight-13)
	
	else
		loadpair z_ix,(screenwidth-33)
		loadpair z_iy,(screenheight-1)

		loadpair z_hl,(screenwidth-33)
		loadpair z_de,(screenheight-25)
	endif

	jsr drawline				;start=ix,iy... dest hl,de=yoffset 

	ifdef screenwidth20
		loadpair z_hl,16
	else
		loadpair z_hl,32
	endif
	loadpair z_de,0
	jsr drawlinerelative

	ifdef screenwidth20
		loadpair z_ix,16
		loadpair z_iy,(screenheight-1)

		loadpair z_hl,16
		loadpair z_de,(screenheight-13)
	else
		loadpair z_ix,32
		loadpair z_iy,(screenheight-1)

		loadpair z_hl,32
		loadpair z_de,(screenheight-25)
	endif

	jsr drawline;		start=ix,iy... dest hl,de=yoffset 

	ifdef screenwidth20
		loadpair z_hl,(-16)
	else
		loadpair z_hl,(-32)
	endif
	loadpair z_de,0

	jsr drawlinerelative

	ifdef screenwidth20
		loadpair z_ix,(screenwidth-17)
		loadpair z_iy,0
		loadpair z_hl,(screenwidth-17)
		loadpair z_de,12
	else
		loadpair z_ix,(screenwidth-33)
		loadpair z_iy,0
		loadpair z_hl,(screenwidth-33)
		loadpair z_de,24
	endif

	jsr drawline	;start=ix,iy... dest hl,de=yoffset 

	ifdef screenwidth20
		loadpair z_hl,16
	else
		loadpair z_hl,32
	endif

	loadpair z_de,0

	jsr drawlinerelative

	rts


	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	

showboostpower:
	pha
		loadpair z_hl,(-4)
	ifdef screenwidth20
		loadpair z_de,(screenheight-6)
	else
		loadpair z_de,(screenheight-16)
	endif
		jsr locate
	pla			
	lsr									;halve boost

	jsr showdecimal						;show number to screen

	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


gameover:
	jsr cls

	ifdef screenwidth20
		lda #0
	else
		lda #1
	endif

	sta scale				;text scale
	
	lda #color1
	sta linecolor		;color

	ifdef screenwidth40
		loadpair z_hl,50				;xpos
	else
		loadpair z_hl,20
	endif

	loadpair z_de,(screenheight/2-20)	;ypos
	jsr locate

	loadpair z_hl,tgameover

	jsr printstring				;show gameover message

	ifdef screenwidth20
		lda #-1
	else
		lda #0
	endif
	sta scale
	
	loadpair z_hl,bestlevel
	lda level

	ldx #0
	cmp (z_hl,x)				;beat best level?
	
	beq majorsuckage
	bcs newbest

	jmp majorsuckage

newbest:

	sta (z_hl,x)				;yes? update best!
	lda #color3
	sta linecolor

	ifdef screenwidth40
		loadpair z_hl,40
	else
		loadpair z_hl,10
	endif

	loadpair z_de,(screenheight/2+20)
	jsr locate

	loadpair z_hl,tyourock		;new highscore message
	jsr printstring
	jmp waitrestart

majorsuckage:
	lda #color2
	sta linecolor

	ifdef screenwidth40
		loadpair z_hl,20
	else
		loadpair z_hl,20
	endif
	loadpair z_de,(screenheight/2+20)

	jsr locate


	loadpair z_hl,tyousuck			;no highscore message
	jsr printstring

waitrestart:
	jsr waitforfire

	jmp mainmenu			;new game


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


handleplayer:

	lda boost	;player using boost?

	bne noboost

	lda boostpower
	and #%11111110
	sta z_e

	lda shownboostpower
	and #%11111110
	cmp z_e
	beq boostpowersame

	lda #0
	sta linecolor
	lda shownboostpower

	jsr showboostpower				;clear old boost power

	dec boostpower					;decrease boost power
	
	lda #color1
	sta linecolor

	lda boostpower
	sta shownboostpower

	jsr showboostpower				;show new boost power

	jmp noboost
	
boostpowersame:

	lda boostpower					;decrease boost power
	sec
	sbc #1
	sta boostpower

noboost:

	lda tick	;no boost=move every other tick
	and boost					;boost=move every tick
	
	beq nonotplayertick
		jmp notplayertick
nonotplayertick

	loadpairfrom z_bc,playerxacc
	
	loadpairfrom z_hl,playerx	
	jsr addHl_Bc			;move player x
	savepairto z_hl,playerx

	jsr exde_hl

	loadpairfrom z_bc,playeryacc

	loadpairfrom z_hl,playery
	jsr addHl_Bc						;move player y
	savepairto z_hl,playery
	
	
	
	lda z_e
	sta z_b				;xl
	
	lda z_l
	sta z_c				;yl
	
	lda z_d
	pha

	pushpairsafe z_bc

		jsr point	;has player collided?
		beq nothit
	
		dec lives	;yes! depleat lives

		lda lives
		beq norestart
		jmp restart
norestart
	
		jmp gameover
nothit:

	pullpair z_bc
	pla

	ldx #color2
	stx z_d
	
	jsr pset		;draw player to screen
notplayertick:
	rts

	
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;                                      setplayerdirection:			;rotate player/cpu in response to keypress
setplayerdirection:
	pushpair z_hl
		ldy #0
		
		lda playerdirection
		and #%00000011				;wrap around 4 directions
		sta playerdirection

		asl							;4 bytes per direction
		asl
		tax
			loadpair z_hl,directions
		txa
		clc
		adc z_l
		sta z_l
		bcc DirCopy
		inc z_h
		
DirCopy	
		lda (z_hl),y
		sta (z_ix),y
		iny
		cpy #4								;copy 4 bytes to player/cpu acceleration
		bne DirCopy

	pullpair z_hl
	rts	
	


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





	
handlecpu:
	lda tick
	cmp #1
	beq nonocputick				;move every other other tick
	jmp nocputick
nonocputick:

	loadpairfrom z_bc,cpuxacc
	loadpairfrom z_hl,cpux
	jsr AddHl_Bc				;move x
	jsr applyai					;look ahead for cpu responses
	jsr exDe_Hl
	loadpairfrom z_bc,cpuyacc
	loadpairfrom z_hl,cpuy

	jsr AddHl_Bc				;move y
	jsr applyai					;look ahead for cpu responses	

	lda z_e
	sta z_b			;xl

	lda z_l
	sta z_c			;yl
	
	lda z_d			;xh	

	jsr point				;x=ab y=c		;test cpu pos (for ai)

	bne nonotmovecpu
	jmp notmovecpu
	
nonotmovecpu:
	jsr dorandom			;change cpu turn direction?
	cmp #240
	bcc cpudirectionok

	lda cputurn
	jsr neg					;flip rotation direction
	sta cputurn

cpudirectionok:
	lda #3					;rotation tests
	sta z_b
nextcputest:
	loadpair z_hl,cputurn

	lda cpudirection

	clc
	ldx #0	
	adc (z_hl,x)			;rotate once

	and #%00000011
	sta cpudirection

	lda z_b
	cmp #2
	beq testskip			;facing opposited direction?

	pushpair z_bc
		lda cpudirection
		asl
		asl
		sta z_c

		lda #0
		sta z_b
	
		pushpair z_ix

			loadpair z_ix,directions	;get cpu direction
			jsr AddIX_BC

			ldy #2
			lda (z_ix),y
			sta z_l
	
			iny				;A=3
			lda (z_ix),y
			sta z_h
	
			ldy #0
			lda (z_ix),y
			sta z_e
	
			iny 
			lda (z_ix),y
			sta z_d
	
		pullpair z_ix		;call cputest ;hl=yacc ;de=xacc - test direction
		jsr cputest
		sta z_as

	pullpair z_bc
	lda z_as

	beq notmovecpu				;found a move

testskip:
	dec z_b					;repeat check
	bne nextcputest

notmovecpu:

	loadpairfrom z_bc,cpuxacc
	
	loadpairfrom z_hl,cpux	;back up x
	jsr AddHl_Bc
	savepairto z_hl,cpux
	
	jsr exde_hl
	
	loadpairfrom z_bc,cpuyacc
	
	loadpairfrom z_hl,cpuy	;back up y
	jsr AddHl_Bc
	savepairto z_hl,cpuy
	
	
	lda z_d				;xh
	pha
		lda z_e		
		sta z_b			;xl

		lda z_l
		sta z_c			;yl
		
		pushpair z_bc
			lda z_d

			jsr point	;check cpu position
			sta z_as
		pullpair z_bc
		
		lda z_as
		beq nothitcpu
		
;CPU has Died!
		loadpair z_hl,level	;level

		ldx #0
		lda (z_hl,x)
		clc
		adc #1
		sta (z_hl,x)
	pla
	jmp restart
	
nothitcpu:

		lda #color3
		sta z_d
	pla

	jsr pset			;draw cpu to screen

nocputick:
	rts


		
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		
		
cputest:				;hl=yacc ;de=xacc	- test a possible turn

	savepairto z_hl,cpuyacc
	savepairto z_de,cpuxacc	;store the tested rotation
	
	pushpair z_hl
	pushpair z_de	
		lda z_ixh
		sta z_b
	
		lda z_ixl
		sta z_c
	
		jsr addhl_bc		;add x-offset
		jsr exde_hl
	
		lda z_iyh
		sta z_b
	
		lda z_iyl
		sta z_c
	
		jsr addhl_bc		;add y-offset
	
		lda z_e
		sta z_b				;xl

		lda z_l
		sta z_c				;yl
		
		lda z_d				;xh

		jsr point			;test point
		sta z_as

	pullpair z_de
	pullpair z_hl

	lda z_as
	
	rts			;Return NZ=Collide Z=NoCollide
;                                      





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		
		
		
		
		
;look ahead level
applyai:

	ldy cpuai					;lower=tighter reactions (better)
applyaiB:
	bne applyaiagain
	rts							;Return if 0
applyaiagain:
	jsr addhl_bc	
	dey	
	bne applyaiagain
	rts
;                                      




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		

psethlde:	;DE=Xpos HL=Ypos A=Color
	sta z_as
	pushpair z_hl
	pushpair z_bc
	pushpair z_de
		lda z_d
		tax
			lda z_l		;Y
			sta z_c
		
			lda z_e		;X Bottom Byte
			sta z_b
		
			lda z_as
			sta z_d		;Color
		txa				;X Top Byte			
		jsr pset
	pullpair z_de
	pullpair z_bc
	pullpair z_hl
	rts
	



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Z80 style functions		
SubDE_BC:		
	sec
SbcDE_BC:			;Subtract BC from DE
	lda z_E		;Subract C from E
	sbc z_C
	sta z_E
	lda z_D		;Subtract B from D (with any carry)
	sbc z_B
	sta z_D
	rts		
		
	
ExDe_Hl:				;Swap z_HL and z_DE
	pha
		lda z_h
		pha
			lda z_l
			pha
				lda z_d
				sta z_h
				lda z_e
				sta z_l
			pla
			sta z_e
		pla
		sta z_d
	pla
	rts
	
AddIX_BC:				;Add z_BC to z_IX
		clc
AdcIX_BC				
		lda z_c			;Add C to ixl
		adc z_ixl
		sta z_ixl
		lda z_b 		;Add B to ixh (with any carry)
		adc z_ixh
		sta z_ixh
		rts

neg:				;Negate a 
	eor #255
	clc
	adc #1
	rts
	
	