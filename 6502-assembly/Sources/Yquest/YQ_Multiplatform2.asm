
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
	add z_l
	sta z_l
	lda (z_hl,x)		;Get Byte from LUT2
	eor z_d				;Xor 1st lookup
	rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Randomly Position an object IX without colliding with any other objects 
;Test C objects
saferandomizeobjectposition
	pushpair z_iy
	pushpair z_bc
nextobjectinitrerandomize:
		jsr randomizeobjectposition		;Select a new position
		loadpair z_iy,objectarray		;We need to check if an object is colliding
nextobjectinittextnext:
		ldx #0

		lda (z_ixh,x)					;Don't compare object to itself!
		cmp (z_iyh,x)
		bne checkthisobject
		lda (z_ixl,x)
		cmp (z_iyl,x)
		bne checkthisobject
		jmp checknextobject				;Comparing an object to itself!
checkthisobject:
		pushpair z_bc
			ldy #o_xpos
			lda (z_ix),y
			sta z_d
			lda (z_iy),y
			sta z_b
			
			ldy #o_ypos
			lda (z_ix),y
			sta z_e
			lda (z_iy),y
			sta z_c
			jsr rangetestw
			sta z_as
		pullpair z_bc					;Return AS<>0 if collided limit 
		lda z_as
		bne nextobjectinitrerandomize
	
checknextobject:
		addpair z_iy,objectbytesize
		dec z_c
		bne nextobjectinittextnext
	pullpair z_bc
	pullpair z_iy
	rts
	
nextobjectinitrerandomize2:;      ;reset enemy count and restart
	pullpair z_bc
	jmp nextobjectinitrerandomize


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;See if object XY pos DE hits object BC
;Return A=1 Collision... A!=1 means no collision
rangetestw:				;object range check
	pushpair z_hl
		lda #5			;Wide
		sta z_h
		lda #10
		sta z_l
	jmp rangetest2

rangetest:				;Bullet range check
	pushpair z_hl
		lda #3			;Narrow
		sta z_h
		lda #6
		sta z_l                        	
rangetest2:				;See if object XY pos DE hits object BC (with radius HL)
	pushpair z_bc
		lda z_b			;X axis check
		sec
		sbc z_h
		bcc rangetestb
		cmp z_d
		bcs rangetestoutofrange
rangetestb:
		clc
		adc z_h
		adc z_h
		bcs rangetestd
		cmp z_d
		bcc rangetestoutofrange
		
rangetestd:
		lda z_c			;Y Axis Check
		sec
		sbc z_l
		bcc rangetestc
		cmp z_e
		bcs rangetestoutofrange
rangetestc:
		clc
		adc z_l
		adc z_l
		bcs rangeteste
		cmp z_e
		bcc rangetestoutofrange
		
rangeteste:
	pullpair z_bc
	pullpair z_hl
	lda #1			;1=Collided
	rts
rangetestoutofrange:
	pullpair z_bc
	pullpair z_hl
	lda #0			;0=No Collision
	rts
	


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

dorangedrandom:			;return a value between z_b and z_c
	jsr dorandom
	cmp z_b
	bcc dorangedrandom
	cmp z_c
	bcs dorangedrandom
	rts

randomxpos:					;Pick a random horizontal location
	lda #$02
	sta z_b					;Min
	lda #screenobjwidth	
	sta z_c					;Max
	jsr dorangedrandom
	ldy #o_xpos
	sta (z_ix),y
	rts

randomypos:					;Pick a random Vertical Position
	lda #$08
	sta z_b					;Min
	lda #screenobjheight
	sta z_c					;Max
	jsr dorangedrandom
	ldy #o_ypos
	sta (z_ix),y
	rts

randomizeobjectposition:	;Randomize Location of object (both)
	pushpairsafe z_bc
		jsr randomxpos
		jsr randomypos
	pullpairsafe z_bc
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	
randomizeedgeobjectposition:	;Put an enemy at the edge of the screen
	pushpairsafe z_bc
		jsr dorandom
		and #%00000011			;Pick one of the 4 sides
		beq randomtop
		cmp #1
		beq randombottom
		cmp #2
		beq randomleft
		cmp #3
		beq randomright
randomtop:
		jsr randomxpos
			ldy #o_ypos			
			lda #8
			sta (z_ix),y
		jmp randomizeedge		;Put enemy at top of screen
randombottom:
		jsr randomxpos
			ldy #o_ypos
			lda #screenobjheight-8
			sta (z_ix),y
		jmp randomizeedge		;Put enemy at bottom of screen
randomleft:
		jsr randomypos
			ldy #o_xpos
			lda #4
			sta (z_ix),y
		jmp randomizeedge		;Put Enemy on left of screen
randomright:
		jsr randomypos
			ldy #o_xpos
			lda #screenobjwidth-6
			sta (z_ix),y		;Put Enemy on right of screen
randomizeedge:
	pullpairsafe z_bc
	rts
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PrintSpace:				;Used by decimal function
	lda #' '
	jmp PrintChar
	
Locate:					;Set Cursor to pos X,Y
	stx CursorX	
	sty CursorY
	rts
	
LocateAndPrintString:	;Locate X,Y... print z_hl
	jsr Locate
	jmp PrintString
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

drawui:
	lda spriteframe				;4 frames of animation!
	clc
	adc #1						;Next frame
	and #%00000011
	sta spriteframe

	ldx #0
	ldy #0
	loadpair z_hl,txtlives		;show lives
	jsr LocateAndPrintString
	lda lives
	clc
	adc #'0'
	jsr printchar

	ifdef screenwidth20
		ldx #screenwidth-5
	else
		ldx #screenwidth-12
	endif
	ldy #0
	loadpair z_hl,txtcrystals	;Show Crystals
	jsr LocateAndPrintString
	lda crystals
	jsr showdecimal

	ifdef screenwidth20
		ldx #screenwidth-5
	else
		ldx #screenwidth-10
	endif
	ldy #screenheight-1
	loadpair z_hl,txtlevel
	jsr LocateAndPrintString	;show level

	lda level
	clc
	adc #1
	jsr showdecimal

	ldx #$00
	ldy #screenheight-1
	jsr locate
	loadpair z_hl,txtscore	
	jsr locateandprintstring
	loadpair z_de,Score			;Show the highscore
	ldx #4
	jsr BCD_Show
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


	
levelinit:			;define the new level data

	lda #<levelmap	;Load levelmap address
	sta z_l
	lda #>levelmap
	sta z_h
	
	lda level
	and #%00001111	;Levels cycle after 16 levels
	asl 			;2 bytes per pointer
	sta z_c
	jsr AddHL_0C
	
	
	ldy #0
	lda (z_hl),y	;Load levelmap address
	sta z_iyl
	iny
	lda (z_hl),y
	sta z_iyh
	
	ldy #0
	lda (z_iy),y
	sta crystals	;Get crystal count from levelmap
	addpair z_iy,2

	lda #enemies	;Populate enemies
	sta z_b
	loadpair z_ix,objectarray
nextobjecttype:
	pushpair z_iy
		ldy #1
		lda (z_iy),y ;Enemy Count
		sta z_d
		inc z_d
		pushpair z_de
			dey 	;ldy #0
			lda (z_iy),y
			asl 
			asl 
			asl 	;8 bytes per object def
			pha
				loadpair z_iy,enemydefinitions
			pla
			jsr AddIY_A	;Get Enemy Definition offset
		pullpair z_de
nextobjectinitloop:
		dec z_d
		bne notlastobject	;Last object of this type?
			;Last Object!
			pullpair z_iy		;so reset and move on
			addpair z_iy,2
			jmp nextobjecttype
notlastobject:
		pushpair z_de
		pushpair z_bc
			
			ldy #d_sprnum	;Fill settings from enemy def
			lda (z_iy),y
			ldy #o_sprnum
			sta (z_ix),y
			
			ldy #d_collprog
			lda (z_iy),y
			ldy #o_collprog
			sta (z_ix),y
			
			ldy #d_program
			lda (z_iy),y
			ldy #o_program
			sta (z_ix),y
			
			ldy #d_xacc
			lda (z_iy),y
			ldy #o_xacc
			sta (z_ix),y
			
			ldy #d_yacc
			lda (z_iy),y
			ldy #o_yacc
			sta (z_ix),y

			pushpair z_iy
				jsr randomizeobjectposition	;Position object
				lda enemies
				sec
				sbc z_b
				beq testnextobposok		;Don't check 1st object
				sta z_c
				
				;Check if object collides with existing
				jsr saferandomizeobjectposition
testnextobposok:
				addpair z_ix,objectbytesize
			pullpair z_iy
		pullpair z_bc
		pullpair z_de
		dec z_b
		beq notnextobjectinitloop			;Do next object
			jmp nextobjectinitloop
notnextobjectinitloop:
	pullpair z_iy
	
	;Clear both bullet arrays
	loadpair z_hl,bulletarray		
						 ;BulletArray+EnemyBulletArray
	loadpair z_bc,((bulletcount*objectbytesize*2)-1)
	lda #255
	jmp CLDIR			;Clear bullets to 255	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


resetplayer:
	lda #48							;set invincibility time
	sta invincibility

	lda #0							;silence sound

	sta playingsfx
	loadpair z_hl,playerobjectbak	;Default player state

	loadpair z_de,playerobject		;current player state
	loadpair z_bc,objectbytesize
	jmp ldir						;reset player params

	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


	
DrawPlayer:
	loadpair z_ix,PlayerObject
	
	lda Invincibility			;Flash player if invincible
	beq InvOk
	dec Invincibility
InvOk:
	and #%00000010
	bne InvSkip
	jsr DrawObject				;Draw Player Sprite
InvSkip:
	rts


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



drawandmove:					;handle player and enemies
	ldx #bulletcount
	loadpair z_ix,bulletarray	;Redraw Player Bullets
nextbulletdrawloop:
	pushx
		jsr blanksprite			
		jsr drawobject
		AddPair z_IX,objectbytesize
	pullx
	dex 
	bne nextbulletdrawloop

	jsr drawplayer				;Show Player

	
	;Process Player Bullet collisions
	;(it's slow so we draw separately!)
	ldx #bulletcount
	loadpair z_ix,bulletarray	
nextbulletloop
	pushx
		ldy #o_collprog
		lda (z_ix),y
		cmp #250
		bcs enemynotest
		ldx #enemies
		
;collision detection of bullet and enemy
		loadpair z_iy,objectarray
enemycollideloop:
		pushx
			ldy #o_collprog
			lda (z_iy),y
			cmp #1					;shot crystal
			beq enemynocollide	
			cmp #250				;shot dead/uninitialized object
			bcs enemynocollide
			
			ldy #o_sprnum
			lda (z_iy),y
			cmp #4					;Shot Mine
			beq enemynocollide
		
			ldy #o_xpos
			lda (z_ix),y			;Bullet X
			sta z_d
			lda (z_iy),y			;Enemy X
			sta z_b
			
			ldy #o_ypos
			lda (z_ix),y			;Bullet Y
			sta z_e
			lda (z_iy),y			;Enemy Y
			sta z_c
		
			jsr rangetest			;return NZ if collided limit
			beq enemynocollide
		
			loadpair z_hl,bcd5
			jsr applyscore			;player has shot enemy
			pushpair z_ix
				lda z_iyh
				sta z_ixh
				lda z_iyl
				sta z_ixl				
				jsr blanksprite 	;remove Enemy sprite from screen
			pullpair z_ix

			ldy #o_collprog
			lda #254
			sta (z_iy),y
			lda #%11011111
			sta playingsfx

enemynocollide:
			AddPair z_IY,objectbytesize	;Move To next Enemy
		pullx
		dex
		bne enemycollideloop
enemynotest:
		AddPair z_Ix,objectbytesize		;move to the next bullet
	pullx
	dex
	beq NoNextBullet
		jmp nextbulletloop		;NZ=more bullets to process
NoNextBullet:
	ldx #enemies
	loadpair z_ix,objectarray
nextobjectloop:
	pushx
		ldy #o_xacc
		lda (z_ix),y
	
		ldy #o_yacc
		ora (z_ix),y
		beq DontBlank				;only blank sprite if accel!=0
			jsr blanksprite
DontBlank
		jsr drawobject				;Draw enemy
		jsr objectcollision			;See if player collided
		
		ldy #o_collprog
		lda (z_ix),y
		cmp #254					;254=killed enemy
		bne objectnotdead
		
		jsr dorandom				;Probability of respawn
		and #%00111111
		bne objectnotdead			
			
		lda #0
		ldy #o_collprog				;Respawn enemy
		sta (z_ix),y
			
		jsr randomizeedgeobjectposition
		
		lda #%01000011				;Respawn sound
		sta playingsfx
objectnotdead:
		AddPair z_IX,objectbytesize	;Move to next object
	pullx
	dex
	bne nextobjectloop
	
;Do Enemy Bullets	
	ldx #bulletcount
	loadpair z_ix,enemybulletarray
nextenemybulletloop:
	pushx
		ldy #o_collprog
		lda (z_ix),y
		cmp #250
		bcs bulletplayernocollide
		jsr blanksprite
		jsr drawobject
		
		;Collision detection
		ldy #o_xpos				;Object Pos	
		lda (z_ix),y
		ifdef collisionmaskx
			and #collisionmaskx	;Strip a few bits (for tile systems)
		endif
		sta z_d
		ldy #o_ypos
		lda (z_ix),y
		ifdef collisionmasky
			and #collisionmasky ;Strip a few bits (for tile systems)
		endif
		sta z_e
		
		lda playerx				;Player Pos
		ifdef collisionmaskx
			and #collisionmaskx ;Strip a few bits (for tile systems)
		endif
		sta z_b
		lda playery
		ifdef collisionmasky
			and #collisionmasky ;Strip a few bits (for tile systems)
		endif
		sta z_c
		
		jsr rangetest			;Return NZ if collided limit
		
		beq bulletplayernocollide
			jsr playerhurt		;Kill player
bulletplayernocollide:
		AddPair z_Ix,objectbytesize	;Move to next object
	pullx
	dex 
	bne nextenemybulletloop
	
	lda playingsfx		;Play the current soundeffect (0=nosound)
	cmp playingsfx2
	beq nosound			;See if sound has changed?
	sta playingsfx2
	jsr chibisound		;if it has - update sound.
nosound:
	lda #0
	sta playingsfx		;Silence Sound
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


ApplyScore:
	loadpair z_de,Score	;Destination
	ldx #4				;Bytes of BCD
	jmp BCD_Add			;Add the score

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



DrawObject:	;IX=Object
	ldy #O_CollProg
	lda (z_ix),y
	cmp #250			;255 = object unused
	bcc ObjOK
	rts
ObjOK:
	ldy #o_program		;check animation routine for object
	lda (z_ix),y

	bne Notprogramok
		jmp programok	;Static
Notprogramok
	cmp #1				
	bne notprogram1		
		jmp program1	;Regular
notprogram1:
	cmp #2				
	bne Notprogram2		
		jmp program2	;Faster Change
Notprogram2
	cmp #3				
	bne Notprogram3		
		jmp program3	;Shooting
Notprogram3
	cmp #4				
	bne Notprogram4		
		jmp program4	;Indecisive
Notprogram4:
	cmp #5				
	beq program5		;Waiter
	cmp #6
	beq program6		;Seeker
	
	
program6:				;6=Seeker
	jsr dorandom
	and #%11000000
	beq program6nomove	;move 3 times in 4
	
	lda playerx			;Get Player pos
	sta z_h
	lda playery
	sta z_l
	
	ldy #o_xpos			;Get Object Xpos
	lda (z_ix),y
	sta z_b				
	cmp z_h
	beq program6_xok
	bcc program6_xlow
	dec z_b				;Move Left
	jmp program6_xok
program6_xlow:
	inc z_b				;Move Right
program6_xok:
	lda z_b
	ldy #o_xpos
	sta (z_ix),y		;Save Xpos

	ldy #o_ypos
	lda (z_ix),y		;Get Ypos
	sta z_c
	
	cmp z_l				
	beq program6_yok
	bcc program6_ylow
	dec z_c				;Move Up
	jmp program6_yok
program6_ylow:
	inc z_c				;Move Down
program6_yok:
	lda z_c
	ldy #o_ypos
	sta (z_ix),y		;Save Ypos
	jmp programnomoveb
program6nomove:
	jsr dorandom		;Randomize fire direction
	ldy #o_yacc
	sta (z_ix),y
	jsr dorandom
	ldy #o_xacc
	sta (z_ix),y
	
	and #%00000001
	bne Prog6NoBullet
		jsr enemyfirebullet	;Randomly fire a bullet
Prog6NoBullet:
	jmp programnomove


program5:				;5=waiter
	jsr dorandom
	sta z_b
	and #%10000000
	beq program5b		;Maybe Fire, Maybe Wait
	lda z_b
	and #%00011100		;Chance of continued movement
	beq notprogramok2
	jmp programok
notprogramok2:
	lda z_b
	and #%00000011		;Chance of direction change
	bne programnewdir
program5b:
	jsr dorandom
	sta z_b
	and #%00000011		;Chance of firing
	bne Prog5NoBullet
		jsr enemyfirebullet
Prog5NoBullet:
	jmp programnomove

	
program4:				;4=indecisive
	jsr dorandom
	sta z_b
	and #%00011100		;Chance of direction change
	beq programok
	jmp programnewdir
	
	
program3:				;3=shooting
	jsr dorandom
	sta z_b
	and #%00001111		;Chance of shooting
	bne Prog3NoBullet
		jsr enemyfirebullet
Prog3NoBullet:
	jsr dorandom
	sta z_b
	and #%11111100		;Chance of direction change
	bne programok
	jmp programnewdir

	
program2:				;2=rarely change direction
	jsr dorandom
	sta z_b
	and #%11111100		;chance of direction change
	bne programok
	jmp programnewdir

	
program1:
	jsr dorandom
	sta z_b
	and #%00110000		;Chance of direction change
	bne programok

programnewdir:
	lda z_b
	and #%00000001
	beq dontflipy
	ldy #o_yacc
	jsr negIX				;Flip Y speed
	
programprocessed:
dontflipy:
	lda z_b
	and #%00000010
	beq dontflipx
	ldy #o_xacc
	jsr negIX				;Flip X speed
	
dontflipx:
	jmp programok

programnomove:			;static object
	ldy #o_xpos
	lda (z_ix),y		;Load in last Xypos
	sta z_b
	ldy #o_ypos
	lda (z_ix),y
	sta z_c
	jmp programnomoveb


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


ProgramOK:		;Finished handling movement code
	ldy #O_Xpos
	lda (z_ix),y
	sta LastPosX	
	
	ldy #O_Xacc
	clc
	adc (z_ix),y
	
	ldy #O_Xpos
	sta (z_ix),y	;Update X
	sta z_b
	
	ldy #O_Ypos
	lda (z_ix),y
	sta LastPosY
	
	ldy #O_Yacc
	clc
	adc (z_ix),y
	
	ldy #O_Ypos
	sta (z_ix),y	;Update Y
	sta z_c
	
	
	
ProgramNoMoveB:
;X Boundary Check - if we go <0 we will end up back at &FF
	lda z_b
	cmp #ScreenObjWidth 		
	bcc ObjectPosXOk		;Not Out of bounds X
	ldy #O_Xacc
	
	jsr negIX
	jmp ObjectReset 		;Player out of bounds - Reset!
	
ObjectPosXOk:
;Y Boundary Check - only need to check 1 byte
	lda z_c
	cmp #ScreenObjHeight
	bcc ObjectPosYOk		;Not Out of bounds Y
	ldy #O_Yacc
	jsr negIX	;Player out of bounds - Reset!
						
ObjectReset:
	lda LastPosX 			;Reset Xpos	
	sta z_b
	ldy #O_Xpos
	sta (z_IX),y
	lda LastPosY 			;Reset Xpos	
	sta z_c
	ldy #O_Ypos
	sta (z_IX),y
	
	ldy #O_CollProg
	lda (z_IX),y
	cmp #3					;Is object Bullet?
	bne ObjectNotBullet
;Object is a bullet	
	jsr  BlankSprite		;Bullet offscreen - clear sprite
	lda #255
	ldy #O_CollProg
	sta (z_IX),y			;Kill the bullet
	rts
	
	
ObjectNotBullet:
ObjectPosYOk:
	ldy #O_HSprNum			;Check hardware sprite number
	lda (z_IX),y
	beq DoGetSpriteObj2		;0=Soft Sprite
	sec
	sbc #1
	cmp #128				
	bcs DoGetSpriteObj2		;>128= Soft Sprite

DoGetSpriteObj2b:
	jmp DoGetHSpriteObj		;Hsprite 0-127
	
DoGetSpriteObj2:
	jmp DoGetSpriteObj	
	
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
AddIY_A				;Add DE to HL
	clc
	adc z_iyl
	sta z_iyl
	lda #0			;Add D to H (with any carry)
	adc z_iyh
	sta z_iyh
	rts	
	
INC_IX_Y:
	lda (z_ix),y
	clc
	adc #1
	sta (z_ix),y
	rts


DEC_IX_Y:
	lda (z_ix),y
	sec
	sbc #1
	sta (z_ix),y
	rts	
	
negIX:		;Invert Z_IX  +5 <> -5
	lda (z_IX),y
	eor #255
	clc
	adc #1
	sta (z_IX),y	
	rts


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

objectcollision:						;See if object has hit the player (object in IX)
	ldy #o_collprog
	lda (z_ix),y
;1=crystal 0=anything 3=bullet (player) 255=nothing 254=dead	

	cmp #250					;collision routine >250 = disabled object
	bcc CollNoReturnA
		rts
CollNoReturnA:
	pushpair z_bc
	pushpair z_de
		lda playerx
		ifdef collisionmaskx
			and #collisionmaskx		;Strip a few bits (for tile systems)
		endif
		sta z_b
		lda playery
		ifdef collisionmasky
			and #collisionmasky		;Strip a few bits (for tile systems)
		endif
		sta z_c
		ldy #o_xpos
		lda (z_ix),y
		ifdef collisionmaskx
			and #collisionmaskx		;Strip a few bits (for tile systems)
		endif
		sta z_d
		ldy #o_ypos
		lda (z_ix),y
		ifdef collisionmasky
			and #collisionmasky		;Strip a few bits (for tile systems)
		endif
		sta z_e
		jsr rangetest				;NZ = Collision
		sta z_as
	pullpair z_de
	pullpair z_bc
	lda z_as
	bne CollNoReturnB
		rts
CollNoReturnB:
	ldy #o_collprog
	lda (z_ix),y
	beq noplayercrystal				;0=player has been hurt by enemy 
		jmp playercrystal
noplayercrystal
	jmp playerhurt
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


playercrystal:				;prog 1=got crystal

	jsr blanksprite			;remove old player sprite
	
	loadpairsafe z_hl,bcd1
	jsr applyscore			;add points to score

	lda #%00001111
	sta playingsfx			;make a sound

	dec crystals			;decrease remaining crystals
	bne notnextlevel	
		jmp nextlevel		;level complete
notnextlevel:
	lda crystals
	cmp #onscreencrystals	;only 5 max crystals shown onscreen
	bcc clearcrystal

;if we've still got more crystals to collect, respawn crystal	
	lda #enemies
	sta z_c
	jsr saferandomizeobjectposition	;give crystal a new position
	rts

clearcrystal:
	lda #255				;255= unused object
	ldy #o_collprog
	sta (z_ix),y
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Prog 0=Hurts Player

playerhurt:
	lda invincibility				;Is player invincible?
	beq PlayerNotInv
	rts
PlayerNotInv:
	lda #0							;Player hurt - stop movement
	sta playeraccx
	sta playeraccy

	pushpair z_ix
		loadpair z_ix,playerobject
		ifdef BuildC64
			lda #1
		else
			lda #5
		endif
		sta playerobject
		ldx #0
playerdeathanim:
		pushx
			stx spriteframe			;set frame of explosion
			jsr drawobject			;show player
			lda spriteframe
			asl 
			asl 
			asl 
			ora #%11000111
			jsr chibisound
		
			loadpair z_bc,$2000		;Delay
playerdeathanimloop:
			jsr decbc
			lda z_b
			ora z_c
			bne playerdeathanimloop
		pullx
		inx							;next anim frame
		cpx #4
		bne playerdeathanim
		jsr blanksprite
	pullpair z_ix


	jsr resetplayer					;reset player to centre
	lda #0
	jsr chibisound					;silence sound

	lda lives
	beq playerdead					;Any lives left?
	dec lives						;decrease life count
	rts
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

playerdead:	
	ifdef ScreenWidth32
		ldx #$02
		ldy #$0B			;Text position
	endif
	
	ifdef ScreenWidth40
		ldx #$06
		ldy #$0C
	endif
	ifdef ScreenWidth20
		ldx #$06
		ldy #$09
	endif
	LoadPair z_hl,txtDead
	jsr LocateAndPrintString		;Show Player Dead Message

	loadpair z_hl,Score
	loadpair z_de,HiScore
	loadone z_b,#4
	jsr BCD_Cp					;Check if we have a new highscore
	bcs GameOverWaitForFire
	
;New Highscore
	ifdef ScreenWidth32
		ldx #$0A
		ldy #$0D			;New Highscore message pos
	endif
	ifdef ScreenWidth40
		ldx #$0E
		ldy #$10
	endif
	ifdef ScreenWidth20
		ldx #$05
		ldy #$0B
	endif
	loadpair z_hl,TxtNewHiscore
	jsr LocateAndPrintString				;Show the 'new highscore' message
	
;Transfer score to highscore
	loadpair z_bc,4
	loadpair z_hl,Score
	loadpair z_de,HiScore
	jsr ldir					;update the highscore
	
GameOverWaitForFire:
	jsr WaitForFire
	jmp ShowTitle

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

nextlevel:
	ifdef screenwidth32
		ldx #$09			;level complete message pos
		ldy #$0b
	endif
	ifdef screenwidth40
		ldx #$0d
		ldy #$0c
	endif
	ifdef screenwidth20
		ldx #$03
		ldy #$09
	endif
	loadpair z_hl,txtcomplete
	jsr locateandprintstring

	inc lives				;Extra life every game
	inc level
	jmp startlevel			;Init new level

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
playerfirebullet:
	pushpair z_de
	pushpair z_hl
	pushpair z_bc
		loadpair z_iy,bulletarray	;Check if there are any spare bullets
		ldx #bulletcount
checknextbullet:
		ldy #o_collprog
		lda (z_iy),y
		cmp #250					;Yes! - create a bullet
		bcs foundbullet
		addpair z_iy,ObjectByteSize
		dex
		bne checknextbullet
checkbulletreturn:
	pullpair z_bc
	pullpair z_hl
	pullpair z_de
	rts

foundbullet:			;player can fire
	lda #%00000001
	sta playingsfx		;Make bullet sound
	
	ldx #0
	ldy #0				;Zero Acceleration of bullet
	
	
	lda playeraccx	;fire bullet depending on player direction
	beq xzero
	ldx #-4			;Left
	cmp #127
	bcs xnegative
	ldx #4				;Right
	sta z_b
xnegative:
xzero:
	lda playeraccy
	beq yzero
	ldy #-8			;up

	cmp #127
	bcs ynegative
	ldy #8				;Down
	
ynegative:
yzero:
	stx z_b
	sty z_c
	
	txa				;x and y=0? no bullet
	ora z_c
	beq checkbulletreturn

	;bullet acceleration
	txa
	ldy #o_xacc
	sta (z_iy),y
	
	lda z_c
	ldy #o_yacc
	sta (z_iy),y

	
	lda playerx			;bullet starts at player position
	ldy #o_xpos
	sta (z_iy),y

	lda playery
	ldy #o_ypos
	sta (z_iy),Y

	lda #6				;Bullet Sprite
	ldy #o_sprnum
	sta (z_iy),y

;bullet program
	ldy #o_program
	lda #0
	sta (z_iy),y
	ldy #o_collprog
	
;bullet collision routine  
	lda #3
	sta (z_iy),y
	jmp checkbulletreturn

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


enemyfirebullet:
	pushpair z_de
	pushpair z_hl
	pushpair z_bc
		loadpair z_iy,enemybulletarray		;check if there are any spare bullets
		ldx #bulletcount
enemychecknextbullet:
		pushx
			ldy #o_collprog
			lda (z_iy),y
			cmp #250
			bcs enemyfoundbullet	;yes! - create a bullet
			addpair z_iy,objectbytesize
		pullx
		dex
		bne enemychecknextbullet
enemycheckbulletreturn2:
	pullpair z_bc
	pullpair z_hl
	pullpair z_de
	rts
	
enemycheckbulletreturn:
	pullx
	jmp enemycheckbulletreturn2
	
enemyfoundbullet:
	lda #%11000011			;make bullet sound
	sta playingsfx
	
	ldx #0
	ldy #o_xacc				;convert enemy accel to bullet accel (x)
	lda (z_ix),y
	beq enemyxzero
		ldx #-2				;left
	cmp #127
	bcs enemyxnegative
		ldx #2				;right
enemyxnegative:
enemyxzero:
	txa
	pha
		ldx #0
		ldy #o_yacc				;convert enemy accel to bullet accel (y)
		lda (z_ix),y
		beq enemyyzero
			ldx #-2				;Up
		cmp #127
		bcs enemyynegative
			ldx #2				;Down
enemyynegative:
enemyyzero:
		stx z_c	;Ypos
	pla
	sta z_b	;Xpos
	
	
	;x and y=0? no bullet
	ora z_c
	beq enemycheckbulletreturn

	lda z_b
	ldy #o_xacc				;movement speed
	sta (z_iy),y
	lda z_c
	ldy #o_yacc
	sta (z_iy),y

	ldy #o_xpos				;ix=enemy
	lda (z_ix),y
	sta (z_iy),y			;iy=bullet

	ldy #o_ypos
	lda (z_ix),y
	sta (z_iy),y

	ldy #o_sprnum
	lda #7					;bullet sprite
	sta (z_iy),y


	ldy #o_program
	lda #0
	sta (z_iy),y

	ldy #o_collprog
	lda #3
	sta (z_iy),y
	jmp enemycheckbulletreturn

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;IX=Pointer to first object
;z_C = Hsprite Num ... z_B = Count
SetHardwareSprites:		
	ldy #O_HSprNum
	lda z_c
	sta (z_IX),y		;HSpriteNum = z_C
	
	addpair z_ix,objectbytesize
	inc z_c
	dec z_b
	bne SetHardwareSprites
	rts
	
	