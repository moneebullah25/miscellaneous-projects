spr_tilel equ 0
spr_tileh equ 1

spr_pattl equ 2
spr_patth equ 3

spr_width equ 4
spr_height equ 5

spr_xpos equ 6
spr_ypos equ 7

spr_refreshtile equ 8
spr_refreshsprite equ 9

spr_oldxpos equ 10
spr_oldypos equ 11

spr_flags equ 12

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



FlagSpriteForRefresh:			;Mark sprite needs redrawing
	lda #1
	ldy #Spr_RefreshTile
	sta (z_ix),y
	ldy #Spr_RefreshSprite
	sta (z_ix),y
	rts		
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DrawSprite:						;check if we need to do redeaw
	ldy #spr_refreshsprite
	lda (z_ix),y
	bne drawsprite_NeedRedraw
	rts							;No Redraw Needed
drawsprite_NeedRedraw:

	lda #0
	ldy #spr_refreshsprite
	sta (z_ix),y

drawspritealways:				;force redraw


	ldy #spr_tilel		;Tile Map source
	lda (z_ix),y
	sta z_iyl
	ldy #spr_tileh
	lda (z_ix),y
	sta z_iyh


	ldy #spr_xpos		;sprite position X
	lda (z_ix),y		
	sta z_b
	ldy #spr_oldxpos
	sta (z_ix),y

	ldy #spr_ypos		;sprite position X
	lda (z_ix),y
	sta z_c
	ldy #spr_oldypos
	sta (z_ix),y


	ldy #spr_pattl		;Sprite pattern data
	lda (z_ix),y
	sta z_es

	ldy #spr_patth
	lda (z_ix),y
	sta z_ds

	
	

	ldy #spr_flags
	lda (z_ix),y		;Flip flag
	pha
		ldy #spr_width
		lda (z_ix),y
		sta z_d
		pha
			ldy #spr_height
			lda (z_ix),y
			sta z_e
			jsr ex_de_hl		;HL=Width,Height
		
			ldy #spr_flags		;Get Xflip state
			lda (z_ix),y
	
			jsr docrop			;Crop the sprite BC=XY pos 
								;HL=WidthHeigh, IY=source data
								
			bcs drawabort		;Nothing to draw
			
			
			lsr z_l
			lsr z_l				;Convert to tile width
			
			lsr z_h
			lsr z_h				;convert to tile height

			pushpair z_hl
				jsr getscreenpos
			pullpair z_ix		;Width Height
			
			lda z_iyh			;Tilemap source
			sta z_d
			lda z_iyl
			sta z_e
		pla
		lsr
		lsr		
		sta z_iyh				;Tilemap width
	pla
	and #%00000001				;Xflip state
	bne Flipped_drawtilemap
	jmp drawtilemap				;Regular Tilemap
	
Flipped_drawtilemap:
	jmp drawtilemaprev			;Xflipped Tilemap

drawabort:
	pla
	pla
	rts

	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;DE=Tilemap DE'=Pattern data IX=WH IYH=Tilemap Width HL=Vram Dest

DrawTilemap:
NextLine:
	lda z_d
	sta z_bs			;BC' = Tilemap Source
	lda z_e
	sta z_cs
	
	lda z_ixh
	sta z_iyl				;Draw Width 
	
	
	pushpair z_hl
	pushpair z_de
		ifdef striproutine
			lda striproutine	;Draw type?
			bne dostriprevver

			jsr dostrip			;Normal
			jmp dostripdone
dostriprevver:
			jsr dostriprev		;Reverse
dostripdone:
		else
		
			jsr dostrip			;Draw a Hline
striproutine_plus2:	;<-- Use Self modifying code to switch routine

		endif
	pullpair z_de				;Tilemap Source
	pullpair z_hl				;Screen Pos
		
	
;Platform specific NextLine Commands
	
	ifdef BuildAP2
		lda z_l
		clc
		adc #$80
		sta z_l
		bcc SpriteNextLine
		inc z_h
		lda z_h
		and #%00000011
		bne SpriteNextLine
		lda z_h
		sec
		sbc #%00000100
		sta z_h
		lda z_l
		clc
		adc #$28
		sta z_l
SpriteNextLine:
	endif
	ifdef buildLNX	
		lda z_L
		clc
		adc #$E0
		sta z_L
		lda z_H
		adc #$1
		sta z_H
	endif
	ifdef BuildPET
		lda z_l
		clc
		adc #40
		sta z_l
		bcc SpriteNextLine
		inc z_h
SpriteNextLine:
	endif	
	ifdef BuildVIC
		lda z_l
		clc
		adc #22
		sta z_l
		bcc SpriteNextLine
		inc z_h
SpriteNextLine:
	endif	
	ifdef BuildPCE
		lda z_l
		clc
		adc #32
		sta z_l
		bcc SpriteNextLine
		inc z_h
SpriteNextLine:
	endif	
	ifdef BuildNES
		lda z_l
		clc
		adc #32
		sta z_l
		bcc SpriteNextLine
		inc z_h
SpriteNextLine:
	endif	
	ifdef BuildSNS
		lda z_l
		clc
		adc #64
		sta z_l
		bcc SpriteNextLine
		inc z_h
SpriteNextLine:
	endif	
	ifdef BuildBBC
		inc z_h	
		inc z_h
	endif
	ifdef BuildC64
		lda z_l
		clc
		adc #$40
		sta z_l
		lda z_h
		adc #$1
		sta z_h
	endif	
	ifdef BuildA52
		lda z_l
		clc
		adc #$40
		sta z_l
		lda z_h
		adc #$1
		sta z_h
	endif	
	ifdef BuildA80
		lda z_l
		clc
		adc #$40
		sta z_l
		lda z_h
		adc #$1
		sta z_h
	endif	
	
	lda z_iyh			;Update Tilemap source (Add width)
	clc
	adc z_e
	sta z_e
	bcc noupper2
	inc z_d
noupper2:

	dec z_ixl			;Repeat for next line
	bne nextline
	rts


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


DrawTilemapRev:			;Draw a flipped tilemap

	lda z_ixh			;Width of tilemap
	sec
	sbc #1
	clc
	adc z_e
	sta z_e						;Move to right hand of tilemap

	bcc dostriprevtopok
	inc z_d
dostriprevtopok:

	ifdef striproutine
		lda #1
		sta striproutine		;Switch drawing routins on ROM
	else
		lda #<dostriprev
		sta striproutine_plus2-2 ;Patch in new draw engine
		lda #>dostriprev
		sta striproutine_plus2-1
	endif
		
	jsr drawtilemap				;Do the draw

	ifdef striproutine
		lda #0
		sta striproutine		;Reset the drawingroutine
	else
		lda #<dostrip
		sta striproutine_plus2-2	;Restore old engine
		lda #>dostrip
		sta striproutine_plus2-1
	endif
	rts

	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
;DE=Tilemap DE'=Pattern data IX=WH IYH=Tilemap Width HL=Vram Dest
	
cls:
	lda z_h
	sta z_ds				;DE' = Pattern Data
	lda z_l
	sta z_es
		
	lda z_d
	sta z_bs				;BC' = Tilemap Source
	lda z_e
	sta z_cs
	
	ifdef BuildVIC
		lda #23				;draw height in tiles
	else
		ifdef BuildLNX
			lda #17			;draw height
		else
			lda #24			;draw height (Default)
		endif
	endif
	sta z_ixl
	
	loadpair z_hl,screenbase ;Vram Destination (Platform Specific)
	
	
NextLineF:
	ifdef BuildVIC
		lda #22 			;draw width
	else
		ifdef BuildLNX
			lda #26 		;draw width
		else
			lda #32 		;draw width (Default)
		endif
	endif
	sta z_iyl
	
	jsr DoStrip			;Platform specific draw routine to draw one
						;8 pixel tall line
						
						
;Platform specific NextLine Commands to correct HL after a full 32 tile strip

	ifdef BuildVIC
		lda z_cs
		clc
		adc #10
		sta z_cs
		bcc ClsNextLine
		inc z_bs
ClsNextLine:
	endif	
	ifdef BuildLNX
		lda z_cs
		clc
		adc #6
		sta z_cs
		bcc ClsNextLine
		inc z_bs
ClsNextLine:
		lda z_L
		clc
		adc #$92
		sta z_L
		lda z_H
		adc #$1
		sta z_H
ClsNextLineB:
	endif	
	ifdef BuildAP2
		lda z_l
		clc
		adc #$80-32
		sta z_l
		bcc ClsNextLine
		inc z_h
		lda z_h
		and #%00000011
		bne ClsNextLine
		lda z_h
		sec
		sbc #%00000100
		sta z_h
		lda z_l
		clc
		adc #$28
		sta z_l
ClsNextLine:
	endif
	ifdef BuildPET
		lda z_l
		clc
		adc #8
		sta z_l
		bcc ClsNextLine
		inc z_h
ClsNextLine:
	endif	
	ifdef BuildC64
		lda z_l
		clc
		adc #$8*8
		sta z_l
		bcc ClsNextLine
		inc z_h
ClsNextLine:
	endif	
	ifdef BuildA52
		lda z_l
		clc
		adc #$18+8
		sta z_l
		lda z_h
		adc #$01
		sta z_h
	endif	
	ifdef BuildA80
		lda z_l
		clc
		adc #$18+8
		sta z_l
		lda z_h
		adc #$01
		sta z_h
	endif	
	
	dec z_ixl			;Repeat for next vertical line
	bne nextlinef
	rts
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;X,Y pos = BC / Width+Height = HL   IY=Source
;A= Sprite Flags


docrop_alloffscreen:
	sec		;set carry = nothing to draw
	rts
	
docrop:
	 and #%00000001
	 beq DoCropNoRev

; The Source recalc routine won't work right for flipped sprites
; so we flip the Xpos before running the crop routine	
	
	lda z_h
	clc
	adc z_b
	jsr neg				;Flip Xpos
	sta z_b

	jsr DoCropNoRev
	bcc DoCropNoRevNC
		rts
DoCropNoRevNC:	
	lda #VscreenWid+1	;Correct Xpos
	
	sec
	sbc z_h				;Flip to opposite side of screen.
	sbc z_b
	sta z_b
	clc					;Clear Carry
	rts
	
	
DoCropNoRev:
	ldy #0
	;sty spritehclip
	
	sty z_d
	sty z_e				;Temp Vars (E=Top Crop D=Bottom Crop)

;Crop Top side
	lda z_c
	sec
	sbc #vscreenminy
	bcs NoTCrop			;>0 = nothing needs cropping
	
	jsr neg
	clc
	adc #2				;Round Up
	
	cmp z_l			
	bcs docrop_alloffscreen	;all offscreen
	sta z_e				;Top crop
	
	and #%00000011
	eor #%00000011		;Shift amount
NoTCrop:
	sta z_c				;draw ypos

;Crop Bottom side	
	clc
	adc z_l				;add height
	sec
	sbc #vscreenhei-vscreenheiclip	;logical height of screen
	bcc NoBCrop			;>0 =nothing needs cropping
	
	and #%11111100		;Convert to tile count
	cmp z_l				;no pixels onscreen?
	bcs docrop_alloffscreen	;all offscreen
	sta z_d				;bottom crop
NoBCrop:


;Calculate New Height
	lda z_e				;units to remove from top
	and #%11111100
	clc					;units to remove from bottom
	adc z_d
	beq novclip			;nothing to remove?
	jsr neg

	clc
	adc z_l				;subtract from old height
	sta z_l				;new height

	lda z_e				;lines to remove from top

	
;Remove lines from top
	lda z_e				;lines to remove from top
	lsr
	lsr
	beq novclip			;any lines to remove from the top?
	tax
	
	lda z_h				;calc Tiles per line
	lsr
	lsr
	sta z_e
		
	lda z_iyl			;Old Start Tile pos
	clc
movedownaline:
	adc z_e				;Add E to IY
	bcc movedownalineB
	inc z_iyh
	clc
movedownalineB:		
	dex
	bne movedownaline
	sta z_iyl

	
novclip:
	;ldy #0		;Y=0 throughout this routine
	sty z_d
	sty z_e				;Temp Vars (E=Right Crop D=Left Crop)

	
;Crop Left hand side
	lda z_b
	sec
	sbc #vscreenminx 	;64 = Leftmost visible tile
	bcs nolcrop			;>0=nothing needs cropping
	
	jsr neg
	clc
	adc #2
	cmp z_h				;no pixels onscreen?
		
	bcc cropc
	jmp docrop_alloffscreen	;all offscreen
cropc
	sta z_e				;left crop

	and #%00000011
	eor #%00000011		;Shift amount
nolcrop:
	sta z_b				;draw xpos

;Crop Right hand side
	
	clc
	adc z_h				;add width

	sec
	sbc #vscreenwid-vscreenwidclip	;logical width of screen

	bcc norcrop			;c=nothing needs cropping
	cmp z_h				;no pixels onscreen?
	bcc  cropd
		jmp docrop_alloffscreen	;all offscreen
cropd:
	sta z_d				;Right crop
	
NoRcrop:


;Calculate new width
	lda z_d				;units to remove from left
	clc
	adc z_e				;units to remove from right
	lsr 
	lsr
	beq nohclip			;nothing to crop?

	sta spritehclip		;number of horizontal bytes to skip
						;after each line
	asl
	asl
	jsr neg
	clc
	adc z_h
	bne cropb
		jmp docrop_alloffscreen	;nothing to draw?
cropb:
	sta z_h				;Updated Width
	
	
;Caclulate start Tile
	lda z_e				;amount to subtract from left
	lsr 				;Convert to tile count (/4)
	lsr
	
	clc
	adc z_iyl
	sta z_iyl			;move across Tilemap
	bcc nohclip
	inc z_iyh
	
NoHClip:
	clc ;clear carry = crop ok
	rts

		
neg:				;Negate a 
	eor #255
	clc
	adc #1
	rts

ex_de_hl:
	lda z_d
	pha
		lda z_e
		pha
			lda z_l
			sta z_e
			lda z_h
			sta z_d
		pla
		sta z_l
	pla
	sta z_h
	rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;redraw the background under a sprite

RemoveSprite:	;BC= X,Y pos	;A=Sprite flags

	ldy #spr_refreshtile
	lda (z_ix),y
	bne RemoveSprite_NeedsDoing
	rts						;Nothing changed
RemoveSprite_NeedsDoing

	ldy #spr_oldxpos
	lda (z_ix),y
	sta z_b
	
	ldy #spr_oldypos
	lda (z_ix),y
	sta z_c

	ldy #spr_width
	lda (z_ix),y
	ifdef tilesmoothxmove	;Do we allow 2/4 pixel moves?
		clc
		adc #4				;add one tile
	endif
	sta z_d

	ldy #spr_height
	lda (z_ix),y
	ifdef tilesmoothymove	;Do we allow 2/4 pixel moves?
		clc
		adc #4				;add one tile	
	endif
	sta z_e

	jsr ex_de_hl			;Swap DE and HL
	

	jsr docropnorev		;Crop the sprite BC=XY pos 
	bcc lbl18471		;HL=WidthHeigh, IY=source data
	
	rts					;Nothing to draw
lbl18471

	jsr docroplogicaltotile	;Convert to tile co-ords
	bne lbl3073				;Nothing to draw
	rts
lbl3073


	lda z_h
	sta z_ixh			;Width/Height
	lda z_l
	sta z_ixl
	
	lda #$20			;Tilemap Width
	sta z_iyh	
	loadpair z_de,tilecache
	jsr shifttilemap	;Uses IYH only

	pushpair z_hl
		lda #$24		;Tilemap Width
		sta z_iyh
		loadpairsafe z_de,tilemap2
		lda offset		;X Scroll position
		and #%00000011	;4 tile 'scroll area'
		sta z_l
		lda #0
		sta z_h
		jsr addhl_de	;Shift source Tilemap by offset
		jsr ex_de_hl

		jsr shifttilemap ;Alter tilemap pos for current sprite position
	pullpair z_de
	
	
	lda #$20			;Tilemap Width (32 tiles in cache)
	sta z_iyl
updatetilecache:		;HL=Source tilemap   DE=Tile cache dest
	lda #0				;IXL=Spr Height  IXH=Spr Width
	sta z_b		
updatetilecacheb:	
	pushpair z_hl
		pushpair z_de
			
			lda z_ixh	;Width of sprite
			sta z_c
		
			jsr ldir	;copy bytes of tilemap to cache
updatetilecachea:
		pullpair z_hl
		lda z_iyl
		sta z_c
		jsr AddHL_BC
		jsr ex_de_hl
	pullpair z_hl

	lda z_iyh	
	sta z_c				;Width of tilemap
	jsr AddHL_BC

	dec z_ixl
	bne updatetilecacheb
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;			DE=Tilemap	BC=XY pos  IYH=Width
;Returns: 	HL=New Tilemap Pos

shifttilemap:
	jsr ex_de_hl
	
	ldy z_c			;y
	beq noadd
readd:
	lda z_iyh		;tilemap width
	jsr Add_HL_A	;move down c lines 
	dey
	bne readd
noadd:
	lda z_b			;Move across B tiles
Add_HL_A:
	clc
	adc z_l
	sta z_l
	bcc Add_HL_A_Done
	inc z_h
Add_HL_A_Done:	
	rts				;HL=New Tilemap Pos
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; convert logical pos to tilemap tile pos 
; z=true if size is zero

docroplogicaltotile: ;z= invalid width/height
	lsr z_b
	lsr z_b				;Xpos /4
	
	lsr z_c	
	lsr z_c				;Ypos /4
		
	lsr z_l
	lsr z_l				;Height /4
	beq docroplogicaltotile_Zero	;Is height zero?
	
	lsr z_h
	lsr z_h				;Width /4
docroplogicaltotile_Zero
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;compare tilemap bc to hl and copy tiles to cache de if different
;bc and hl must be &24 tiles wilde... de must be &20

changescroll:
	pushpair z_ix
		ldx #0
changescrollagain:
		lda (z_bc,x)		;Get source Tile
		cmp (z_hl,x)		;Is Dest Correct?
		beq changescrollok
		sta (z_de,x)		;No! Request an update
changescrollok:
		jsr inchl
		jsr incbc
		jsr incde
		dec z_ixh
		bne changescrollagain
	pullpair z_ix

	lda #4			;Skip 4 unused tiles in tilemap
	clc
	adc z_l
	sta z_l
	bcc changescroll_Hok
	inc z_h
changescroll_Hok:	
	
	lda #4			;Skip 4 unused tiles in tilemap
	clc
	adc z_c
	sta z_c
	bcc changescroll_Bok
	inc z_b
changescroll_Bok:	
		
	dec z_ixl
	bne changescroll
	rts
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;remove a sprite from the cache (stops sprite flickering)

ZeroSpriteInCache:			;BC= X,Y pos   IX=Sprite Source

	ldy #spr_refreshtile	;Check if we need to zero the sprite
	lda (z_ix),y
	bne lbl58740
		rts					;No? Return!
lbl58740

	lda #0
	sta (z_ix),y			;Clear refresh flag

	
	
	ldy #spr_xpos
	lda (z_ix),y
	sta z_b

	ldy #spr_ypos
	lda (z_ix),y
	sta z_c

	ldy #spr_width
	lda (z_ix),y
	sta z_h

	ldy #spr_height
	lda (z_ix),y
	sta z_l
		
	ifdef tilesmoothxmove
		lda z_b
		and #%00000010		;Shift Xpos one half tile
		beq nohalf			;& reduce width 1/2 tile
		lda z_b
		clc
		adc #2
		and #%11111100
		bne lbl54160
			rts
lbl54160
		sta z_b				;Alter Xpos
		dec z_h				;Alter Width
		dec z_h
nohalf:
	endif
	ifdef tilesmoothymove
		lda z_c
		and #%00000010		;Shift Xpos one half tile
		beq nohalf2			;& reduce width 1/2 tile
		lda z_c
		clc
		adc #2
		and #%11111100
		bne lbl207
			rts
lbl207
		sta z_c				;Alter Ypos
		dec z_l				;Alter Height
		dec z_l
nohalf2:
	endif

	
	
	ldy #spr_tilel
	lda (z_ix),y
	sta z_iyl				;Tilemap L
	
	ldy #spr_tileh
	lda (z_ix),y			;Tilemap H
	sta z_iyh
	
	ldy #spr_flags
	lda (z_ix),y

	jsr docrop		;Crop the sprite BC=XY pos HL=WidthHeight
					;IY=source data
	bcc lbl2179
		rts			;nothing to draw
lbl2179:

	ldy #spr_flags
	lda (z_ix),y
	and #%00000001			;Xlip?
	pha
	
		beq docroplogicaltotileb
		inc z_b				;Shift Xpos if X-flipped
		inc z_b
docroplogicaltotileb:
		jsr docroplogicaltotile
		bne lbl10388
		
		pla
	rts					;Return if Width/Height=0
lbl10388:


		ldy #spr_width
		lda (z_ix),y		;Width
		pha
			pushpair z_iy		;Tilemap HL
				lda z_h
				sta z_ixh
				lda z_l
				sta z_ixl		;Width/Height of draw
			
				lda #$20
				sta z_iyh		;Cache Tilemap Width
			
				loadpair z_de,tilecache
				jsr shifttilemap ;Shift the tilecache
								 ; to match sprite
				jsr ex_de_hl
			pullpair z_hl
		pla
		lsr
		lsr
		sta z_iyh			;Sprite Tilemap Width

		lda #$20			;tilemap width
		sta z_iyl
	pla
	
	
	beq zerotilecache		;Not Xflipped	
zerotilecacherev:
	lda z_ixh				;Width
	sec
	sbc #1
	clc
	adc z_l					;Shift Tilemap by width
	sta z_l
	bcc zerotilecache2
	inc z_h
zerotilecache2:

	ifdef ZeroTileCacheRevM		;Alter move mode
		lda #1
		sta ZeroTileCacheRevM			;Romver
	else
		lda #<dechl
		sta zerotilecacherev_plus2-2	;RamVer
		lda #>dechl
		sta zerotilecacherev_plus2-1
	endif
	
	
	jsr zerotilecache
	ifdef ZeroTileCacheRevM
		lda #0							;Romver
		sta ZeroTileCacheRevM
	else
		lda #<inchl
		sta zerotilecacherev_plus2-2	;Ramver
		lda #>inchl
		sta zerotilecacherev_plus2-1
	endif
	rts 
	
	
zerotilecache:
	ldx #0		;chech sprite tile
	pushpair z_hl
		pushpair z_de
			ldy z_ixh				;Width of sprite
			
zerocachec:
			lda (z_hl,x)			;Chech sprite tile
			beq zerocacheb	
			txa
			sta (z_de,x)			;Zero Cache (unused tile)
zerocacheb:
			jsr incde				;Inc Cache address
			ifdef ZeroTileCacheRevM
				lda ZeroTileCacheRevM
				bne zerocacheMoveDec
				jsr inchl			;0=INC
				jmp zerocacheMoveDone
zerocacheMoveDec:	
				jsr dechl			;1=DEC
zerocacheMoveDone:	
			else
				jsr inchl
zerotilecacherev_plus2:
			endif
			dey
			bne zerocachec
		pullpair z_de
	
		lda z_iyl		;&20
		clc
		adc z_e			;Move Cache down a line (B=0)
		sta z_e
		bcc zerocached
		inc z_d
zerocached:
	pullpair z_hl

	lda z_iyh			;Sprite Tilemap width
	clc
	adc z_l				;Move Sprite down a line (B=0)
	sta z_l					
	bcc zerocachee
	inc z_h
zerocachee:
	
	dec z_ixl
	bne zerotilecache
	rts

