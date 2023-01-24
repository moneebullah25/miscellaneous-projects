	macro AddHL_HL				;Add DE to HL
		asl z_l
		rol z_h
	endm 
	macro rlca
		clc
		adc #$80
		rol
	endm
	
	
	macro rrca
		pha
			ror
		pla
		ror
	endm
	macro PushAll
		pha
			txa
			pha
				tya 
				pha
	endm
	macro PullAll
				pla 
				tay
			pla
			tax
		pla
	endm
	macro PushXY
		txa
		pha
			tya 
			pha
	endm
	macro PullXY
			pla 
			tay
		pla
		tax
	endm
	macro PushX
		txa
		pha
	endm
	macro PullX
		pla
		tax
	endm
	macro PushY
		tya
		pha
	endm
	macro PullY
		pla
		tay
	endm
	macro PushPair,ra	;Push a pair onto the stack (eg PushPair z_HL)
		lda \ra			
		pha				;Push lower Zpage entry
		lda \ra+1
		pha
	endm				;Push higher Zpage entry
	
	macro PullPair,ra	;Pull a pair onto the stack (eg PullPair z_HL)
		pla
		sta \ra+1		;Pull lower Zpage entry
		pla
		sta \ra			;Pull higher Zpage entry
	endm
	macro PushPairSafe,ra	;Push a pair onto the stack (eg PushPair z_HL)
		sta z_spec
		lda \ra			
		pha				;Push lower Zpage entry
		lda \ra+1
		pha
		lda z_spec
	endm				;Push higher Zpage entry
	
	macro PullPairSafe,ra	;Pull a pair onto the stack (eg PullPair z_HL)
		sta z_spec
		pla
		sta \ra+1		;Pull lower Zpage entry
		pla
		sta \ra			;Pull higher Zpage entry
		lda z_spec
	endm
	macro LoadPair,zr,val
		lda #<\val
		sta \zr
		lda #>\val
		sta \zr+1
	endm
	macro LoadPairSafe,zr,val
		pha
			lda #<\val
			sta \zr
			lda #>\val
			sta \zr+1
		pla
	endm
	macro LoadXY,val
		ldy #<\val
		ldx #>\val
	endm

	macro LoadTripleAYX,val
		lda #(\val>>16 & $FF)
		ldy #(\val>>8 & $FF)
		ldx #(\val & $FF)
	endm
	macro LoadPairFromsafe,zr,addr
		pha
			lda \addr
			sta \zr
			lda \addr+1
			sta \zr+1
		pla
	endm
	macro LoadPairFrom,zr,addr
		lda \addr
		sta \zr
		lda \addr+1
		sta \zr+1
	endm
		macro SavePairTo,zr,addr
		lda \zr
		sta \addr
		lda \zr+1
		sta \addr+1
	endm
	macro LoadOne,zr,val
		lda \val
		sta \zr
	endm
	macro AddPair,zr,val
		clc
		lda \zr
		adc #<\val
		sta \zr
		lda #>\val
		adc \zr+1
		sta \zr+1
	endm
	macro add,val
		clc
		adc \val
	endm
	macro sub,val
		sec
		sbc \val
	endm