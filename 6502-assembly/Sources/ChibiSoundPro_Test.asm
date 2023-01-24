;Options For SMS/BBC
;SmsSimpleNoise equ 1	;This limits noise frequency to 0-2
						;Otherwise we use channel 2 for frequency
  ;SmsTranspose equ 1		;Pitchshift





	ifdef BuildBBC 
chibisoundram equ $3800
	endif
	ifdef BuildAP2
chibisoundram equ $6000		;Game Vars	
	endif
	
	ifdef BuildNES
chibisoundram equ $300
	endif
	
	ifdef BuildPCE
chibisoundram equ $2300	
	endif
	ifdef BuildSNS
chibisoundram equ $1000	
	endif
UseDualJoy equ 1		;Enable 2nd joystick - to save memory on some systems we can turn it off.

	include "\SrcALL\V1_Header.asm"
	include "\SrcAll\BasicMacros.asm"
	
;FourColor equ 1	


	SEI			;Stop interrupts
	jsr ScreenInit
	jsr Cls
	
	jsr ChibiSoundPro_Init			;Init sound if needed
	
	lda #$00
	sta z_l
	sta z_h
	jsr ChibiSoundPro_Set

	loadpair z_de,$8000
	
	
	lda #$0
	sta z_l
JoytestLoop:

	pushpair z_bc
		ldx #0
		ldy #0
		jsr Locate
		pushpair z_de
			lda z_l
			pha
			jsr Player_ReadControlsDual			;read key and joy controls
			pla 
			sta z_l
		pullpair z_de
		lda z_h
		and #%00000001
		bne JoyNotUp
		ldx #16
IncLoop:
		jsr IncDE
		dex
		bne IncLoop
JoyNotUp:		
		lda z_h
		and #%00000010
		bne JoyNotDown
		ldx #16
DecLoop:
		jsr DecDE
		dex
		bne DecLoop
JoyNotDown:		
		lda z_h
		and #%00000100
		bne JoyNotLeft
		inc z_d
JoyNotLeft:		
		lda z_h
		and #%00001000
		bne JoyNotRight
		dec z_d
JoyNotRight:		
		lda #0
		sta z_l

		lda z_h
		and #%00010000
		bne JoyNotFire
		
		lda #128
		sta z_l
JoyNotFire:		
		
		
		jsr Monitor_DE
		
		
		lda #%00010000
		sta z_h
		
		pushpair z_hl
		pushpair z_de
			jsr ChibiSoundPro_Set		;Set Chibisound tone
			jsr ChibiSoundPro_Update	;Keep sound playing on beeper systems
		pullpair z_de
		pullpair z_hl
		
		loadpair z_bc,1
delay:		
		jsr decbc
		lda z_b
		ora z_c
		bne delay
		
	pullpair z_bc 

	jmp JoytestLoop	
	
	
	ldx #6
	ldy #3*8
	
	ifdef BuildAP2
bmpwidth equ 8
	else
	ifdef BuildLNX
bmpwidth equ 24
	else
	ifdef BuildC64
bmpwidth equ 3
	else
bmpwidth equ 6	
	endif

	endif
	endif
	
	
	
Monitor_DE:
	PushAll
	pushpair z_hl
	pushpair z_de
	pushpair z_bc
		lda z_e
		pha
			lda z_d
			jsr printhex
		pla
		jsr printhex
	pullpair z_bc
	pullpair z_de
	pullpair z_hl
	Pullall
	rts	
	

	include "\SrcALL\Multiplatform_ChibiSoundPro.asm"
	
	;include "\SrcBBC\BBC_V1_ChibiSoundPro.asm"
	;include "\SrcNES\NES_V1_ChibiSoundPro.asm"
	;include "\SrcPCE\PCE_V1_ChibiSoundPro.asm"
	;include "\SrcC64\C64_V1_ChibiSoundPro.asm"
	;include "\SrcVIC\VIC_V1_ChibiSoundPro.asm"
	;include "\SrcAP2\AP2_V1_ChibiSoundPro.asm"
	;include "\SrcA52\A52_V1_ChibiSoundPro.asm"
	;include "\SrcLNX\LNX_V1_ChibiSoundPro.asm"
	;include "\SrcSNS\SNS_V1_ChibiSoundPro.asm"
	;include "\SrcPET\PET_V1_ChibiSoundPro.asm"
	
	
	include "\SrcAll\monitor.asm"
	include "\SrcAll\BasicFunctions.asm"
	;include "\SrcC64\C64_V1_KeyboardDriver.asm"
	;include "\SrcBBC\BBC_V1_KeyboardDriver.asm"
	
	;include "\SrcA52\A52_V1_ChibiSound.asm"
	;include "\SrcAP2\AP2_V1_ChibiSound.asm"
	;include "\SrcPCE\PCE_V1_ChibiSound.asm"
	;include "\SrcNES\NES_V1_ChibiSound.asm"
	;include "\SrcC64\C64_V1_ChibiSound.asm"
	;include "\SrcBBC\BBC_V1_ChibiSound.asm"
	;include "\SrcLNX\LNX_V1_ChibiSound.asm"
;	include "\SrcVIC\VIC_V1_ChibiSound.asm"
	;include "\SrcSNS\SNS_V1_ChibiSound.asm"

Bitmapfont:
	ifndef BuildVIC
		incbin "\ResALL\Font96.FNT"		;Not used by the VIC due to memory limitations
	endif
	
	



Palette:
	;   -grb
	dw $0000	;0 - Background;
	dw $0099	;1
	dw $0E0F	;2
	dw $0FFF	;3 - Last color in 4 color modes
	dw $000F	;4;
	dw $004F	;5
	dw $008F	;6
	dw $00AF	;7
	dw $00FF	;8
	dw $04FF	;9
	dw $08FF	;10
	dw $0AFF	;11
	dw $0CCC	;12
	dw $0AAA	;13
	dw $0888	;14
	dw $0444	;15
	
	
	ifdef BuildNES	;Nes sprite colors
		dw $0000	;0 - Background;
		dw $0099	;1
		dw $0E0F	;2
		dw $0FF0	;3 - Last color in 4 color modes
		dw $000F	;4;
		dw $004F	;5
		dw $008F	;6
		dw $00AF	;7
		dw $00FF	;8
		dw $04FF	;9
		dw $08FF	;10
		dw $0AFF	;11
		dw $0CCC	;12
		dw $0AAA	;13
		dw $0888	;14
		dw $0444	;15
		dw $0FFF	;Border
	endif

	ALIGN 8

KeyboardScanner_KeyPresses
        db 16


		include "\SrcALL\V1_ReadJoystick.asm"
		
		include "..\SrcALL\V1_Functions.asm"
		include "\SrcALL\V1_Palette.asm"
		;include "\SrcAll\V1_SimpleTile.asm"
	
	include "\SrcAll\V1_BitmapMemory.asm"

	include "\SrcAll\V1_VdpMemory.asm"
		
		;
		ifdef BuildVIC
		ifndef BuildVIC_Rom
			org $1C00
			db 0,0,0,0,0,0,0,0	;Set Char 0 to blank
			incbin "\ResAll\Sprites\RawVIC.raw"
		endif
		endif
		
		include "\SrcALL\V1_Footer.asm"
		
		
		
 

 